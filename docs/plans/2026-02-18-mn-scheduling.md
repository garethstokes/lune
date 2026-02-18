# M:N Scheduling Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Run multiple scheduler loops on OS threads, enabling true parallel execution of Lune fibers on multi-core systems.

**Architecture:** M:N scheduling multiplexes M Lune fibers onto N OS threads. Each OS thread runs an independent scheduler loop. A shared work-stealing queue distributes fibers fairly across threads. Thread-local state remains separate while global fiber registry uses STM for coordination.

**Tech Stack:** GHC runtime (`forkIO`), STM (`TVar`/`TMVar`), existing CPS evaluator with fuel.

---

## Background

The current scheduler (`Scheduler.hs`) is single-threaded:
```haskell
data SchedulerState = SchedulerState
  { schedFibers :: IORef (IntMap FiberEntry)      -- NOT thread-safe
  , schedReadyQueue :: IORef (Seq FiberId)        -- NOT thread-safe
  , schedSleeping :: IORef [(Int, FiberId, ...)]  -- NOT thread-safe
  , ...
  }
```

We need to:
1. Make shared state thread-safe with STM
2. Spawn N worker threads, each running a scheduler loop
3. Implement work-stealing for load balancing
4. Handle thread-local resources (sockets, TLS connections)

The fuel-based CPS evaluator already enables preemption - fibers yield after ~100 steps. This gives us natural scheduling points.

---

## Task 1: Add Thread-Safe Scheduler Types

**Files:**
- Modify: `src/Lune/Eval/Scheduler.hs`

**Step 1: Add STM imports**

At the top of the file, add:

```haskell
import Control.Concurrent.STM
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Monad (replicateM_, forever)
```

**Step 2: Create ParSchedulerState type**

Add after the existing `SchedulerState` type:

```haskell
-- | Thread-safe scheduler state for M:N scheduling
data ParSchedulerState = ParSchedulerState
  { parFibers :: TVar (IntMap FiberEntry)
    -- ^ All fiber entries by ID (thread-safe)
  , parReadyQueue :: TVar (Seq FiberId)
    -- ^ Global ready queue (work-stealing source)
  , parSleeping :: TVar [(Int, FiberId, World -> IO (Either EvalError IOStep))]
    -- ^ Sleeping fibers (one thread manages this)
  , parNextId :: TVar FiberId
    -- ^ Next fiber ID to allocate
  , parResults :: TVar (IntMap Value)
    -- ^ Completed fiber results
  , parAwaitConts :: TVar (IntMap (World -> Value -> IO (Either EvalError IOStep)))
    -- ^ Stored await continuations
  , parShutdown :: TVar Bool
    -- ^ Signal to stop worker threads
  , parMainDone :: TMVar (Either EvalError (World, Value))
    -- ^ Result of main fiber (fiber 0)
  }
```

**Step 3: Add newParScheduler**

```haskell
-- | Create a new parallel scheduler
newParScheduler :: IO ParSchedulerState
newParScheduler = atomically $ do
  fibers <- newTVar IntMap.empty
  ready <- newTVar Seq.empty
  sleeping <- newTVar []
  nextId <- newTVar 0
  results <- newTVar IntMap.empty
  awaitConts <- newTVar IntMap.empty
  shutdown <- newTVar False
  mainDone <- newEmptyTMVar
  pure ParSchedulerState
    { parFibers = fibers
    , parReadyQueue = ready
    , parSleeping = sleeping
    , parNextId = nextId
    , parResults = results
    , parAwaitConts = awaitConts
    , parShutdown = shutdown
    , parMainDone = mainDone
    }
```

**Step 4: Add spawnFiberPar**

```haskell
-- | Spawn a fiber in the parallel scheduler
spawnFiberPar :: ParSchedulerState -> (World -> IO (Either EvalError IOStep)) -> IO FiberId
spawnFiberPar sched cont = atomically $ do
  fid <- readTVar (parNextId sched)
  writeTVar (parNextId sched) (fid + 1)
  let entry = FiberEntry cont []
  modifyTVar' (parFibers sched) (IntMap.insert fid entry)
  modifyTVar' (parReadyQueue sched) (Seq.|> fid)
  pure fid
```

**Step 5: Build to verify**

Run: `cabal build`
Expected: Compiles with unused warnings

---

## Task 2: Add Worker Thread Logic

**Files:**
- Modify: `src/Lune/Eval/Scheduler.hs`

**Step 1: Add tryDequeue helper**

```haskell
-- | Try to dequeue a ready fiber (STM transaction)
tryDequeue :: ParSchedulerState -> STM (Maybe FiberId)
tryDequeue sched = do
  q <- readTVar (parReadyQueue sched)
  case Seq.viewl q of
    Seq.EmptyL -> pure Nothing
    fid Seq.:< rest -> do
      writeTVar (parReadyQueue sched) rest
      pure (Just fid)
```

**Step 2: Add worker thread function**

```haskell
-- | Worker thread - runs fibers from the ready queue
workerThread :: ParSchedulerState -> IO ()
workerThread sched = loop
  where
    loop = do
      -- Check for shutdown
      shouldStop <- atomically $ readTVar (parShutdown sched)
      if shouldStop
        then pure ()
        else do
          -- Try to get a fiber
          mFid <- atomically $ tryDequeue sched
          case mFid of
            Nothing -> do
              -- No work - wait briefly then retry
              threadDelay 1000  -- 1ms
              loop
            Just fid -> do
              -- Get fiber entry
              mEntry <- atomically $ do
                fibers <- readTVar (parFibers sched)
                pure (IntMap.lookup fid fibers)
              case mEntry of
                Nothing -> loop  -- Fiber was removed
                Just entry -> do
                  -- Create fresh World for this fiber step
                  -- Note: Each fiber step gets a fresh World
                  result <- fiberCont entry emptyWorldForFiber
                  case result of
                    Left err -> do
                      -- Error in fiber 0 terminates everything
                      when (fid == 0) $
                        atomically $ tryPutTMVar (parMainDone sched) (Left err)
                      loop
                    Right step -> do
                      handleStepPar sched fid step
                      loop
```

Note: `emptyWorldForFiber` is a placeholder - we'll address World management in Task 4.

**Step 3: Build to see what's missing**

Run: `cabal build`
Expected: Errors about `handleStepPar` and `emptyWorldForFiber`

---

## Task 3: Add Parallel handleStep

**Files:**
- Modify: `src/Lune/Eval/Scheduler.hs`

**Step 1: Add handleStepPar**

```haskell
-- | Handle a step result in parallel scheduler
handleStepPar :: ParSchedulerState -> FiberId -> IOStep -> IO ()
handleStepPar sched fid step =
  case step of
    StepDone world value -> do
      -- Fiber completed
      atomically $ do
        -- Store result
        modifyTVar' (parResults sched) (IntMap.insert fid value)
        -- Get waiters
        fibers <- readTVar (parFibers sched)
        case IntMap.lookup fid fibers of
          Nothing -> pure ()
          Just entry -> do
            -- Wake all waiters
            forM_ (fiberWaiters entry) $ \waiterId -> do
              awaitConts <- readTVar (parAwaitConts sched)
              case IntMap.lookup waiterId awaitConts of
                Just cont -> do
                  modifyTVar' (parFibers sched) $
                    IntMap.adjust (\e -> e { fiberCont = \w -> cont w value }) waiterId
                  modifyTVar' (parAwaitConts sched) (IntMap.delete waiterId)
                Nothing -> pure ()
              modifyTVar' (parReadyQueue sched) (Seq.|> waiterId)
        -- Remove completed fiber
        modifyTVar' (parFibers sched) (IntMap.delete fid)
        -- If main fiber, signal completion
        when (fid == 0) $
          tryPutTMVar (parMainDone sched) (Right (world, value))

    StepYield world cont -> do
      atomically $ do
        modifyTVar' (parFibers sched) $
          IntMap.adjust (\e -> e { fiberCont = cont }) fid
        modifyTVar' (parReadyQueue sched) (Seq.|> fid)

    StepSleep world ms cont -> do
      now <- nowMs
      let wakeAt = now + ms
      atomically $ do
        modifyTVar' (parSleeping sched) $
          insertSorted wakeAt fid cont
        modifyTVar' (parFibers sched) $
          IntMap.adjust (\e -> e { fiberCont = cont }) fid

    StepAwait world targetFid cont -> do
      atomically $ do
        results <- readTVar (parResults sched)
        case IntMap.lookup targetFid results of
          Just value -> do
            -- Already done
            modifyTVar' (parFibers sched) $
              IntMap.adjust (\e -> e { fiberCont = \w -> cont w value }) fid
            modifyTVar' (parReadyQueue sched) (Seq.|> fid)
          Nothing -> do
            -- Add to waiters
            modifyTVar' (parFibers sched) $
              IntMap.adjust (\e -> e { fiberWaiters = fid : fiberWaiters e }) targetFid
            modifyTVar' (parAwaitConts sched) (IntMap.insert fid cont)

    StepSpawn world newCont resumeCont -> do
      newFid <- spawnFiberPar sched newCont
      atomically $ do
        modifyTVar' (parFibers sched) $
          IntMap.adjust (\e -> e { fiberCont = \w -> resumeCont w newFid }) fid
        modifyTVar' (parReadyQueue sched) (Seq.|> fid)
```

**Step 2: Build to verify**

Run: `cabal build`
Expected: Compiles (may have warnings about unused `world`)

---

## Task 4: Add Sleep Manager Thread

**Files:**
- Modify: `src/Lune/Eval/Scheduler.hs`

**Step 1: Add sleep manager**

The sleep manager is a dedicated thread that wakes sleeping fibers:

```haskell
-- | Sleep manager thread - wakes sleeping fibers at their scheduled time
sleepManager :: ParSchedulerState -> IO ()
sleepManager sched = loop
  where
    loop = do
      shouldStop <- atomically $ readTVar (parShutdown sched)
      if shouldStop
        then pure ()
        else do
          wakeSleptPar sched
          -- Sleep until next scheduled wakeup or 10ms
          nextWake <- atomically $ do
            sleeping <- readTVar (parSleeping sched)
            case sleeping of
              [] -> pure Nothing
              ((wake, _, _) : _) -> pure (Just wake)
          now <- nowMs
          case nextWake of
            Nothing -> threadDelay 10000  -- 10ms if no sleepers
            Just wake -> do
              let delay = max 0 (wake - now)
              when (delay > 0) $
                threadDelay (delay * 1000)
          loop

-- | Wake sleeping fibers that are due (parallel version)
wakeSleptPar :: ParSchedulerState -> IO ()
wakeSleptPar sched = do
  now <- nowMs
  atomically $ do
    sleeping <- readTVar (parSleeping sched)
    let (due, remaining) = span (\(wake, _, _) -> wake <= now) sleeping
    writeTVar (parSleeping sched) remaining
    forM_ due $ \(_, fid, cont) -> do
      modifyTVar' (parFibers sched) $
        IntMap.adjust (\e -> e { fiberCont = cont }) fid
      modifyTVar' (parReadyQueue sched) (Seq.|> fid)
```

**Step 2: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 5: Add runParScheduler Entry Point

**Files:**
- Modify: `src/Lune/Eval/Scheduler.hs`

**Step 1: Add runParScheduler**

```haskell
-- | Run the parallel scheduler with N worker threads
-- N is determined by GHC runtime capabilities
runParScheduler :: ParSchedulerState -> World -> IO (Either EvalError (World, Value))
runParScheduler sched world0 = do
  -- Get number of capabilities (OS threads GHC can use)
  numCaps <- getNumCapabilities
  let numWorkers = max 1 (numCaps - 1)  -- Reserve one for sleep manager

  -- Store initial World in a TVar for workers to clone
  -- (Each fiber step gets a fresh copy)
  worldVar <- newTVarIO world0

  -- Start sleep manager
  _ <- forkIO $ sleepManager sched

  -- Start worker threads
  replicateM_ numWorkers $ forkIO $ workerThreadWithWorld sched worldVar

  -- Wait for main fiber to complete
  atomically $ do
    result <- takeTMVar (parMainDone sched)
    -- Signal shutdown
    writeTVar (parShutdown sched) True
    pure result

-- | Worker thread with access to World template
workerThreadWithWorld :: ParSchedulerState -> TVar World -> IO ()
workerThreadWithWorld sched worldVar = loop
  where
    loop = do
      shouldStop <- atomically $ readTVar (parShutdown sched)
      if shouldStop
        then pure ()
        else do
          mFid <- atomically $ tryDequeue sched
          case mFid of
            Nothing -> do
              threadDelay 1000
              loop
            Just fid -> do
              mEntry <- atomically $ IntMap.lookup fid <$> readTVar (parFibers sched)
              case mEntry of
                Nothing -> loop
                Just entry -> do
                  -- Get a copy of World for this fiber step
                  world <- readTVarIO worldVar
                  result <- fiberCont entry world
                  case result of
                    Left err -> do
                      when (fid == 0) $
                        atomically $ tryPutTMVar (parMainDone sched) (Left err)
                      loop
                    Right step -> do
                      handleStepPar sched fid step
                      loop
```

**Step 2: Export new functions**

Update module exports:

```haskell
module Lune.Eval.Scheduler
  ( SchedulerState (..)
  , ParSchedulerState (..)
  , FiberEntry (..)
  , newScheduler
  , newParScheduler
  , spawnFiber
  , spawnFiberPar
  , runScheduler
  , runParScheduler
  ) where
```

**Step 3: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 6: Update Runtime to Use Parallel Scheduler

**Files:**
- Modify: `src/Lune/Eval/Runtime.hs`

**Step 1: Add parallel scheduler imports**

```haskell
import qualified Lune.Eval.Scheduler as Sched
```

**Step 2: Add runIOParallel function**

Add after `runIO`:

```haskell
-- | Run IO action with parallel scheduler (M:N threading)
runIOParallel :: Value -> IO (Either EvalError (World, Value))
runIOParallel v = do
  case force v of
    Left err ->
      pure (Left err)
    Right v' ->
      case v' of
        VIO act -> do
          -- Create shared state for TVars
          tvarsVar <- newTVarIO IntMap.empty
          nextIdVar <- newTVarIO 0
          let shared = SharedState tvarsVar nextIdVar
          let world = World [] shared IntMap.empty 0 [] Nothing IntMap.empty 0 IntMap.empty 0 IntMap.empty 0 Nothing 0

          -- Create parallel scheduler and spawn main fiber
          sched <- Sched.newParScheduler
          _ <- Sched.spawnFiberPar sched act

          -- Run parallel scheduler
          Sched.runParScheduler sched world

        VCon "Lune.Prelude.Task" [io] ->
          runIOParallel io
        other ->
          pure (Left (NotAnIO other))
```

**Step 3: Export runIOParallel**

Add to exports: `runIOParallel`

**Step 4: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 7: Add CLI Flag for Parallel Mode

**Files:**
- Modify: `app/Main.hs`

**Step 1: Read Main.hs to understand flag parsing**

First, read the current Main.hs structure.

**Step 2: Add --parallel flag**

Look for the argument parsing and add a `--parallel` flag that uses `runIOParallel` instead of `runIO`.

The exact changes depend on the current structure. The pattern should be:

```haskell
-- In the arg parsing section, add:
"--parallel" -> -- enable parallel scheduler

-- In the eval section, use:
if parallel
  then runIOParallel mainVal
  else runIO mainVal
```

**Step 3: Build and test**

Run: `cabal build`
Expected: Compiles with new flag

Run: `cabal run lune -- --parallel --run examples/12_Fiber_Basic.lune`
Expected: Fibers execute (may interleave differently than single-threaded)

---

## Task 8: Handle World State Properly

**Problem:** The current design passes World by value, but in M:N we need to:
1. Share the `SharedState` (TVars) across all fibers
2. Keep thread-local state (stdout, sockets) per-fiber or synchronized

**Files:**
- Modify: `src/Lune/Eval/Types.hs`
- Modify: `src/Lune/Eval/Scheduler.hs`

**Step 1: Add WorldId and per-fiber World storage**

In `Types.hs`, the `World` type already has `worldShared` which uses STM TVars. This is already thread-safe.

The problem is `worldStdout` - multiple fibers printing concurrently could interleave badly.

**Step 2: Add synchronized stdout**

Option A: Each fiber buffers its own stdout, merged at completion
Option B: Use TVar for synchronized stdout

For simplicity, use Option B - add to `SharedState`:

```haskell
data SharedState = SharedState
  { sharedTVars :: TVar (IntMap Value)
  , sharedNextTVarId :: TVar TVarId
  , sharedStdout :: TVar [Text]          -- NEW: synchronized stdout
  }
```

**Step 3: Update IO primitives**

In `Builtins.hs`, update `prim_println` to use the shared stdout:

```haskell
prim_println :: String -> IO Unit
-- becomes:
\world -> do
  atomically $ modifyTVar' (sharedStdout (worldShared world)) (line :)
  pure $ Right $ StepDone world (VCon "Lune.Prelude.Unit" [])
```

**Step 4: Flush stdout at end**

In `runParScheduler`, after getting result:

```haskell
-- Flush buffered stdout
bufferedOut <- atomically $ readTVar (sharedStdout (worldShared world0))
mapM_ TIO.putStrLn (reverse bufferedOut)
```

**Step 5: Build to verify**

Run: `cabal build`
Expected: Compiles

---

## Task 9: Add Parallel Scheduling Test

**Files:**
- Create: `examples/43_Parallel_Fibers.lune`

**Step 1: Create test file**

```lune
module ParallelFibers exposing (main)

{-| Tests M:N parallel scheduling.
    Multiple CPU-bound fibers should run concurrently on multiple cores.
-}

import Lune.Prelude exposing (Task, Unit, unit, Int, Bool(..), Applicative(..), Monad(..))
import Lune.IO as IO
import Lune.Fiber as Fiber
import Lune.Int as Int
import Lune.String as String

main : Task Unit Unit
main =
  do
    _ <- IO.println "Spawning 4 CPU-bound fibers..."
    f1 <- Fiber.spawn (cpuWork "A" 20)
    f2 <- Fiber.spawn (cpuWork "B" 20)
    f3 <- Fiber.spawn (cpuWork "C" 20)
    f4 <- Fiber.spawn (cpuWork "D" 20)
    _ <- IO.println "Awaiting results..."
    _ <- Fiber.await f1
    _ <- Fiber.await f2
    _ <- Fiber.await f3
    _ <- Fiber.await f4
    IO.println "All done!"

cpuWork : String -> Int -> Task Unit Unit
cpuWork name n =
  do
    _ <- IO.println (String.append name ": starting")
    let result = fib n
    _ <- IO.println (String.append name (String.append ": fib result = " (String.fromInt result)))
    IO.println (String.append name ": done")

fib : Int -> Int
fib n =
  case Int.lte n 1 of
    True -> n
    False -> Int.add (fib (Int.sub n 1)) (fib (Int.sub n 2))
```

**Step 2: Run test**

Run: `cabal run lune -- --parallel --run examples/43_Parallel_Fibers.lune`

Expected: Output shows fibers starting/completing in parallel (may interleave).
With single-threaded scheduler, fibers run sequentially.

---

## Task 10: Add Work-Stealing for Load Balance

**Files:**
- Modify: `src/Lune/Eval/Scheduler.hs`

Currently all workers share one global queue. For better cache locality, each worker could have a local queue and steal from others when empty.

**Step 1: Add per-worker queues (optional enhancement)**

This is an optimization. The simple global queue works correctly, just with more contention.

For now, skip this and note it as a future enhancement.

**Step 2: Document the architecture**

Add a comment in Scheduler.hs:

```haskell
{-| M:N Scheduler Architecture

Current design:
- Global ready queue (TVar (Seq FiberId))
- N worker threads dequeue from global queue
- Sleep manager thread handles timed wakeups

Future enhancements:
- Per-worker local queues for cache locality
- Work-stealing: workers steal from others when local queue empty
- Fiber affinity: prefer running fiber on same thread as parent

-}
```

---

## Task 11: Handle Socket/TLS Thread Affinity

**Problem:** Network sockets and TLS connections may have thread affinity requirements.

**Files:**
- Modify: `src/Lune/Eval/Scheduler.hs`
- Modify: `src/Lune/Builtins.hs`

**Step 1: Document the issue**

For now, document that socket operations should complete atomically within a single fiber step. This is already the case since each socket operation returns `StepDone`.

Add comment:

```haskell
-- NOTE: Socket/TLS operations complete within a single step, so thread
-- affinity is not an issue. If we add streaming operations that span
-- multiple steps, we'd need to pin the fiber to its current thread.
```

**Step 2: No code changes needed**

The current design handles this correctly because:
- Socket accept returns `StepDone` with the connection
- Socket read/write return `StepDone` with the data
- The fiber can migrate between threads between steps, but each step is atomic

---

## Task 12: Run All Tests

**Step 1: Run golden tests**

Run: `cabal test`
Expected: All tests pass

**Step 2: Run examples with parallel scheduler**

```bash
for f in examples/*.lune; do
  echo "Testing: $f"
  cabal run lune -- --parallel --run "$f" 2>&1 | head -10
done
```

Expected: All examples work correctly

---

## Task 13: Commit

```bash
git add -A
git commit -m "$(cat <<'EOF'
feat(scheduler): implement M:N parallel scheduling

Add parallel scheduler that runs M Lune fibers on N OS threads:

- ParSchedulerState: Thread-safe scheduler using STM TVars
- Worker threads: Each runs scheduler loop, dequeues from global queue
- Sleep manager: Dedicated thread for timed wakeups
- runParScheduler: Entry point spawning N-1 workers
- runIOParallel: Runtime integration for parallel execution
- --parallel flag: CLI option to enable M:N scheduling

Architecture:
- Fibers migrate between OS threads between steps
- Global ready queue with STM for thread-safe dequeue
- SharedState (TVars) already thread-safe
- Stdout synchronized via TVar buffer

This enables true parallel execution of Lune fibers on multi-core CPUs.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Summary

| Component | Change |
|-----------|--------|
| `ParSchedulerState` | Thread-safe scheduler state using TVars |
| `workerThread` | Runs fibers from ready queue |
| `sleepManager` | Dedicated thread for timed wakeups |
| `handleStepPar` | STM-based step handling |
| `runParScheduler` | Spawns N workers, waits for main |
| `runIOParallel` | Runtime entry point |
| `--parallel` | CLI flag to enable M:N scheduling |

**Key Design Decisions:**

1. **Global Queue**: Simple shared queue vs per-worker + stealing. Start simple.

2. **World Handling**: Each fiber step gets a copy of World. SharedState (TVars) is already thread-safe. Stdout is buffered in a TVar.

3. **Thread Count**: Use `getNumCapabilities - 1` workers (leave one for sleep manager).

4. **Fiber Migration**: Fibers can run on any thread. No affinity requirements since each step is atomic.

5. **Error Handling**: Errors in main fiber (fid=0) signal completion. Other fiber errors are silently dropped (future: add error propagation).

---

## Future Enhancements

1. **Work-Stealing Queues**: Per-worker local queues with stealing for better cache locality

2. **Fiber Affinity**: Option to pin fibers to threads for socket/TLS operations

3. **Structured Concurrency**: Nurseries that cancel child fibers when parent fails

4. **Load Metrics**: Expose fiber counts and queue depths for monitoring

5. **Configurable Thread Count**: `--threads N` flag instead of auto-detection
