# Lune.Scheduler Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement a Prelude-level Scheduler module that provides cron-like job scheduling using Lune's existing concurrency primitives (fibers, STM/Atomic shared state).

**Architecture:** The scheduler is built entirely in Lune (no new Haskell primitives needed). It uses STM-based shared state for the job registry and queue, spawns timer fibers for scheduled jobs, and a runner fiber that processes the queue. The design separates scheduling rules from execution, following the same patterns as `Lune.Database.Pool`.

**Tech Stack:** Pure Lune using existing `Lune.Fiber`, `Lune.Atomic`, `Lune.Time`, `Lune.Task`

---

## Overview

The scheduler consists of:

1. **Schedule types** - `Manual`, `AfterMs`, `EveryMs` (defer `Cron` - requires more complex time arithmetic)
2. **Concurrency policy** - `OneInstance` (drop concurrent triggers) or `AllowConcurrent`
3. **JobSpec** - Job definition with id, schedule, concurrency, and action
4. **Scheduler state** - STM-managed registry, queue, and in-flight tracking
5. **Timer fibers** - Spawned for `AfterMs`/`EveryMs` jobs
6. **Runner fiber** - Pulls from queue and executes jobs

Key design decisions:
- `OneInstance` drops triggers while job is running (simplest semantics)
- `AfterMs` auto-unregisters after first run
- No wall-clock cron initially - `EveryMs` provides interval-based scheduling
- Uses `Task e Unit` for job actions to match existing Lune patterns

---

## Task 1: Create Lune.Scheduler module with types

**Files:**
- Create: `prelude/Lune/Scheduler.lune`

**Step 1: Create the directory if needed**

Run: `mkdir -p prelude/Lune`

Expected: Directory exists (may already exist)

**Step 2: Write the module with core types**

Create `prelude/Lune/Scheduler.lune`:

```lune
module Lune.Scheduler exposing (
  Scheduler,
  JobId(..),
  Schedule(..),
  Concurrency(..),
  JobSpec,
  SchedulerError(..),
  new,
  start,
  stop,
  register,
  unregister,
  trigger
)

{-| Job scheduling for Lune.

Provides cron-like scheduling using fibers and STM.
Jobs can run on intervals, after delays, or be triggered manually.

Example:
```
main =
  do
    sched <- Scheduler.new
    _ <- Scheduler.register sched
      { id = JobId "tick"
      , schedule = EveryMs 1000
      , concurrency = OneInstance
      , action = IO.println "tick"
      }
    _ <- Scheduler.start sched
    _ <- IO.sleepMs 5000
    Scheduler.stop sched
```
-}

import Lune.Prelude exposing (
  Bool(..), Int, List(..), Maybe(..), Result(..), Task(..), Unit
)

-- =============================================================================
-- Public Types
-- =============================================================================

-- | Unique identifier for a job
type JobId = JobId String

-- | When a job should run
type Schedule =
  Manual
  | AfterMs Int
  | EveryMs Int

-- | How concurrent triggers are handled
type Concurrency =
  OneInstance
  | AllowConcurrent

-- | Job specification
type alias JobSpec =
  { id : JobId
  , schedule : Schedule
  , concurrency : Concurrency
  , action : Task Unit Unit
  }

-- | Scheduler errors
type SchedulerError =
  JobAlreadyRegistered
  | JobNotFound
  | SchedulerNotRunning

-- | The scheduler handle (opaque to users)
type alias Scheduler =
  { state : SchedulerState
  }

-- =============================================================================
-- Internal Types
-- =============================================================================

-- Internal state held in STM
type alias SchedulerState =
  { running : Shared Bool
  , jobs : Shared (List JobEntry)
  , queue : Shared (List JobId)
  , inFlight : Shared (List JobId)
  }

-- Job entry with metadata
type alias JobEntry =
  { spec : JobSpec
  , timerFiber : Maybe (Fiber (Result Unit Unit))
  }
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: Parses successfully (shows AST or no errors)

**Step 4: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(prelude): add Lune.Scheduler module with core types"
```

---

## Task 2: Add imports and helper type annotations

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add required imports**

After the module doc comment, add:

```lune
import Lune.Atomic as Atomic exposing (Atomic, Shared)
import Lune.Fiber as Fiber exposing (Fiber)
import Lune.Task as Task
import Lune.IO as IO
import Lune.Int as Int
import Lune.String as String
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): add imports for Atomic, Fiber, Task, IO"
```

---

## Task 3: Implement `new` - create scheduler

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add the `new` function**

Add after the type definitions:

```lune
-- =============================================================================
-- Lifecycle
-- =============================================================================

-- | Create a new scheduler (not yet running)
new : Task e Scheduler
new =
  do
    runningVar <- Atomic.commit (Atomic.new False)
    jobsVar <- Atomic.commit (Atomic.new Nil)
    queueVar <- Atomic.commit (Atomic.new Nil)
    inFlightVar <- Atomic.commit (Atomic.new Nil)
    Task.succeed
      { state =
        { running = runningVar
        , jobs = jobsVar
        , queue = queueVar
        , inFlight = inFlightVar
        }
      }
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): implement new function"
```

---

## Task 4: Implement `start` and `stop`

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add `start` function**

```lune
-- | Start the scheduler (spawns the runner fiber)
start : Scheduler -> Task e Unit
start sched =
  do
    _ <- Atomic.commit (Atomic.write sched.state.running True)
    _ <- Fiber.spawn (runnerLoop sched)
    Task.succeed Unit
```

**Step 2: Add `stop` function**

```lune
-- | Stop the scheduler (runner will exit on next iteration)
stop : Scheduler -> Task e Unit
stop sched =
  Atomic.commit (Atomic.write sched.state.running False)
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: May fail (runnerLoop not yet defined) - that's expected

**Step 4: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): implement start and stop functions"
```

---

## Task 5: Implement job registry helpers

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add helper to find job by ID**

```lune
-- =============================================================================
-- Internal Helpers
-- =============================================================================

-- | Check if a JobId matches
jobIdEq : JobId -> JobId -> Bool
jobIdEq a b =
  case a of
    JobId sa ->
      case b of
        JobId sb -> String.eq sa sb

-- | Find a job entry by ID
findJob : JobId -> List JobEntry -> Maybe JobEntry
findJob targetId entries =
  case entries of
    Nil -> Nothing
    Cons entry rest ->
      case jobIdEq targetId entry.spec.id of
        True -> Just entry
        False -> findJob targetId rest

-- | Check if a JobId is in a list
memberJobId : JobId -> List JobId -> Bool
memberJobId targetId ids =
  case ids of
    Nil -> False
    Cons id rest ->
      case jobIdEq targetId id of
        True -> True
        False -> memberJobId targetId rest

-- | Remove a job entry by ID
removeJob : JobId -> List JobEntry -> List JobEntry
removeJob targetId entries =
  case entries of
    Nil -> Nil
    Cons entry rest ->
      case jobIdEq targetId entry.spec.id of
        True -> rest
        False -> Cons entry (removeJob targetId rest)

-- | Remove a JobId from a list
removeJobId : JobId -> List JobId -> List JobId
removeJobId targetId ids =
  case ids of
    Nil -> Nil
    Cons id rest ->
      case jobIdEq targetId id of
        True -> rest
        False -> Cons id (removeJobId targetId rest)
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): add job registry helper functions"
```

---

## Task 6: Implement `register`

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add `register` function**

```lune
-- =============================================================================
-- Job Management
-- =============================================================================

-- | Register a job with the scheduler
-- Returns Err if a job with the same ID already exists
register : Scheduler -> JobSpec -> Task SchedulerError Unit
register sched spec =
  do
    -- Check if job exists and add atomically
    result <- Atomic.commit
      (do
        jobs <- Atomic.read sched.state.jobs
        case findJob spec.id jobs of
          Just _ -> pure (Err JobAlreadyRegistered)
          Nothing ->
            do
              let entry = { spec = spec, timerFiber = Nothing }
              Atomic.write sched.state.jobs (Cons entry jobs)
              pure (Ok Unit)
      )
    case result of
      Err e -> Task.fail e
      Ok _ ->
        do
          -- Spawn timer fiber based on schedule
          _ <- armSchedule sched spec
          Task.succeed Unit
```

**Step 2: Add `armSchedule` helper**

```lune
-- | Spawn a timer fiber for scheduled jobs
armSchedule : Scheduler -> JobSpec -> Task e Unit
armSchedule sched spec =
  case spec.schedule of
    Manual ->
      Task.succeed Unit
    AfterMs delayMs ->
      do
        _ <- Fiber.spawn (afterMsTimer sched spec.id delayMs)
        Task.succeed Unit
    EveryMs intervalMs ->
      do
        _ <- Fiber.spawn (everyMsTimer sched spec.id intervalMs)
        Task.succeed Unit

-- | Timer for AfterMs: sleep then trigger once
afterMsTimer : Scheduler -> JobId -> Int -> Task Unit Unit
afterMsTimer sched jobId delayMs =
  do
    _ <- IO.sleepMs delayMs
    _ <- triggerInternal sched jobId
    -- Auto-unregister after running
    unregisterInternal sched jobId

-- | Timer for EveryMs: loop forever, sleep then trigger
everyMsTimer : Scheduler -> JobId -> Int -> Task Unit Unit
everyMsTimer sched jobId intervalMs =
  do
    isRunning <- Atomic.commit (Atomic.read sched.state.running)
    case isRunning of
      False -> Task.succeed Unit
      True ->
        do
          _ <- IO.sleepMs intervalMs
          _ <- triggerInternal sched jobId
          everyMsTimer sched jobId intervalMs
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: May fail (triggerInternal, unregisterInternal not yet defined)

**Step 4: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): implement register with timer spawning"
```

---

## Task 7: Implement `unregister`

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add `unregister` function**

```lune
-- | Unregister a job (removes it from the scheduler)
unregister : Scheduler -> JobId -> Task e Unit
unregister sched jobId =
  unregisterInternal sched jobId

-- | Internal unregister (doesn't fail if job not found)
unregisterInternal : Scheduler -> JobId -> Task e Unit
unregisterInternal sched jobId =
  do
    _ <- Atomic.commit
      (do
        jobs <- Atomic.read sched.state.jobs
        Atomic.write sched.state.jobs (removeJob jobId jobs)
      )
    -- Also remove from queue and inFlight
    _ <- Atomic.commit
      (do
        queue <- Atomic.read sched.state.queue
        Atomic.write sched.state.queue (removeJobId jobId queue)
      )
    _ <- Atomic.commit
      (do
        inFlight <- Atomic.read sched.state.inFlight
        Atomic.write sched.state.inFlight (removeJobId jobId inFlight)
      )
    Task.succeed Unit
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): implement unregister"
```

---

## Task 8: Implement `trigger`

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add `trigger` function**

```lune
-- | Manually trigger a job to run
-- Returns Err if job doesn't exist or scheduler not running
trigger : Scheduler -> JobId -> Task SchedulerError Unit
trigger sched jobId =
  do
    isRunning <- Atomic.commit (Atomic.read sched.state.running)
    case isRunning of
      False -> Task.fail SchedulerNotRunning
      True ->
        do
          result <- Atomic.commit
            (do
              jobs <- Atomic.read sched.state.jobs
              case findJob jobId jobs of
                Nothing -> pure (Err JobNotFound)
                Just _ ->
                  do
                    queue <- Atomic.read sched.state.queue
                    Atomic.write sched.state.queue (Cons jobId queue)
                    pure (Ok Unit)
            )
          case result of
            Err e -> Task.fail e
            Ok _ -> Task.succeed Unit

-- | Internal trigger (doesn't fail, used by timers)
triggerInternal : Scheduler -> JobId -> Task e Unit
triggerInternal sched jobId =
  do
    _ <- Atomic.commit
      (do
        queue <- Atomic.read sched.state.queue
        Atomic.write sched.state.queue (Cons jobId queue)
      )
    Task.succeed Unit
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): implement trigger"
```

---

## Task 9: Implement the runner loop

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add runner loop**

```lune
-- =============================================================================
-- Runner
-- =============================================================================

-- | Main runner loop - processes jobs from the queue
runnerLoop : Scheduler -> Task Unit Unit
runnerLoop sched =
  do
    isRunning <- Atomic.commit (Atomic.read sched.state.running)
    case isRunning of
      False -> Task.succeed Unit
      True ->
        do
          -- Try to get a job from the queue
          maybeJob <- Atomic.commit (tryDequeue sched)
          case maybeJob of
            Nothing ->
              do
                -- No jobs ready, yield and retry
                _ <- Fiber.yield
                runnerLoop sched
            Just entry ->
              do
                -- Execute the job
                _ <- executeJob sched entry
                runnerLoop sched

-- | Try to dequeue a job, respecting concurrency policy
tryDequeue : Scheduler -> Atomic (Maybe JobEntry)
tryDequeue sched =
  do
    queue <- Atomic.read sched.state.queue
    case queue of
      Nil -> pure Nothing
      Cons jobId rest ->
        do
          Atomic.write sched.state.queue rest
          jobs <- Atomic.read sched.state.jobs
          case findJob jobId jobs of
            Nothing ->
              -- Job was unregistered, skip it
              pure Nothing
            Just entry ->
              case entry.spec.concurrency of
                AllowConcurrent ->
                  pure (Just entry)
                OneInstance ->
                  do
                    inFlight <- Atomic.read sched.state.inFlight
                    case memberJobId jobId inFlight of
                      True ->
                        -- Already running, drop this trigger
                        pure Nothing
                      False ->
                        do
                          -- Mark as in-flight
                          Atomic.write sched.state.inFlight (Cons jobId inFlight)
                          pure (Just entry)
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: May fail (executeJob not yet defined)

**Step 3: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): implement runner loop with dequeue logic"
```

---

## Task 10: Implement job execution

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add executeJob function**

```lune
-- | Execute a job and clean up inFlight
executeJob : Scheduler -> JobEntry -> Task Unit Unit
executeJob sched entry =
  do
    -- Run the job action (ignore errors for now)
    _ <- Task.onError entry.spec.action (\_ -> Task.succeed Unit)
    -- Remove from inFlight (only matters for OneInstance)
    case entry.spec.concurrency of
      AllowConcurrent ->
        Task.succeed Unit
      OneInstance ->
        do
          _ <- Atomic.commit
            (do
              inFlight <- Atomic.read sched.state.inFlight
              Atomic.write sched.state.inFlight (removeJobId entry.spec.id inFlight)
            )
          Task.succeed Unit
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): implement job execution with inFlight cleanup"
```

---

## Task 11: Create scheduler example

**Files:**
- Create: `examples/42_Scheduler.lune`

**Step 1: Write the example**

```lune
module Scheduler exposing (main)

{-| Demonstrates the job scheduler with different schedule types. -}

import Lune.Prelude exposing (Task, Result(..), Unit, Int)
import Lune.IO as IO
import Lune.Scheduler as Scheduler exposing (
  Scheduler, JobId(..), Schedule(..), Concurrency(..), JobSpec, SchedulerError
)
import Lune.Task as Task

main : Task Unit Unit
main =
  do
    _ <- IO.println "Creating scheduler..."
    sched <- Scheduler.new

    -- Register a periodic job (every second)
    _ <- IO.println "Registering periodic job..."
    tickResult <- registerTick sched
    case tickResult of
      Err _ -> IO.println "Failed to register tick job"
      Ok _ -> IO.println "Registered tick job"

    -- Register a one-shot job (after 2.5 seconds)
    _ <- IO.println "Registering one-shot job..."
    oneshotResult <- registerOneshot sched
    case oneshotResult of
      Err _ -> IO.println "Failed to register oneshot job"
      Ok _ -> IO.println "Registered oneshot job"

    -- Register a manual job
    _ <- IO.println "Registering manual job..."
    manualResult <- registerManual sched
    case manualResult of
      Err _ -> IO.println "Failed to register manual job"
      Ok _ -> IO.println "Registered manual job"

    -- Start the scheduler
    _ <- IO.println "Starting scheduler..."
    _ <- Scheduler.start sched

    -- Let it run for 3 seconds
    _ <- IO.sleepMs 3000

    -- Manually trigger the manual job
    _ <- IO.println "Triggering manual job..."
    _ <- Scheduler.trigger sched (JobId "manual")

    -- Let it run for 2 more seconds
    _ <- IO.sleepMs 2000

    -- Stop the scheduler
    _ <- IO.println "Stopping scheduler..."
    _ <- Scheduler.stop sched

    IO.println "Done!"

registerTick : Scheduler -> Task SchedulerError Unit
registerTick sched =
  Scheduler.register sched
    { id = JobId "tick"
    , schedule = EveryMs 1000
    , concurrency = OneInstance
    , action = IO.println "  tick!"
    }

registerOneshot : Scheduler -> Task SchedulerError Unit
registerOneshot sched =
  Scheduler.register sched
    { id = JobId "oneshot"
    , schedule = AfterMs 2500
    , concurrency = AllowConcurrent
    , action = IO.println "  oneshot fired!"
    }

registerManual : Scheduler -> Task SchedulerError Unit
registerManual sched =
  Scheduler.register sched
    { id = JobId "manual"
    , schedule = Manual
    , concurrency = OneInstance
    , action = IO.println "  manual job executed!"
    }
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/42_Scheduler.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add examples/42_Scheduler.lune
git commit -m "feat(example): add scheduler example"
```

---

## Task 12: Run and test the scheduler

**Files:**
- None (manual testing)

**Step 1: Build the project**

Run: `cabal build`

Expected: Build succeeds

**Step 2: Run the example**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/42_Scheduler.lune --eval`

Expected output (approximately):
```
Creating scheduler...
Registering periodic job...
Registered tick job
Registering one-shot job...
Registered oneshot job
Registering manual job...
Registered manual job
Starting scheduler...
  tick!
  tick!
  oneshot fired!
  tick!
Triggering manual job...
  manual job executed!
  tick!
  tick!
Stopping scheduler...
Done!
```

**Step 3: Debug if needed**

If the output doesn't match, check:
- Timer fibers are being spawned
- Runner loop is processing the queue
- STM transactions are completing

---

## Task 13: Update golden tests

**Files:**
- Modify: `tests/golden/` (various files)

**Step 1: Run golden tests**

Run: `cabal test golden`

Expected: New example may cause test discovery changes

**Step 2: Accept new golden output**

Run: `cabal test golden --test-options="--accept"`

Expected: All tests pass

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: add golden tests for scheduler example"
```

---

## Task 14: Add startWith for worker pool (optional enhancement)

**Files:**
- Modify: `prelude/Lune/Scheduler.lune`

**Step 1: Add to exports**

Update the module exposing list to include `startWith`:

```lune
module Lune.Scheduler exposing (
  Scheduler,
  JobId(..),
  Schedule(..),
  Concurrency(..),
  JobSpec,
  SchedulerError(..),
  new,
  start,
  startWith,
  stop,
  register,
  unregister,
  trigger
)
```

**Step 2: Add `startWith` function**

```lune
-- | Start the scheduler with multiple worker fibers
startWith : Scheduler -> { workers : Int } -> Task e Unit
startWith sched config =
  do
    _ <- Atomic.commit (Atomic.write sched.state.running True)
    spawnWorkers sched config.workers

spawnWorkers : Scheduler -> Int -> Task e Unit
spawnWorkers sched remaining =
  case Int.lte remaining 0 of
    True -> Task.succeed Unit
    False ->
      do
        _ <- Fiber.spawn (runnerLoop sched)
        spawnWorkers sched (Int.sub remaining 1)
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Scheduler.lune`

Expected: Parses successfully

**Step 4: Commit**

```bash
git add prelude/Lune/Scheduler.lune
git commit -m "feat(scheduler): add startWith for worker pool configuration"
```

---

## Summary

After completing all tasks, the Scheduler delivers:

1. **Schedule types:**
   - `Manual` - only runs when triggered
   - `AfterMs Int` - runs once after delay, then auto-unregisters
   - `EveryMs Int` - runs periodically on interval

2. **Concurrency policies:**
   - `OneInstance` - drops triggers while job is running
   - `AllowConcurrent` - allows multiple instances

3. **API functions:**
   - `new : Task e Scheduler` - create scheduler
   - `start : Scheduler -> Task e Unit` - start with one worker
   - `startWith : Scheduler -> { workers : Int } -> Task e Unit` - start with N workers
   - `stop : Scheduler -> Task e Unit` - graceful shutdown
   - `register : Scheduler -> JobSpec -> Task SchedulerError Unit` - add job
   - `unregister : Scheduler -> JobId -> Task e Unit` - remove job
   - `trigger : Scheduler -> JobId -> Task SchedulerError Unit` - manual trigger

4. **Working example** - `examples/42_Scheduler.lune`

**Future enhancements (not in this plan):**
- `Cron CronExpr` schedule type (requires time arithmetic)
- Job result callbacks
- Job cancellation (killing timer fibers)
- Coalescing triggers for OneInstance (run once more after completion)
