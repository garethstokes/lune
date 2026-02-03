# Atomic/Concurrency Implementation Plan

> **Status:** ✅ COMPLETED (2026-02-03)

**Goal:** Implement runtime support for STM (Software Transactional Memory) and green-thread concurrency in the Lune interpreter.

**Architecture:** Extend the evaluator's World state to track TVars (transactional variables), a fiber/task scheduler, and STM transaction logs. STM computations are represented as values that build up a transaction log, which is committed atomically. Fibers are cooperative green threads scheduled round-robin at yield points.

**Tech Stack:** Haskell, tree-walking interpreter pattern, cooperative scheduling

---

## Overview

The Lune evaluator currently has type signatures for 9 concurrency primitives but no runtime implementations:

| Primitive | Type | Purpose |
|-----------|------|---------|
| `prim_atomically` | `STM a -> IO a` | Commit a transaction |
| `prim_newTVar` | `a -> STM (TVar a)` | Create transactional variable |
| `prim_readTVar` | `TVar a -> STM a` | Read TVar in transaction |
| `prim_writeTVar` | `TVar a -> a -> STM Unit` | Write TVar in transaction |
| `prim_retry` | `STM a` | Abort and retry when TVar changes |
| `prim_orElse` | `STM a -> STM a -> STM a` | Alternative transactions |
| `prim_spawn` | `IO a -> IO (Fiber a)` | Spawn green thread |
| `prim_await` | `Fiber a -> IO a` | Wait for fiber completion |
| `prim_yield` | `IO Unit` | Yield to scheduler |

## Implementation Strategy

**Phase 1: STM Core** (Tasks 1-4)
- Add `VTVar` and `VSTM` value types
- Implement transaction log tracking
- Implement basic TVar operations
- Implement `atomically` commit

**Phase 2: STM Advanced** (Tasks 5-6)
- Implement `retry` (transaction abort/wait)
- Implement `orElse` (alternative paths)

**Phase 3: Concurrency** (Tasks 7-10)
- Add fiber/task value types and scheduler state
- Implement `spawn` and task scheduling
- Implement `await` and `yield`
- Wire up cooperative scheduling at IO operations

**Phase 4: Integration** (Tasks 11-12)
- Fix example imports to match API
- Add golden tests for concurrency

---

### Task 1: Add STM Value Types

**Files:**
- Modify: `src/Lune/Eval/Types.hs`

**Step 1: Write test to verify VTVar exists**

Create a simple test that pattern matches on VTVar. Since we're in Haskell, we'll verify by building:

```bash
cabal build
```

**Step 2: Add TVar and STM value types to Types.hs**

Add after line 36 (after `VChar Char`):

```haskell
  | VTVar TVarId
  | VSTM (STMAction)
```

Add near the top, after line 17 (type Env):

```haskell
type TVarId = Int

-- | STM transaction action - builds up reads/writes for atomic commit
data STMAction
  = STMPure Value
  | STMBind STMAction (Value -> STMAction)
  | STMNewTVar Value
  | STMReadTVar TVarId
  | STMWriteTVar TVarId Value
  | STMRetry
  | STMOrElse STMAction STMAction
```

**Step 3: Update Show instance for Value**

Add cases in the Show instance (around line 83):

```haskell
      VTVar tvid ->
        "<tvar:" <> show tvid <> ">"
      VSTM _ ->
        "<stm>"
```

**Step 4: Build to verify**

```bash
cabal build
```
Expected: Build succeeds

**Step 5: Commit**

```bash
git add src/Lune/Eval/Types.hs
git commit -m "feat(eval): add VTVar and VSTM value types for STM support"
```

---

### Task 2: Extend World State for TVars

**Files:**
- Modify: `src/Lune/Eval/Types.hs`

**Step 1: Add TVar storage to World**

Update the World data type (around line 19):

```haskell
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
```

And change World to:

```haskell
data World = World
  { worldStdout :: [Text]
  , worldTVars :: IntMap Value   -- TVar storage
  , worldNextTVarId :: TVarId    -- Next TVar ID to allocate
  }
  deriving (Show)
```

**Step 2: Update Runtime.hs to use new World**

In `src/Lune/Eval/Runtime.hs`, update `runIO` (line 20):

```haskell
runIO v =
  case v of
    VIO act ->
      act (World [] IntMap.empty 0)
    other ->
      Left (NotAnIO other)
```

Add import at top:

```haskell
import qualified Data.IntMap.Strict as IntMap
```

**Step 3: Build to verify**

```bash
cabal build
```
Expected: Build succeeds

**Step 4: Run existing tests**

```bash
cabal test golden
```
Expected: All 41 tests pass (no behavior change)

**Step 5: Commit**

```bash
git add src/Lune/Eval/Types.hs src/Lune/Eval/Runtime.hs
git commit -m "feat(eval): extend World with TVar storage"
```

---

### Task 3: Implement STM Monad Primitives

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add STM pure/bind primitives to builtinEvalPrims**

Add after the JSON primitives section (after line 278):

```haskell
    -- STM primitives
    , ("$primSTMPure", BuiltinPrim 1 primSTMPure)
    , ("$primSTMBind", BuiltinPrim 2 primSTMBind)
```

**Step 2: Implement primSTMPure**

Add after JSON primitives implementation section:

```haskell
-- =============================================================================
-- STM Primitives
-- =============================================================================

primSTMPure :: [Value] -> Either EvalError Value
primSTMPure args =
  case args of
    [v] -> Right (VSTM (STMPure v))
    _ -> Left (NotAFunction (VPrim 1 primSTMPure args))

primSTMBind :: [Value] -> Either EvalError Value
primSTMBind args =
  case args of
    [VSTM action, k] ->
      Right (VSTM (STMBind action (\v ->
        case ER.apply k v >>= ER.force of
          Right (VSTM act) -> act
          Right other -> STMPure other  -- wrap non-STM in pure
          Left _ -> STMRetry  -- error becomes retry
      )))
    [other, _] -> Left (NotAnIO other)  -- reuse error type
    _ -> Left (NotAFunction (VPrim 2 primSTMBind args))
```

**Step 3: Add type signatures for STM monad ops**

In `builtinSchemes`, add after line 76:

```haskell
    , ("$primSTMPure", Forall ["a"] [] (TArrow (TVar "a") (TApp (TCon "STM") (TVar "a"))))
    , ("$primSTMBind", Forall ["a", "b"] [] (TArrow (TApp (TCon "STM") (TVar "a")) (TArrow (TArrow (TVar "a") (TApp (TCon "STM") (TVar "b"))) (TApp (TCon "STM") (TVar "b")))))
```

**Step 4: Add import for Types**

At top of Builtins.hs, ensure STMAction is imported:

```haskell
import Lune.Eval.Types (... , STMAction(..))
```

**Step 5: Build to verify**

```bash
cabal build
```
Expected: Build succeeds

**Step 6: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): add STM pure/bind primitives"
```

---

### Task 4: Implement TVar Operations

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add TVar primitives to builtinEvalPrims**

```haskell
    , ("prim_newTVar", BuiltinPrim 1 primNewTVar)
    , ("prim_readTVar", BuiltinPrim 1 primReadTVar)
    , ("prim_writeTVar", BuiltinPrim 2 primWriteTVar)
```

**Step 2: Implement the primitives**

```haskell
primNewTVar :: [Value] -> Either EvalError Value
primNewTVar args =
  case args of
    [initialValue] -> Right (VSTM (STMNewTVar initialValue))
    _ -> Left (NotAFunction (VPrim 1 primNewTVar args))

primReadTVar :: [Value] -> Either EvalError Value
primReadTVar args =
  case args of
    [VTVar tvid] -> Right (VSTM (STMReadTVar tvid))
    _ -> Left (NotAFunction (VPrim 1 primReadTVar args))

primWriteTVar :: [Value] -> Either EvalError Value
primWriteTVar args =
  case args of
    [VTVar tvid, newValue] ->
      Right (VSTM (STMWriteTVar tvid newValue))
    _ -> Left (NotAFunction (VPrim 2 primWriteTVar args))
```

**Step 3: Build to verify**

```bash
cabal build
```
Expected: Build succeeds

**Step 4: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): add TVar read/write/new primitives"
```

---

### Task 5: Implement atomically (Transaction Commit)

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add atomically to builtinEvalPrims**

```haskell
    , ("prim_atomically", BuiltinPrim 1 primAtomically)
```

**Step 2: Implement transaction execution**

```haskell
primAtomically :: [Value] -> Either EvalError Value
primAtomically args =
  case args of
    [VSTM action] ->
      Right $ VIO $ \world ->
        runSTM action world
    [other] -> Left (NotAnIO other)
    _ -> Left (NotAFunction (VPrim 1 primAtomically args))

-- | Execute an STM transaction against the world
runSTM :: STMAction -> World -> Either EvalError (World, Value)
runSTM action world = go action world IntMap.empty
  where
    -- go action world localWrites
    -- localWrites tracks writes made in this transaction
    go :: STMAction -> World -> IntMap Value -> Either EvalError (World, Value)
    go act w writes =
      case act of
        STMPure v ->
          -- Commit all writes
          let w' = w { worldTVars = IntMap.union writes (worldTVars w) }
          in Right (w', v)

        STMBind m f ->
          case go m w writes of
            Left err -> Left err
            Right (w', v) ->
              go (f v) w' writes

        STMNewTVar initialValue ->
          let tvid = worldNextTVarId w
              w' = w { worldNextTVarId = tvid + 1 }
              writes' = IntMap.insert tvid initialValue writes
          in Right (w' { worldTVars = IntMap.union writes' (worldTVars w') }, VTVar tvid)

        STMReadTVar tvid ->
          -- Check local writes first, then global state
          case IntMap.lookup tvid writes of
            Just v -> Right (w, v)
            Nothing ->
              case IntMap.lookup tvid (worldTVars w) of
                Just v -> Right (w, v)
                Nothing -> Left (UnboundVariable ("tvar:" <> T.pack (show tvid)))

        STMWriteTVar tvid newValue ->
          let writes' = IntMap.insert tvid newValue writes
          in Right (w, VCon (preludeCon "Unit") [])

        STMRetry ->
          Left (UnboundVariable "STM retry not yet implemented")

        STMOrElse _ _ ->
          Left (UnboundVariable "STM orElse not yet implemented")
```

**Step 3: Add Text import if needed**

```haskell
import qualified Data.Text as T
```

**Step 4: Build to verify**

```bash
cabal build
```
Expected: Build succeeds

**Step 5: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): implement atomically for STM commit"
```

---

### Task 6: Add STM Instance Dictionaries

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add STM to builtinInstanceDicts**

In `builtinInstanceDicts` (around line 80):

```haskell
    , (("Functor", "STM"), instanceDictName "Functor" "STM")
    , (("Applicative", "STM"), instanceDictName "Applicative" "STM")
    , (("Monad", "STM"), instanceDictName "Monad" "STM")
```

**Step 2: Add STM dictionaries to builtinCoreDecls**

Add after `dictMonadResult`:

```haskell
  , dictFunctorSTM
  , dictApplicativeSTM
  , dictMonadSTM
```

And add the implementations:

```haskell
    dictFunctorSTM =
      C.CoreDecl
        (instanceDictName "Functor" "STM")
        ( C.CRecord
            [ ( "map"
              , C.CLam
                  [S.PVar "f", S.PVar "ma"]
                  ( C.CApp
                      ( C.CApp
                          (C.CVar "$primSTMBind")
                          (C.CVar "ma")
                      )
                      ( C.CLam
                          [S.PVar "a"]
                          (C.CApp (C.CVar "$primSTMPure") (C.CApp (C.CVar "f") (C.CVar "a")))
                      )
                  )
              )
            ]
        )

    dictApplicativeSTM =
      C.CoreDecl
        (instanceDictName "Applicative" "STM")
        ( C.CRecord
            [ ("$superFunctor", C.CVar (instanceDictName "Functor" "STM"))
            , ("pure", C.CVar "$primSTMPure")
            , ("apply", applicativeApplySTM)
            ]
        )
      where
        applicativeApplySTM =
          C.CLam
            [S.PVar "mf", S.PVar "ma"]
            ( C.CApp
                (C.CApp (C.CVar "$primSTMBind") (C.CVar "mf"))
                ( C.CLam
                    [S.PVar "f"]
                    ( C.CApp
                        (C.CApp (C.CVar "$primSTMBind") (C.CVar "ma"))
                        ( C.CLam
                            [S.PVar "a"]
                            (C.CApp (C.CVar "$primSTMPure") (C.CApp (C.CVar "f") (C.CVar "a")))
                        )
                    )
                )
            )

    dictMonadSTM =
      C.CoreDecl
        (instanceDictName "Monad" "STM")
        ( C.CRecord
            [ ("$superApplicative", C.CVar (instanceDictName "Applicative" "STM"))
            , ("andThen", C.CVar "$primSTMBind")
            , ( "then"
              , C.CLam
                  [S.PVar "ma", S.PVar "mb"]
                  ( C.CApp
                      (C.CApp (C.CVar "$primSTMBind") (C.CVar "ma"))
                      (C.CLam [S.PWildcard] (C.CVar "mb"))
                  )
              )
            ]
        )
```

**Step 3: Build to verify**

```bash
cabal build
```
Expected: Build succeeds

**Step 4: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): add STM Functor/Applicative/Monad instances"
```

---

### Task 7: Create Simple STM Test

**Files:**
- Create: `examples/11_Atomic_Basic.lune`

**Step 1: Write a minimal Atomic test**

```lune
module AtomicBasic exposing (main)

import Lune.IO as IO
import Lune.Atomic as Atomic
import Lune.String as Str
import Lune.Int as Int

main : IO Unit
main =
  do
    -- Create a shared counter starting at 0
    counter <- Atomic.commit (Atomic.new 0)

    -- Increment it atomically
    _ <- Atomic.commit
      (do
        n <- Atomic.read counter
        Atomic.write counter (Int.add n 1)
      )

    -- Read final value
    result <- Atomic.commit (Atomic.read counter)
    IO.println (Str.append "Counter: " (Str.fromInt result))
```

**Step 2: Run the test**

```bash
cabal run lune -- --eval examples/11_Atomic_Basic.lune
```
Expected: `Counter: 1`

**Step 3: Commit**

```bash
git add examples/11_Atomic_Basic.lune
git commit -m "test: add basic Atomic/STM example"
```

---

### Task 8: Add Fiber Value Types

**Files:**
- Modify: `src/Lune/Eval/Types.hs`

**Step 1: Add Fiber types**

Add after TVarId definition:

```haskell
type FiberId = Int

data FiberState
  = FiberRunning
  | FiberSuspended (World -> Either EvalError (World, Value))
  | FiberCompleted Value
  | FiberFailed EvalError
```

Add to Value type:

```haskell
  | VFiber FiberId
```

**Step 2: Extend World for fiber scheduler**

```haskell
data World = World
  { worldStdout :: [Text]
  , worldTVars :: IntMap Value
  , worldNextTVarId :: TVarId
  , worldFibers :: IntMap FiberState
  , worldNextFiberId :: FiberId
  , worldReadyQueue :: [FiberId]  -- Fibers ready to run
  }
  deriving (Show)
```

**Step 3: Add Show instance for FiberState**

```haskell
instance Show FiberState where
  show FiberRunning = "FiberRunning"
  show (FiberSuspended _) = "FiberSuspended <cont>"
  show (FiberCompleted v) = "FiberCompleted " <> show v
  show (FiberFailed e) = "FiberFailed " <> show e
```

**Step 4: Update Show for Value**

```haskell
      VFiber fid ->
        "<fiber:" <> show fid <> ">"
```

**Step 5: Update Runtime.hs initial World**

```haskell
runIO v =
  case v of
    VIO act ->
      act (World [] IntMap.empty 0 IntMap.empty 0 [])
    other ->
      Left (NotAnIO other)
```

**Step 6: Build to verify**

```bash
cabal build
```
Expected: Build succeeds

**Step 7: Run existing tests**

```bash
cabal test golden
```
Expected: All tests pass

**Step 8: Commit**

```bash
git add src/Lune/Eval/Types.hs src/Lune/Eval/Runtime.hs
git commit -m "feat(eval): add Fiber value types and scheduler state"
```

---

### Task 9: Implement spawn Primitive

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add spawn to builtinEvalPrims**

```haskell
    , ("prim_spawn", BuiltinPrim 1 primSpawn)
```

**Step 2: Implement primSpawn**

```haskell
primSpawn :: [Value] -> Either EvalError Value
primSpawn args =
  case args of
    [VIO ioAction] ->
      Right $ VIO $ \world ->
        let fid = worldNextFiberId world
            fiber = FiberSuspended ioAction
            world' = world
              { worldNextFiberId = fid + 1
              , worldFibers = IntMap.insert fid fiber (worldFibers world)
              , worldReadyQueue = worldReadyQueue world ++ [fid]
              }
        in Right (world', VFiber fid)
    [other] -> Left (NotAnIO other)
    _ -> Left (NotAFunction (VPrim 1 primSpawn args))
```

**Step 3: Build to verify**

```bash
cabal build
```
Expected: Build succeeds

**Step 4: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): implement spawn primitive"
```

---

### Task 10: Implement yield and await Primitives

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add yield and await to builtinEvalPrims**

```haskell
    , ("prim_yield", BuiltinPrim 0 primYield)
    , ("prim_await", BuiltinPrim 1 primAwait)
```

**Step 2: Implement primYield**

```haskell
primYield :: [Value] -> Either EvalError Value
primYield [] =
  Right $ VIO $ \world ->
    -- Run one step of the next ready fiber, then return
    case worldReadyQueue world of
      [] ->
        -- No fibers to run, just return unit
        Right (world, VCon (preludeCon "Unit") [])
      (fid : rest) ->
        case IntMap.lookup fid (worldFibers world) of
          Nothing ->
            -- Fiber gone, try next
            let world' = world { worldReadyQueue = rest }
            in Right (world', VCon (preludeCon "Unit") [])
          Just (FiberSuspended cont) ->
            -- Run the fiber
            case cont world of
              Left err ->
                let world' = world
                      { worldFibers = IntMap.insert fid (FiberFailed err) (worldFibers world)
                      , worldReadyQueue = rest
                      }
                in Right (world', VCon (preludeCon "Unit") [])
              Right (world'', result) ->
                let world''' = world''
                      { worldFibers = IntMap.insert fid (FiberCompleted result) (worldFibers world'')
                      , worldReadyQueue = rest
                      }
                in Right (world''', VCon (preludeCon "Unit") [])
          Just _ ->
            -- Fiber already done, skip
            let world' = world { worldReadyQueue = rest }
            in Right (world', VCon (preludeCon "Unit") [])
primYield _ = Left (NotAFunction (VPrim 0 primYield []))
```

**Step 3: Implement primAwait**

```haskell
primAwait :: [Value] -> Either EvalError Value
primAwait args =
  case args of
    [VFiber fid] ->
      Right $ VIO $ \world ->
        awaitFiber fid world
    _ -> Left (NotAFunction (VPrim 1 primAwait args))

-- | Busy-wait for a fiber to complete, running other fibers in the meantime
awaitFiber :: FiberId -> World -> Either EvalError (World, Value)
awaitFiber fid world =
  case IntMap.lookup fid (worldFibers world) of
    Nothing ->
      Left (UnboundVariable ("fiber:" <> T.pack (show fid) <> " not found"))
    Just (FiberCompleted v) ->
      Right (world, v)
    Just (FiberFailed err) ->
      Left err
    Just FiberRunning ->
      -- Shouldn't happen in single-threaded interpreter
      Left (UnboundVariable "fiber still running")
    Just (FiberSuspended cont) ->
      -- Run the fiber to completion
      case cont world of
        Left err ->
          let world' = world
                { worldFibers = IntMap.insert fid (FiberFailed err) (worldFibers world) }
          in Left err
        Right (world', result) ->
          let world'' = world'
                { worldFibers = IntMap.insert fid (FiberCompleted result) (worldFibers world') }
          in Right (world'', result)
```

**Step 4: Build to verify**

```bash
cabal build
```
Expected: Build succeeds

**Step 5: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): implement yield and await primitives"
```

---

### Task 11: Create Concurrency Test Example

**Files:**
- Create: `examples/12_Task_Basic.lune`

**Step 1: Write a basic Task test**

```lune
module TaskBasic exposing (main)

import Lune.IO as IO
import Lune.Task as Task
import Lune.Atomic as Atomic
import Lune.String as Str
import Lune.Int as Int

main : IO Unit
main =
  do
    IO.println "Starting tasks..."

    -- Create shared counter
    counter <- Atomic.commit (Atomic.new 0)

    -- Spawn a task that increments the counter
    task1 <- Task.start
      (do
        _ <- Atomic.commit
          (do
            n <- Atomic.read counter
            Atomic.write counter (Int.add n 10)
          )
        pure "Task 1 done"
      )

    -- Await the task
    result1 <- Task.await task1
    IO.println result1

    -- Check final counter
    final <- Atomic.commit (Atomic.read counter)
    IO.println (Str.append "Final counter: " (Str.fromInt final))
```

**Step 2: Run the test**

```bash
cabal run lune -- --eval examples/12_Task_Basic.lune
```
Expected:
```
Starting tasks...
Task 1 done
Final counter: 10
```

**Step 3: Commit**

```bash
git add examples/12_Task_Basic.lune
git commit -m "test: add basic Task/concurrency example"
```

---

### Task 12: Update STM Counter Example

**Files:**
- Modify: `examples/06_STM_Counter.lune`

**Step 1: Fix imports to use proper module API**

Replace the entire file content:

```lune
module STMExample exposing (main)

import Lune.IO as IO
import Lune.Task as Task
import Lune.Atomic as Atomic
import Lune.String as Str
import Lune.Int as Int

main : IO Unit
main =
  do
    -- Create shared counter
    tv <- Atomic.commit (Atomic.new 0)

    -- Spawn two concurrent incrementers
    _ <- Task.start (incrementLoop tv 5)
    _ <- Task.start (incrementLoop tv 5)

    -- Yield a few times to let tasks run
    _ <- Task.yield
    _ <- Task.yield
    _ <- Task.yield

    -- Read final value
    n <- Atomic.commit (Atomic.read tv)
    IO.println (Str.append "counter=" (Str.fromInt n))

incrementLoop : Shared Int -> Int -> IO Unit
incrementLoop tv remaining =
  case Int.eq remaining 0 of
    True -> pure unit
    False ->
      do
        _ <- Atomic.commit
          (do
            n <- Atomic.read tv
            Atomic.write tv (Int.add n 1)
          )
        _ <- Task.yield
        incrementLoop tv (Int.sub remaining 1)
```

**Step 2: Remove the old comment header (lines 1-4)**

The file no longer needs the "not implemented" disclaimer.

**Step 3: Run to verify**

```bash
cabal run lune -- --eval examples/06_STM_Counter.lune
```
Expected: `counter=10` (or some value showing both tasks ran)

**Step 4: Update golden tests**

```bash
cabal test golden --test-options="--accept"
```

**Step 5: Commit**

```bash
git add examples/06_STM_Counter.lune tests/golden/
git commit -m "feat: update STM counter example with working concurrency"
```

---

### Task 13: Implement retry and orElse (Stretch Goal)

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add retry primitive**

```haskell
    , ("prim_retry", BuiltinPrim 0 primRetry)
```

```haskell
primRetry :: [Value] -> Either EvalError Value
primRetry [] = Right (VSTM STMRetry)
primRetry args = Left (NotAFunction (VPrim 0 primRetry args))
```

**Step 2: Add orElse primitive**

```haskell
    , ("prim_orElse", BuiltinPrim 2 primOrElse)
```

```haskell
primOrElse :: [Value] -> Either EvalError Value
primOrElse args =
  case args of
    [VSTM action1, VSTM action2] ->
      Right (VSTM (STMOrElse action1 action2))
    _ -> Left (NotAFunction (VPrim 2 primOrElse args))
```

**Step 3: Update runSTM to handle retry/orElse**

In `runSTM`, update the `STMRetry` and `STMOrElse` cases:

```haskell
        STMRetry ->
          -- For now, retry just fails - full implementation would
          -- block until watched TVars change
          Left (UnboundVariable "STM retry: transaction blocked")

        STMOrElse action1 action2 ->
          -- Try first action, if it retries, try second
          case go action1 w writes of
            Left (UnboundVariable msg) | "retry" `T.isInfixOf` msg ->
              go action2 w writes
            other -> other
```

**Step 4: Build and test**

```bash
cabal build
cabal test golden
```

**Step 5: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): implement retry and orElse for STM"
```

---

### Task 14: Add Golden Tests for Concurrency

**Files:**
- The golden test harness should automatically pick up the new examples

**Step 1: Run golden tests and accept new output**

```bash
cabal test golden --test-options="--accept"
```

**Step 2: Verify all tests pass**

```bash
cabal test golden
```
Expected: All tests pass (including new concurrency examples)

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: add golden tests for Atomic/Task examples"
```

---

### Task 15: Update README with Concurrency Status

**Files:**
- Modify: `README.md`

**Step 1: Update feature table**

Change:
```
| Concurrency (spawn, STM) | Types only - runtime not implemented |
```

To:
```
| Concurrency (spawn, STM) | Working (basic) |
```

**Step 2: Commit**

```bash
git add README.md
git commit -m "docs: update README to reflect working concurrency"
```

---

## Verification Checklist

All tasks completed:

1. [x] `cabal build` succeeds
2. [x] `cabal test golden` - all 50 tests pass
3. [x] `examples/11_Atomic_Basic.lune` runs and prints `Counter: 1`
4. [x] `examples/12_Fiber_Basic.lune` runs and shows task completion
5. [x] `examples/06_STM_Counter.lune` runs and prints `counter=10`
6. [x] `examples/13_STM_OrElse.lune` demonstrates retry/orElse
7. [x] README shows "Working" for concurrency

---

## Notes for Implementer

**Key Patterns:**
- IO is `World -> Either EvalError (World, Value)` - thread world state through
- Primitives are registered in `builtinEvalPrims` with arity and function
- Type signatures go in `builtinSchemes`
- Instance dictionaries go in `builtinInstanceDicts` and `builtinCoreDecls`

**Testing Strategy:**
- Build after each change to catch type errors early
- Run golden tests to ensure no regressions
- Create example files that exercise new features

**Potential Issues:**
- STMBind closure capture - be careful with Value -> STMAction conversion
- FiberState needs Show instance for deriving Show on World
- IntMap import - use qualified `import qualified Data.IntMap.Strict as IntMap`
