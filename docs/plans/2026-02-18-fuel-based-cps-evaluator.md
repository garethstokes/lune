# Fuel-Based CPS Evaluator Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Transform the pure evaluator to use continuation-passing style (CPS) with fuel, enabling preemption of pure computation for fair fiber scheduling.

**Architecture:** Pure evaluation becomes fuel-based and can suspend mid-computation. The IO scheduler resumes suspended evaluations with fresh fuel. This is separate from IO-level preemption (StepYield) - pure suspensions become StepYield at the IO boundary.

**Tech Stack:** Pure Haskell CPS, no additional dependencies.

---

## Background

Currently, `evalExpr` runs to completion:
```haskell
evalExpr :: Env -> C.CoreExpr -> Either EvalError Value
```

A pure infinite loop like `let loop x = loop x` never yields. We need evaluation to periodically return control.

The CPS solution:
1. Transform `evalExpr` to take a continuation parameter
2. Thread fuel through evaluation, decrement on each step
3. When fuel exhausted, return a suspension with the continuation
4. Scheduler resumes with fresh fuel

---

## Task 1: Add EvalResult and EvalCont Types

**Files:**
- Modify: `src/Lune/Eval/Types.hs`

**Step 1: Add the types after EvalError**

Add these type definitions:

```haskell
-- | Continuation for CPS evaluation
-- Takes remaining fuel and a value, produces a result
type EvalCont = Int -> Value -> EvalResult

-- | Result of fuel-based evaluation
data EvalResult
  = EvalDone Value
    -- ^ Evaluation completed with this value
  | EvalSuspend (Int -> EvalResult)
    -- ^ Out of fuel; give fresh fuel to resume
  | EvalError EvalError
    -- ^ Evaluation failed
```

**Step 2: Export the new types**

Add `EvalResult (..)` and `EvalCont` to the module exports.

**Step 3: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 2: Add Fuel Constant and CPS Evaluator Skeleton

**Files:**
- Modify: `src/Lune/Eval/Runtime.hs`

**Step 1: Add fuel constant**

Add near the top of the module:

```haskell
-- | Default fuel for pure evaluation before yielding
defaultEvalFuel :: Int
defaultEvalFuel = 1000
```

**Step 2: Add evalExprCPS signature and basic structure**

Add after the existing `evalExpr`:

```haskell
-- | CPS evaluator with fuel
evalExprCPS :: Int -> Env -> C.CoreExpr -> EvalCont -> EvalResult
evalExprCPS fuel env expr k
  | fuel <= 0 = EvalSuspend $ \fuel' -> evalExprCPS fuel' env expr k
  | otherwise =
      case expr of
        C.CVar name ->
          evalVarCPS (fuel - 1) env name k

        C.CString s ->
          k (fuel - 1) (VString s)

        C.CInt n ->
          k (fuel - 1) (VInt n)

        C.CFloat f ->
          k (fuel - 1) (VFloat f)

        C.CChar c ->
          k (fuel - 1) (VChar c)

        C.CLam pats body ->
          k (fuel - 1) (VClosure env pats body)

        C.CApp f x ->
          evalExprCPS (fuel - 1) env f $ \fuel1 fv ->
            forceCPS fuel1 fv $ \fuel2 fv' ->
              evalExprCPS fuel2 env x $ \fuel3 xv ->
                forceCPS fuel3 xv $ \fuel4 xv' ->
                  applyCPS fuel4 fv' xv' k

        C.CLet name bound body ->
          evalExprCPS (fuel - 1) env bound $ \fuel1 bv ->
            forceCPS fuel1 bv $ \fuel2 bv' ->
              evalExprCPS fuel2 (Map.insert name bv' env) body k

        C.CCase scrut alts ->
          evalExprCPS (fuel - 1) env scrut $ \fuel1 sv ->
            forceCPS fuel1 sv $ \fuel2 sv' ->
              evalCaseAltsCPS fuel2 env sv' alts k

        C.CRecord fields ->
          evalRecordCPS (fuel - 1) env fields [] k

        C.CSelect base field ->
          evalExprCPS (fuel - 1) env base $ \fuel1 bv ->
            forceCPS fuel1 bv $ \fuel2 bv' ->
              case bv' of
                VRecord fields' ->
                  case Map.lookup field fields' of
                    Nothing -> EvalError (MissingField field)
                    Just v -> k fuel2 v
                other ->
                  EvalError (NotARecord other)

        C.CTemplate isBlock parts ->
          let parts' = [ case p of
                           C.CTemplateText t -> TText t
                           C.CTemplateHole ty holeExpr ->
                             THole (TemplateHole ty Nothing (VThunk env holeExpr))
                       | p <- parts ]
          in k (fuel - 1) (VTemplate (Template.leaf isBlock (Seq.fromList parts')))

        C.CDictWanted c ->
          EvalError (UnexpectedDictWanted c)

        C.CForeignImport convention symbol ty ->
          case foreignPrim convention symbol ty of
            Left err -> EvalError err
            Right v -> k (fuel - 1) v
```

**Step 3: Build to see what's missing**

Run: `cabal build`
Expected: Errors about missing `evalVarCPS`, `forceCPS`, `applyCPS`, `evalCaseAltsCPS`, `evalRecordCPS`

---

## Task 3: Add CPS Helper Functions

**Files:**
- Modify: `src/Lune/Eval/Runtime.hs`

**Step 1: Add forceCPS**

```haskell
-- | CPS version of force
forceCPS :: Int -> Value -> EvalCont -> EvalResult
forceCPS fuel v k
  | fuel <= 0 = EvalSuspend $ \fuel' -> forceCPS fuel' v k
  | otherwise =
      case v of
        VThunk thunkEnv thunkExpr ->
          evalExprCPS fuel thunkEnv thunkExpr k
        _ ->
          k fuel v
```

**Step 2: Add applyCPS**

```haskell
-- | CPS version of apply
applyCPS :: Int -> Value -> Value -> EvalCont -> EvalResult
applyCPS fuel f arg k
  | fuel <= 0 = EvalSuspend $ \fuel' -> applyCPS fuel' f arg k
  | otherwise =
      case f of
        VClosure cloEnv pats body ->
          case pats of
            [] ->
              EvalError (NotAFunction f)
            (p : ps) ->
              case matchPattern p arg of
                Nothing -> EvalError (PatternMatchFailure p arg)
                Just binds ->
                  let env' = binds <> cloEnv
                  in if null ps
                       then evalExprCPS (fuel - 1) env' body k
                       else k (fuel - 1) (VClosure env' ps body)
        VCon name args ->
          k (fuel - 1) (VCon name (args <> [arg]))
        VPrim arity fn args ->
          let args' = args <> [arg]
          in if length args' == arity
               then case fn args' of
                      Left err -> EvalError err
                      Right v -> k (fuel - 1) v
               else k (fuel - 1) (VPrim arity fn args')
        _ ->
          EvalError (NotAFunction f)
```

**Step 3: Add evalVarCPS**

```haskell
-- | CPS version of variable lookup
evalVarCPS :: Int -> Env -> Text -> EvalCont -> EvalResult
evalVarCPS fuel env name k =
  case Map.lookup name env of
    Just v -> forceCPS fuel v k
    Nothing ->
      if isConstructorName name
        then k fuel (VCon name [])
        else EvalError (UnboundVariable name)
```

**Step 4: Add evalCaseAltsCPS**

```haskell
-- | CPS version of case alternative matching
evalCaseAltsCPS :: Int -> Env -> Value -> [C.CoreAlt] -> EvalCont -> EvalResult
evalCaseAltsCPS fuel env scrut alts k =
  go fuel alts
  where
    go _ [] = EvalError (NonExhaustiveCase scrut)
    go f (C.CoreAlt pat body : rest) =
      case matchPattern pat scrut of
        Nothing -> go f rest
        Just binds -> evalExprCPS f (binds <> env) body k
```

**Step 5: Add evalRecordCPS**

```haskell
-- | CPS version of record evaluation
evalRecordCPS :: Int -> Env -> [(Text, C.CoreExpr)] -> [(Text, Value)] -> EvalCont -> EvalResult
evalRecordCPS fuel _ [] acc k =
  k fuel (VRecord (Map.fromList (reverse acc)))
evalRecordCPS fuel env ((name, expr) : rest) acc k
  | fuel <= 0 = EvalSuspend $ \fuel' -> evalRecordCPS fuel' env ((name, expr) : rest) acc k
  | otherwise =
      evalExprCPS (fuel - 1) env expr $ \fuel1 v ->
        forceCPS fuel1 v $ \fuel2 v' ->
          evalRecordCPS fuel2 env rest ((name, v') : acc) k
```

**Step 6: Build to verify**

Run: `cabal build`
Expected: Compiles successfully (may have warnings)

---

## Task 4: Add Wrapper Functions and runEvalResult

**Files:**
- Modify: `src/Lune/Eval/Runtime.hs`

**Step 1: Add runEvalResult**

```haskell
-- | Run EvalResult to completion (for non-preemptive contexts)
-- Keeps giving fresh fuel until evaluation completes
runEvalResult :: EvalResult -> Either EvalError Value
runEvalResult result =
  case result of
    EvalDone v -> Right v
    EvalError err -> Left err
    EvalSuspend resume -> runEvalResult (resume defaultEvalFuel)
```

**Step 2: Add doneK helper**

```haskell
-- | Terminal continuation - just return the value
doneK :: EvalCont
doneK _ v = EvalDone v
```

**Step 3: Update evalExpr to use CPS version**

Replace the existing `evalExpr` implementation:

```haskell
-- | Evaluate expression (runs to completion or error)
evalExpr :: Env -> C.CoreExpr -> Either EvalError Value
evalExpr env expr =
  runEvalResult $ evalExprCPS defaultEvalFuel env expr doneK
```

**Step 4: Update force to use CPS version**

Replace the existing `force` implementation:

```haskell
-- | Force a thunk (runs to completion)
force :: Value -> Either EvalError Value
force v =
  runEvalResult $ forceCPS defaultEvalFuel v doneK
```

**Step 5: Update apply to use CPS version**

Replace the existing `apply` implementation:

```haskell
-- | Apply function to argument (runs to completion)
apply :: Value -> Value -> Either EvalError Value
apply f arg =
  runEvalResult $ applyCPS defaultEvalFuel f arg doneK
```

**Step 6: Export new functions**

Add to exports: `defaultEvalFuel`, `evalExprCPS`, `forceCPS`, `applyCPS`, `doneK`, `runEvalResult`, `EvalCont`, `EvalResult(..)`

Wait - `EvalCont` and `EvalResult` are in Types.hs, not Runtime.hs. Just re-export them from Runtime.hs or update the export list accordingly.

**Step 7: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 5: Add runPureToIO Integration

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Import new CPS functions**

Ensure imports include:
```haskell
import Lune.Eval.Types (EvalResult(..), EvalCont)
```

And from Runtime:
```haskell
import qualified Lune.Eval.Runtime as ER
-- ER.applyCPS, ER.defaultEvalFuel, ER.doneK are now available
```

**Step 2: Add runPureToIO function**

Add this function:

```haskell
-- | Run pure evaluation result, converting suspensions to StepYield
runPureToIO :: World -> EvalResult -> IO (Either EvalError IOStep)
runPureToIO w result =
  case result of
    EvalDone (VIO act) -> act w
    EvalDone other -> pure (Left (NotAnIO other))
    EvalError err -> pure (Left err)
    EvalSuspend resume ->
      -- Pure computation needs more fuel - yield to scheduler
      pure $ Right $ StepYield w $ \w' ->
        runPureToIO w' (resume ER.defaultEvalFuel)
```

**Step 3: Update chainStep to use runPureToIO**

Modify the `StepDone` case in `chainStep`:

```haskell
chainStep :: IOStep -> Value -> IO (Either EvalError IOStep)
chainStep step k =
  case step of
    StepDone w a ->
      let steps = worldStepCount w
      in if steps >= maxStepsBeforeYield
        then do
          -- Auto-yield: reset counter and yield, then continue
          let w' = w { worldStepCount = 0 }
          pure $ Right $ StepYield w' $ \w'' ->
            runPureToIO w'' (ER.applyCPS ER.defaultEvalFuel k a ER.doneK)
        else do
          -- Continue normally, increment counter
          let w' = w { worldStepCount = steps + 1 }
          runPureToIO w' (ER.applyCPS ER.defaultEvalFuel k a ER.doneK)

    -- Other cases remain the same but also need updating...
    StepYield w cont ->
      pure $ Right $ StepYield w $ \w' -> do
        result <- cont w'
        case result of
          Left err -> pure (Left err)
          Right step' -> chainStep step' k

    StepSleep w ms cont ->
      pure $ Right $ StepSleep w ms $ \w' -> do
        result <- cont w'
        case result of
          Left err -> pure (Left err)
          Right step' -> chainStep step' k

    StepAwait w fid cont ->
      pure $ Right $ StepAwait w fid $ \w' v -> do
        result <- cont w' v
        case result of
          Left err -> pure (Left err)
          Right step' -> chainStep step' k

    StepSpawn w newCont resumeCont ->
      pure $ Right $ StepSpawn w newCont $ \w' fid -> do
        result <- resumeCont w' fid
        case result of
          Left err -> pure (Left err)
          Right step' -> chainStep step' k
```

**Step 4: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 6: Update chainStepThen Similarly

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Update chainStepThen**

The `chainStepThen` function also needs updating. Find it and update the `StepDone` case:

```haskell
chainStepThen :: IOStep -> (World -> IO (Either EvalError IOStep)) -> IO (Either EvalError IOStep)
chainStepThen step act2 =
  case step of
    StepDone w _ -> act2 w
    -- ... other cases unchanged
```

This one is simpler since it ignores the value and just runs `act2`.

**Step 2: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 7: Test Pure Preemption

**Step 1: Create test file**

Create `/tmp/test_pure_preempt.lune`:

```lune
module TestPurePreempt exposing (main)

import Lune.Prelude exposing (Task, Unit, unit, Int, Bool(..), Applicative(..), Monad(..))
import Lune.IO as IO
import Lune.Fiber as Fiber
import Lune.Task as Task
import Lune.Int as Int
import Lune.String as String

main : Task Unit Unit
main =
  do
    _ <- IO.println "Main: spawning CPU-heavy fiber"
    _ <- Fiber.spawn cpuHeavyFiber
    _ <- IO.println "Main: spawning normal fiber"
    _ <- Fiber.spawn normalFiber
    _ <- IO.println "Main: sleeping"
    _ <- IO.sleepMs 100
    IO.println "Main: done"

-- This fiber does heavy pure computation between IO
cpuHeavyFiber : Task Unit Unit
cpuHeavyFiber =
  cpuLoop 5 0

cpuLoop : Int -> Int -> Task Unit Unit
cpuLoop limit i =
  case Int.gte i limit of
    True -> Task.succeed unit
    False ->
      do
        -- Do expensive pure computation
        let result = fib 25
        _ <- IO.println (String.append "CPU: fib(25) = " (String.fromInt result))
        cpuLoop limit (Int.add i 1)

-- Fibonacci - pure computation
fib : Int -> Int
fib n =
  case Int.lte n 1 of
    True -> n
    False -> Int.add (fib (Int.sub n 1)) (fib (Int.sub n 2))

-- Normal fiber with sleeps
normalFiber : Task Unit Unit
normalFiber =
  normalLoop 5 0

normalLoop : Int -> Int -> Task Unit Unit
normalLoop limit i =
  case Int.gte i limit of
    True -> Task.succeed unit
    False ->
      do
        _ <- IO.println (String.append "Normal: " (String.fromInt i))
        _ <- IO.sleepMs 10
        normalLoop limit (Int.add i 1)
```

**Step 2: Run test**

Run: `cabal run lune -- --run /tmp/test_pure_preempt.lune`

Expected: Output should show interleaved "CPU:" and "Normal:" messages. The CPU-heavy fiber doing `fib 25` should be preempted mid-computation, allowing the normal fiber to run.

Without fuel-based preemption, you'd see all "CPU:" messages first (fib runs to completion), then "Normal:" messages.

---

## Task 8: Run Existing Tests

**Step 1: Run all tests**

Run: `cabal test`

**Step 2: Accept golden test updates if needed**

If any golden tests changed:
Run: `cabal test --test-options='--accept'`
Then: `cabal test` again to verify

---

## Task 9: Commit

```bash
git add -A
git commit -m "$(cat <<'EOF'
feat(eval): implement fuel-based CPS evaluator for fair scheduling

Transform pure evaluation to continuation-passing style with fuel,
enabling preemption of pure computation for true cooperative scheduling.

Key changes:
- Add EvalResult type (EvalDone | EvalSuspend | EvalError)
- Add EvalCont type for CPS continuations
- Implement evalExprCPS, forceCPS, applyCPS with fuel threading
- Add runPureToIO to bridge pure suspensions to IO yields
- Update chainStep to use CPS evaluation

Pure computation now yields after ~1000 eval steps, preventing
infinite loops from starving other fibers. Combined with IO-step
preemption, this gives true fair scheduling.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Summary

| Component | Change |
|-----------|--------|
| `EvalResult` | New type: `EvalDone \| EvalSuspend \| EvalError` |
| `EvalCont` | Type alias: `Int -> Value -> EvalResult` |
| `evalExprCPS` | CPS evaluator with fuel threading |
| `forceCPS`, `applyCPS` | CPS versions of helpers |
| `runPureToIO` | Bridge pure suspensions to `StepYield` |
| `chainStep` | Use CPS evaluation for continuations |

The key insight: CPS naturally captures "where we are" in closures. When fuel runs out, we return `EvalSuspend (\fuel -> ...)` where the closure contains the entire continuation. The scheduler resumes by calling it with fresh fuel.
