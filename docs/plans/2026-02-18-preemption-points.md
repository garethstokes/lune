# Preemption Points Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add preemption points to the scheduler so IO-bound fibers yield control periodically, enabling fair scheduling.

**Architecture:** Track IO steps in `World`. When a fiber executes too many consecutive IO operations via `chainStep`, automatically inject a `StepYield`. This gives the scheduler a chance to run other fibers without requiring explicit `Fiber.yield` calls.

**Tech Stack:** Pure Haskell, minimal changes to existing scheduler.

---

## Background

Currently, fibers that perform many IO operations in a row (e.g., 1000 `IO.println` calls in a loop) will monopolize the scheduler until they explicitly yield, sleep, or await. This creates unfair scheduling where "busy" fibers can starve others.

The solution counts IO steps and auto-yields after a threshold:
1. Add `worldStepCount :: Int` to `World` to track consecutive IO operations
2. In `chainStep`, increment the counter on each `StepDone`
3. When counter exceeds threshold (100), inject a `StepYield` instead of continuing
4. Reset counter on explicit yield, sleep, or await

This doesn't preempt pure computation, but it does preempt between IO operations—covering the common case of fibers doing lots of IO work.

---

## Task 1: Add Step Counter to World

**Files:**
- Modify: `src/Lune/Eval/Types.hs`

**Step 1: Add worldStepCount field to World**

Add `worldStepCount :: Int` to the `World` data type, after the existing fields:

```haskell
, worldStepCount :: Int  -- Steps since last yield (for preemption)
```

**Step 2: Build to verify**

Run: `cabal build`
Expected: Build will fail in places that construct World—that's expected, we'll fix them next.

---

## Task 2: Update World Construction Sites

**Files:**
- Modify: `src/Lune/Eval/Runtime.hs`
- Modify: `src/Lune/Eval/Scheduler.hs` (if it constructs World)

**Step 1: Find all places constructing World**

Search for `World {` or `World ` record construction.

**Step 2: Add worldStepCount = 0 to each construction site**

Initialize the step count to 0 wherever World is created.

**Step 3: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 3: Implement Auto-Yield in chainStep

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add preemption threshold constant**

Add near the top of the module:

```haskell
-- | Max IO steps before auto-yield for fair scheduling
maxStepsBeforeYield :: Int
maxStepsBeforeYield = 100
```

**Step 2: Modify chainStep StepDone case**

In `chainStep`, modify the `StepDone` case to check step count and auto-yield:

```haskell
StepDone w a ->
  let steps = worldStepCount w
  in if steps >= maxStepsBeforeYield
    then do
      -- Auto-yield: reset counter and yield, then continue
      let w' = w { worldStepCount = 0 }
      pure $ Right $ StepYield w' $ \w'' ->
        case ER.apply k a >>= ER.force of
          Left err -> pure (Left err)
          Right (VIO act2) -> act2 w''
          Right other -> pure (Left (NotAnIO other))
    else
      -- Continue normally, increment counter
      let w' = w { worldStepCount = steps + 1 }
      in case ER.apply k a >>= ER.force of
        Left err -> pure (Left err)
        Right (VIO act2) -> act2 w'
        Right other -> pure (Left (NotAnIO other))
```

**Step 3: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 4: Reset Step Count on Explicit Yield

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Update primYield to reset step count**

Modify `primYield` to reset the counter when explicitly yielding:

```haskell
primYield args =
  case args of
    [] ->
      Right $ VIO $ \world ->
        let world' = world { worldStepCount = 0 }
        in pure $ Right $ StepYield world' $ \w ->
          pure $ Right $ StepDone w (VCon (preludeCon "Unit") [])
    _ -> Left (NotAFunction (VPrim 0 primYield args))
```

**Step 2: Build to verify**

Run: `cabal build`
Expected: Compiles successfully

---

## Task 5: Test Preemption Manually

**Step 1: Create test file**

Create `/tmp/test_preempt.lune`:

```lune
module TestPreempt exposing (main)

import Lune.Prelude exposing (Task, Unit, unit, Int, Bool(..), Applicative(..), Monad(..))
import Lune.IO as IO
import Lune.Fiber as Fiber
import Lune.Task as Task
import Lune.Int as Int
import Lune.String as String

main : Task Unit Unit
main =
  do
    _ <- IO.println "Main: spawning busy fiber"
    _ <- Fiber.spawn busyFiber
    _ <- IO.println "Main: spawning normal fiber"
    _ <- Fiber.spawn normalFiber
    _ <- IO.println "Main: sleeping"
    _ <- IO.sleepMs 500
    IO.println "Main: done"

-- This fiber does lots of IO without explicit yields
busyFiber : Task Unit Unit
busyFiber =
  busyLoop 200 0

busyLoop : Int -> Int -> Task Unit Unit
busyLoop limit i =
  case Int.gte i limit of
    True -> Task.succeed unit
    False ->
      do
        _ <- IO.println (String.append "Busy: " (String.fromInt i))
        busyLoop limit (Int.add i 1)

-- This fiber does IO with sleeps
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
        _ <- IO.sleepMs 50
        normalLoop limit (Int.add i 1)
```

**Step 2: Run test**

Run: `cabal run lune -- --run /tmp/test_preempt.lune`

Expected: Output should show interleaved "Busy:" and "Normal:" messages, proving the busy fiber gets preempted despite not explicitly yielding.

Without preemption, you'd see all 200 "Busy:" messages first, then "Normal:" messages.

---

## Task 6: Run Existing Tests

**Step 1: Run all tests**

Run: `cabal test`

**Step 2: Accept golden test updates if needed**

If fiber-related golden tests changed output order due to preemption:
Run: `cabal test --test-options='--accept'`
Then: `cabal test` again to verify

---

## Task 7: Commit

```bash
git add -A
git commit -m "$(cat <<'EOF'
feat(scheduler): add preemption points for fair scheduling

Fibers that perform many IO operations without yielding are now
automatically preempted after 100 steps, giving other fibers a turn.

Key changes:
- Add worldStepCount to World to track steps since last yield
- chainStep auto-yields when step count exceeds threshold
- Step count resets on explicit yield

This prevents IO-heavy fibers from starving others and enables
fair scheduling without requiring explicit Fiber.yield calls.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Summary

This plan implements fair scheduling with minimal code changes:

| Component | Change |
|-----------|--------|
| `World` | Add `worldStepCount :: Int` field |
| `chainStep` | Check count, auto-yield at threshold |
| `primYield` | Reset count on explicit yield |

The approach is simple because every IO operation flows through `chainStep`, making it the natural place to inject preemption. No changes needed to the evaluator, scheduler loop, or fiber data structures.
