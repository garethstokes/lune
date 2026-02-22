# IO/Task Primitives Encapsulation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Hide `prim_ioPure` and `prim_ioBind` behind Elm-style API names in IO and Task modules, and add `Task.result` for error-to-Result conversion.

**Architecture:** Add `IO.pure` and `IO.andThen` to wrap the IO primitives. Add `Task.result` to convert failing tasks to Result values. Update all prelude and example files to use the new API instead of raw primitives.

**Tech Stack:** Lune (Elm-style functional language)

---

### Task 1: Add IO.pure and IO.andThen to Lune.IO

**Files:**
- Modify: `prelude/Lune/IO.lune:1-40`

**Step 1: Add pure and andThen to exports**

Edit the module declaration to add the new exports:

```lune
module Lune.IO exposing (
  println,
  readLine,
  readInt,
  sleepMs,
  readFile,
  writeFile,
  Error,
  pure,
  andThen
)
```

**Step 2: Add pure function**

Add after line 15 (after the Error type):

```lune
-- | Lift a pure value into IO.
pure : a -> IO a
pure a =
  prim_ioPure a

-- | Sequence two IO actions.
andThen : IO a -> (a -> IO b) -> IO b
andThen io f =
  prim_ioBind io f
```

**Step 3: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add prelude/Lune/IO.lune
git commit -m "feat(IO): add pure and andThen to wrap prim_* functions"
```

---

### Task 2: Add Task.result to Lune.Task

**Files:**
- Modify: `prelude/Lune/Task.lune:1-93`

**Step 1: Add result to exports**

Edit the module declaration:

```lune
module Lune.Task exposing (
  succeed,
  fail,
  fromIO,
  map,
  andThen,
  then,
  mapError,
  onError,
  attempt,
  result
)
```

**Step 2: Add result function**

Add at the end of the file (after line 93):

```lune
-- | Convert a Task's success/failure into a Result value.
-- The returned Task never fails (error type is unconstrained).
result : Task e a -> Task x (Result e a)
result task =
  onError (map Ok task) (\e -> succeed (Err e))
```

**Step 3: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add prelude/Lune/Task.lune
git commit -m "feat(Task): add result for Task e a -> Task x (Result e a)"
```

---

### Task 3: Update Nova/Core.lune to use IO.pure

**Files:**
- Modify: `prelude/Nova/Core.lune`

**Step 1: Add IO import**

After line 35 (after the Str import), add:

```lune
import Lune.IO as IO
```

**Step 2: Replace prim_ioPure calls**

Replace all `prim_ioPure` with `IO.pure`:

- Line 255: `prim_ioPure (Err ...)` → `IO.pure (Err ...)`
- Line 295: `prim_ioPure (Err Unauthorized)` → `IO.pure (Err Unauthorized)`
- Line 298: `prim_ioPure (Err ...)` → `IO.pure (Err ...)`
- Line 305: `prim_ioPure (mockOllama ...)` → `IO.pure (mockOllama ...)`
- Line 308: `prim_ioPure (Err ...)` → `IO.pure (Err ...)`

**Step 3: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 4: Run example to verify behavior unchanged**

Run: `cabal run lune -- examples/45_NovaV2.lune`
Expected: Same output as before

**Step 5: Commit**

```bash
git add prelude/Nova/Core.lune
git commit -m "refactor(Nova/Core): use IO.pure instead of prim_ioPure"
```

---

### Task 4: Update Nova.lune to use IO.pure and IO.andThen

**Files:**
- Modify: `prelude/Nova.lune`

**Step 1: Add IO import if not present**

Check for and add if needed:

```lune
import Lune.IO as IO
```

**Step 2: Replace prim_ioPure and prim_ioBind calls**

- Line 411: `prim_ioPure` → `IO.pure`
- Line 468: `prim_ioBind` → `IO.andThen`
- Line 471: `prim_ioPure` → `IO.pure`
- Line 474: `prim_ioPure` → `IO.pure`

**Step 3: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add prelude/Nova.lune
git commit -m "refactor(Nova): use IO.pure/andThen instead of prim_* functions"
```

---

### Task 5: Update Lune.Time to use IO.pure and IO.andThen

**Files:**
- Modify: `prelude/Lune/Time.lune`

**Step 1: Add IO import**

```lune
import Lune.IO as IO
```

**Step 2: Replace primitive calls**

Line 52: Replace `Task (prim_ioBind prim_timeNowMicros (\micros -> prim_ioPure (Ok (Timestamp micros))))` with:

```lune
Task (IO.andThen prim_timeNowMicros (\micros -> IO.pure (Ok (Timestamp micros))))
```

**Step 3: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add prelude/Lune/Time.lune
git commit -m "refactor(Time): use IO.pure/andThen instead of prim_* functions"
```

---

### Task 6: Update Lune.Atomic to use IO.pure and IO.andThen

**Files:**
- Modify: `prelude/Lune/Atomic.lune`

**Step 1: Add IO import**

```lune
import Lune.IO as IO
```

**Step 2: Replace primitive calls**

Line 29: Replace `Task (prim_ioBind (prim_atomically ma) (\a -> prim_ioPure (Ok a)))` with:

```lune
Task (IO.andThen (prim_atomically ma) (\a -> IO.pure (Ok a)))
```

**Step 3: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add prelude/Lune/Atomic.lune
git commit -m "refactor(Atomic): use IO.pure/andThen instead of prim_* functions"
```

---

### Task 7: Update Lune.Fiber to use IO.pure and IO.andThen

**Files:**
- Modify: `prelude/Lune/Fiber.lune`

**Step 1: Add IO import**

```lune
import Lune.IO as IO
```

**Step 2: Replace primitive calls**

- Line 23: Replace `prim_ioBind ... prim_ioPure` pattern with `IO.andThen ... IO.pure`
- Line 31: Same replacement

**Step 3: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add prelude/Lune/Fiber.lune
git commit -m "refactor(Fiber): use IO.pure/andThen instead of prim_* functions"
```

---

### Task 8: Update Lune.IO internal implementations

**Files:**
- Modify: `prelude/Lune/IO.lune`

**Step 1: Update internal implementations to use the new functions**

Replace the implementations to use `IO.pure` and `IO.andThen` internally:

```lune
println : String -> Task e Unit
println s =
  Task (IO.andThen (prim_putStrLn s) (\u -> IO.pure (Ok u)))

readLine : Task e String
readLine =
  Task (IO.andThen prim_readLine (\s -> IO.pure (Ok s)))

readInt : Task e Int
readInt =
  Task (IO.andThen prim_readInt (\n -> IO.pure (Ok n)))

sleepMs : Int -> Task e Unit
sleepMs ms =
  Task (IO.andThen (prim_sleepMs ms) (\u -> IO.pure (Ok u)))
```

**Step 2: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add prelude/Lune/IO.lune
git commit -m "refactor(IO): use IO.pure/andThen internally"
```

---

### Task 9: Update Lune.Task internal implementations

**Files:**
- Modify: `prelude/Lune/Task.lune`

**Step 1: Add IO import**

After line 19 (after the Prelude import):

```lune
import Lune.IO as IO
```

**Step 2: Replace all prim_* calls with IO.pure/IO.andThen**

Update all function implementations:

```lune
succeed : a -> Task e a
succeed a =
  Task (IO.pure (Ok a))

fail : e -> Task e a
fail e =
  Task (IO.pure (Err e))

fromIO : IO a -> Task e a
fromIO io =
  Task (IO.andThen io (\a -> IO.pure (Ok a)))

map : (a -> b) -> Task e a -> Task e b
map f task =
  case task of
    Task io ->
      Task
        ( IO.andThen io (\r ->
            case r of
              Err e -> IO.pure (Err e)
              Ok a -> IO.pure (Ok (f a))
          )
        )

andThen : Task e a -> (a -> Task e b) -> Task e b
andThen task k =
  case task of
    Task io ->
      Task
        ( IO.andThen io (\r ->
            case r of
              Err e -> IO.pure (Err e)
              Ok a ->
                case k a of
                  Task io2 -> io2
          )
        )

mapError : (e -> e2) -> Task e a -> Task e2 a
mapError f task =
  case task of
    Task io ->
      Task
        ( IO.andThen io (\r ->
            case r of
              Err e -> IO.pure (Err (f e))
              Ok a -> IO.pure (Ok a)
          )
        )

onError : Task e a -> (e -> Task e2 a) -> Task e2 a
onError task handler =
  case task of
    Task io ->
      Task
        ( IO.andThen io (\r ->
            case r of
              Ok a -> IO.pure (Ok a)
              Err e ->
                case handler e of
                  Task io2 -> io2
          )
        )
```

**Step 3: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add prelude/Lune/Task.lune
git commit -m "refactor(Task): use IO.pure/andThen instead of prim_* functions"
```

---

### Task 10: Update examples/45_NovaV2.lune

**Files:**
- Modify: `examples/45_NovaV2.lune`

**Step 1: Add IO import**

After line 4 (after Task import):

```lune
import Lune.IO as IO
```

**Step 2: Replace prim_ioPure calls in mockCall**

Replace lines 42-63:

```lune
mockCall _ model prompt =
  case model of
    "echo" ->
      IO.pure
        <| Ok
          { text = prompt
          , usage = Just
              { inputTokens = 0, outputTokens = 0, totalTokens = 0 }
          }
    "json" ->
      IO.pure
        <| Ok
          { text = "{\"n\":1}"
          , usage = Just
              { inputTokens = 0, outputTokens = 0, totalTokens = 0 }
          }
    "tool" ->
      IO.pure
        <| Ok
          { text = "ok\n{\"n\":2}\nbye"
          , usage = Just
              { inputTokens = 0, outputTokens = 0, totalTokens = 0 }
          }
    _ ->
      IO.pure (Err (ProviderError "Unknown model"))
```

**Step 3: Replace toResult with Task.result**

Remove lines 170-173 (the local toResult function).

Replace usages:
- Line 161: `toResult` → `Task.result`
- Line 166: `toResult` → `Task.result`

**Step 4: Run the example to verify**

Run: `cabal run lune -- examples/45_NovaV2.lune`
Expected: Same output as before the refactor

**Step 5: Commit**

```bash
git add examples/45_NovaV2.lune
git commit -m "refactor(examples): use IO.pure and Task.result instead of prim_* functions"
```

---

### Task 11: Final verification

**Step 1: Run full build**

Run: `cabal build`
Expected: Build succeeds with no warnings

**Step 2: Grep for remaining prim_* in user-facing code**

Run: `grep -r "prim_ioPure\|prim_ioBind" examples/`
Expected: No matches

Run: `grep -r "prim_ioPure\|prim_ioBind" prelude/`
Expected: Only in Lune/IO.lune (the two wrapper definitions)

**Step 3: Run all examples that use Task/IO**

Run: `cabal run lune -- examples/45_NovaV2.lune`
Expected: Output matches expected behavior

**Step 4: Final commit if any cleanup needed**

If clean, no commit needed.

---

## Summary

After completing all tasks:
- `IO.pure` wraps `prim_ioPure`
- `IO.andThen` wraps `prim_ioBind`
- `Task.result` converts `Task e a -> Task x (Result e a)`
- All prelude modules use the new API internally
- Examples no longer expose `prim_*` functions
- The only place `prim_ioPure`/`prim_ioBind` appear is in `Lune/IO.lune` definitions
