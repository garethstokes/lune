# Nova Tool Execution — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `Nova/Exec.lune` — a process-backed execution primitive that runs a shell command under `RuntimeConfig` guards + an injectable approval gate and formats the result back as a conversation turn — closing the Nova tool loop.

**Architecture:** A new `Nova/Exec.lune` isolates the `Lune.Process` dependency. `runCommand` runs `sh -c "<command>"` via `Lune.Process.runCapture` (non-zero exit is data, only launch/IO errors fail). `executeApproved` wraps it in the existing allow/maxTurns guard + an injectable `Approve` callback + `withTimeout`. `toolResultMessage` renders a result into an `n4` `User` message. Golden-test driven via a deterministic `echo`/`exit` example.

**Tech Stack:** Lune (compiler in Haskell), `cabal test golden` inside the Nix dev shell. **Spec:** `docs/superpowers/specs/2026-06-07-nova-tool-execution-design.md`. **Branch:** `feat/nova-tool-execution`.

---

## Conventions

- All commands run inside the Nix dev shell: `nix develop --command bash -c '...'` (`cabal update` already done).
- Build: `cabal build all -v0`. Suite: `cabal test golden`. Baseline on this branch: **All 225 tests passed**. Never regress it.
- Run/iterate an example: `cabal run -v0 lune -- --eval examples/49_Nova_RunCommand.lune`.
- Accept new goldens (after eyeballing the diff): `cabal test golden --test-options="--accept -p '/49_Nova_RunCommand/'"`.
- Editing a Nova prelude module mechanically re-churns the `core/*` golden dumps of dependent examples; accept the **Core** group when that happens. The binding invariant is that **existing eval goldens never change**.
- Shell hook `rtk` may garble `git`/`grep` output; re-run prefixed with `rtk proxy ` if so.

## Orientation (APIs you'll use — confirmed)

- `Lune.Process` exposes: `cmd : String -> Cmd`, `arg : String -> Cmd -> Cmd`, `runCapture : Cmd -> Task ProcessError { status : ExitStatus, stdout : Bytes, stderr : Bytes }`, `ExitStatus = Exited Int`, `ProcessError = NotFound String | PermissionDenied String | SpawnFailed String | IoError String | NonZeroExit ...`. `runCapture` returns the status even for non-zero exit (so non-zero exit is NOT a `ProcessError`).
- `Lune.Bytes.toString : Bytes -> String`.
- `Lune.String`: `Str.append`, `Str.fromInt : Int -> String` (used in `Nova/Core.lune` `mockOllama`).
- `Nova.Core` exposes `Message`, `user : String -> Message`, `Set`, `setMember : (a -> a -> Bool) -> a -> Set a -> Bool` (used in `Nova/Runtime.lune`).
- `Nova.Runtime` exposes `RuntimeConfig { maxTurns, toolTimeoutMs, allowedTools : Set ToolId, requireApproval : Set ToolId }`, `ToolExecError(..)` (`ToolNotAllowed`/`ApprovalRequired`/`ToolTimedOut`/`MaxTurnsExceeded`/`ToolNotImplemented`/`ToolFailed String`), and (after Task 1) `withTimeout : Int -> ToolId -> Task ToolExecError a -> Task ToolExecError a`.
- `Nova.Tool` exposes `ToolId`, `toolId : String -> String -> ToolId`, `eqToolId : ToolId -> ToolId -> Bool`.
- `prim_leInt : Int -> Int -> Bool` is a global builtin (used in `Runtime.guardTool`); usable in any module without import.
- `Task` helpers (see `prelude/Lune/Task.lune` and usages in `Nova.lune`/`50_HealthAgent.lune`): `Task.map`, `Task.andThen`, `Task.onError : Task e a -> (e -> Task f a) -> Task f a`, `Task.mapError`, `Task.succeed`, `Task.fail`.

## File structure

| File | Responsibility | Change |
|------|----------------|--------|
| `prelude/Nova/Runtime.lune` | guards/runtime | export `withTimeout` (add to `exposing`) |
| `prelude/Nova/Exec.lune` (new) | process-backed execution primitive | `CommandResult`, `ApprovalDecision`, `Approve`, `runCommand`, `executeApproved`, `toolResultMessage`, `processErrorToString` |
| `examples/49_Nova_RunCommand.lune` (new) | deterministic test harness | runs `echo`/`exit` through the primitive |
| `tests/golden/{parse,core,eval}/49_Nova_RunCommand.golden` (new) | golden output | accepted after review |

---

## Task 1: Export `withTimeout` from `Nova/Runtime.lune`

**Files:** Modify `prelude/Nova/Runtime.lune` (module `exposing` list only).

- [ ] **Step 1: Add `withTimeout` to the export list**

In `prelude/Nova/Runtime.lune`, the module header is:
```
module Nova.Runtime exposing (
  RuntimeConfig,
  ToolExecError(..),
  executeWithGuards,
  executeWithGuardsUsing
)
```
Add `withTimeout`:
```
module Nova.Runtime exposing (
  RuntimeConfig,
  ToolExecError(..),
  executeWithGuards,
  executeWithGuardsUsing,
  withTimeout
)
```
No other change — `withTimeout` is already defined in the module.

- [ ] **Step 2: Build + suite**

Run: `nix develop --command bash -c 'cabal build all -v0 2>&1 | tail -5 && cabal test golden 2>&1 | tail -6'`
Expected: clean build; `All 225 tests passed` (exporting a symbol changes no behavior; no golden should move).

- [ ] **Step 3: Commit**

```bash
git add prelude/Nova/Runtime.lune
git commit -m "feat(nova): export withTimeout from Nova.Runtime for the Exec module"
```

---

## Task 2: `Nova/Exec.lune` — `runCommand` + `toolResultMessage` (+ deterministic example)

**Files:** Create `prelude/Nova/Exec.lune`; create `examples/49_Nova_RunCommand.lune`; accept its goldens.

- [ ] **Step 1: Create `prelude/Nova/Exec.lune` with the types, `runCommand`, `processErrorToString`, `toolResultMessage`**

```
module Nova.Exec exposing (
  CommandResult,
  ApprovalDecision(..),
  Approve,
  runCommand,
  executeApproved,
  toolResultMessage
)

import Lune.Prelude exposing (Int, Bool(..), Result(..), Maybe(..), Unit, unit, String, Task(..), Applicative(..), Monad(..))
import Lune.Process as Process
import Lune.Bytes as Bytes
import Lune.String as Str
import Lune.Task as Task
import Nova.Core as Core exposing (Message, setMember)
import Nova.Runtime exposing (RuntimeConfig, ToolExecError(..), withTimeout)
import Nova.Tool exposing (ToolId, eqToolId)

type alias CommandResult =
  { exitCode : Int
  , stdout : String
  , stderr : String
  }

type ApprovalDecision
  = Approved
  | Denied

-- | Injectable approval gate. The interactive y/n prompt and test stubs both
-- inhabit this type. It is consulted only for tools in requireApproval.
type alias Approve =
  ToolId -> String -> Task Unit ApprovalDecision

processErrorToString : Process.ProcessError -> String
processErrorToString err =
  case err of
    Process.NotFound m ->
      Str.append "command not found: " m
    Process.PermissionDenied m ->
      Str.append "permission denied: " m
    Process.SpawnFailed m ->
      Str.append "spawn failed: " m
    Process.IoError m ->
      Str.append "io error: " m
    Process.NonZeroExit _ info ->
      Str.append "non-zero exit: " info.program

-- | Run a shell command string via `sh -c`, capturing stdout/stderr/exit.
-- A non-zero exit is returned as data (exitCode != 0); only a launch/IO
-- failure becomes ToolFailed.
runCommand : String -> Task ToolExecError CommandResult
runCommand command =
  let
    c = Process.arg command (Process.arg "-c" (Process.cmd "sh"))
    captured =
      Task.map
        (\cap ->
          case cap.status of
            Process.Exited code ->
              { exitCode = code
              , stdout = Bytes.toString cap.stdout
              , stderr = Bytes.toString cap.stderr
              })
        (Process.runCapture c)
  in
    Task.onError captured (\perr -> Task.fail (ToolFailed (processErrorToString perr)))

-- | Render a command + its result into a User message to feed back to the LLM.
toolResultMessage : String -> CommandResult -> Message
toolResultMessage command result =
  Core.user
    (Str.append "Command: "
      (Str.append command
        (Str.append "\nExit: "
          (Str.append (Str.fromInt result.exitCode)
            (Str.append "\nstdout:\n"
              (Str.append result.stdout
                (Str.append "\nstderr:\n" result.stderr)))))))
```
NOTE: `executeApproved` is added in Task 3 but it is in the `exposing` list now — Lune requires exported names to exist, so for THIS task either (a) omit `executeApproved` from `exposing` and add it in Task 3, or (b) add a stub. Prefer (a): in this task the `exposing` list is `(CommandResult, ApprovalDecision(..), Approve, runCommand, toolResultMessage)`; Task 3 adds `executeApproved`. (Adjust the literal above accordingly.)

VERIFY API names by reading the modules if the build complains: `Task.onError`/`Task.map` (`prelude/Lune/Task.lune`), `Str.fromInt` (`prelude/Lune/String.lune`), `Core.user` (`prelude/Nova/Core.lune` exposes `user`), `Process.runCapture`/`Process.Exited`/`Process.cmd`/`Process.arg` (`prelude/Lune/Process.lune`), `Bytes.toString`. If `runCapture`'s record field is accessed differently, match the actual definition. If a `Process.ProcessError` constructor name differs, fix `processErrorToString`.

- [ ] **Step 2: Build to confirm the module compiles**

Run: `nix develop --command bash -c 'cabal build all -v0 2>&1 | tail -8'`
Expected: clean build. (The module isn't used yet; fix any name/syntax errors against the real prelude APIs before moving on.)

- [ ] **Step 3 (RED): Write the example exercising `runCommand`/`toolResultMessage`**

Create `examples/49_Nova_RunCommand.lune`. Model imports/`main`-structure on `examples/41_Nova.lune` (which already converts a failable Task for a `main : Task Unit Unit`). `runCommand` returns `Task ToolExecError CommandResult`, so convert the error away with `Task.result` (used in `50_HealthAgent.lune` as `r <- Task.result System.inventory`; if `Task.result` isn't available, mirror `41_Nova.lune`'s `toResult` helper). Use ONLY deterministic commands.

Helpers (write these in the example):
- `describe : Result ToolExecError CommandResult -> String` — `Ok r` → `"ok exit=" ++ Str.fromInt r.exitCode ++ " stdout=" ++ r.stdout` ; `Err _` → `"err"`. (Compose with `Str.append`; Lune has no `++` — see `45_NovaV2`/`48_Nova_MultiTurn` for `Str.append` chains.) Do NOT escape newlines (no `Str.replace` dependency) — `echo hello`'s trailing `\n` will appear literally in the golden; that is fine and deterministic.
- `roleName : Message -> String` — case on `msg.role` → `"user"`/`"system"`/`"assistant"`. IMPORTANT (gotcha from the n4 work): qualified union constructors like `Core.User` do NOT resolve in patterns. Import the constructors unqualified — `import Nova.Core as Core exposing (Message, Role(..))` — and match on bare `User`/`System`/`Assistant`.

```
main : Task Unit Unit
main =
  do
    r1 <- Task.result (Exec.runCommand "echo hello")
    IO.println (Str.append "echo: " (describe r1))
    r2 <- Task.result (Exec.runCommand "exit 3")
    IO.println (Str.append "exit3: " (describe r2))
    let msg = Exec.toolResultMessage "echo hi" { exitCode = 0, stdout = "hi", stderr = "" }
    IO.println (Str.append "msg.role=" (roleName msg))
```

Run: `nix develop --command bash -c 'cabal run -v0 lune -- --eval examples/49_Nova_RunCommand.lune'`
Expected: it compiles and runs. The REQUIRED, deterministic facts in the output: `echo hello` gives `exit=0` with `hello` on stdout; `exit 3` gives `exit=3` returned as data (an `ok`, NOT an `err`); `msg.role=user`. (Exact line text depends on your `describe`; just keep it deterministic and one fact per line. Note `echo` appends a trailing newline to stdout, so the `echo:` line's golden will include that newline — expected.)

- [ ] **Step 4: Verify determinism, then accept goldens**

Run the example twice and confirm identical output:
`nix develop --command bash -c 'cabal run -v0 lune -- --eval examples/49_Nova_RunCommand.lune > /tmp/a.txt; cabal run -v0 lune -- --eval examples/49_Nova_RunCommand.lune > /tmp/b.txt; diff /tmp/a.txt /tmp/b.txt && echo IDENTICAL'`
Then accept: `nix develop --command bash -c "cabal test golden --test-options=\"--accept -p '/49_Nova_RunCommand/'\""` and `cat tests/golden/eval/49_Nova_RunCommand.golden` to eyeball it.

- [ ] **Step 5: Full suite**

Run: `nix develop --command bash -c 'cabal test golden 2>&1 | tail -6'`
Expected: `All 228 tests passed` (225 + 3 new goldens). Existing eval goldens unchanged.

- [ ] **Step 6: Commit**

```bash
git add prelude/Nova/Exec.lune examples/49_Nova_RunCommand.lune tests/golden/parse/49_Nova_RunCommand.golden tests/golden/core/49_Nova_RunCommand.golden tests/golden/eval/49_Nova_RunCommand.golden
git commit -m "feat(nova): runCommand + toolResultMessage in Nova.Exec (real subprocess execution)"
```

---

## Task 3: `executeApproved` — guards + injectable approval

**Files:** Modify `prelude/Nova/Exec.lune` (add `executeApproved` + export it); extend `examples/49_Nova_RunCommand.lune`; re-accept goldens.

- [ ] **Step 1: Add `executeApproved` and its allow/maxTurns guard helper**

In `prelude/Nova/Exec.lune`, add `executeApproved` to the `exposing` list, and define:
```
-- allow + maxTurns check ONLY (approval is handled separately so the callback decides)
guardAllow : RuntimeConfig -> ToolId -> Result ToolExecError Unit
guardAllow cfg toolId =
  case prim_leInt cfg.maxTurns 0 of
    True ->
      Err (MaxTurnsExceeded cfg.maxTurns)
    False ->
      case setMember eqToolId toolId cfg.allowedTools of
        False ->
          Err (ToolNotAllowed toolId)
        True ->
          Ok unit

executeApproved : RuntimeConfig -> Approve -> ToolId -> String -> Task ToolExecError CommandResult
executeApproved cfg approve toolId command =
  case guardAllow cfg toolId of
    Err e ->
      Task.fail e
    Ok _ ->
      case setMember eqToolId toolId cfg.requireApproval of
        False ->
          withTimeout cfg.toolTimeoutMs toolId (runCommand command)
        True ->
          Task.andThen
            (Task.mapError (\_ -> ToolFailed "approval callback failed") (approve toolId command))
            (\decision ->
              case decision of
                Denied ->
                  Task.fail (ApprovalRequired toolId)
                Approved ->
                  withTimeout cfg.toolTimeoutMs toolId (runCommand command))
```
(`prim_leInt`, `setMember`, `eqToolId` are all in scope per the imports/builtins. If `Task.mapError`'s argument order differs, match `prelude/Lune/Task.lune`.)

- [ ] **Step 2: Build**

Run: `nix develop --command bash -c 'cabal build all -v0 2>&1 | tail -8'` → clean build.

- [ ] **Step 3 (RED→GREEN): Extend the example with approval/guard cases**

In `examples/49_Nova_RunCommand.lune`, add a `RuntimeConfig`, a `ToolId`, and two `Approve` stubs, and exercise the three guard outcomes. Append to `main`:
```
-- near the top-level of the example:
runTool : Nova.Tool.ToolId
runTool = Nova.Tool.toolId "run" "1"

cfg : Nova.Runtime.RuntimeConfig
cfg =
  { maxTurns = 5
  , toolTimeoutMs = 5000
  , allowedTools = Core.setFromList Nova.Tool.eqToolId [runTool]
  , requireApproval = Core.setFromList Nova.Tool.eqToolId [runTool]
  }

autoApprove : Exec.Approve
autoApprove _ _ = Task.succeed Exec.Approved

autoDeny : Exec.Approve
autoDeny _ _ = Task.succeed Exec.Denied

-- ... in main, after the Task 2 lines:
    a <- Task.result (Exec.executeApproved cfg autoApprove runTool "echo approved")
    IO.println (Str.append "approve: " (describe a))     -- ok exit=0 stdout=<approved\n>
    d <- Task.result (Exec.executeApproved cfg autoDeny runTool "echo denied")
    IO.println (Str.append "deny: " (describeErr d))      -- err ApprovalRequired
    let cfgNoTool = { cfg | allowedTools = Core.setEmpty }
    n <- Task.result (Exec.executeApproved cfgNoTool autoApprove runTool "echo nope")
    IO.println (Str.append "notallowed: " (describeErr n)) -- err ToolNotAllowed
```
Add `describeErr : Result ToolExecError CommandResult -> String` that renders the error variant name (e.g. matches `Err (ApprovalRequired _)` → `"err ApprovalRequired"`, `Err (ToolNotAllowed _)` → `"err ToolNotAllowed"`, `Ok _` → `"ok"`). Import `Nova.Runtime`/`Nova.Tool` as needed and `Core.setFromList`/`Core.setEmpty`. The deny case proves the command does NOT run (it returns `ApprovalRequired`, never an `echo` result).

Run: `nix develop --command bash -c 'cabal run -v0 lune -- --eval examples/49_Nova_RunCommand.lune'`
Expected (the new lines): `approve: ok exit=0 stdout=<approved\n> ...`, `deny: err ApprovalRequired`, `notallowed: err ToolNotAllowed`. Verify the deny case shows `ApprovalRequired` (NOT a command result) — that's the proof the gate blocks execution.

- [ ] **Step 4: Determinism + re-accept eval golden**

Run the example twice, `diff` for identical output (as in Task 2 Step 4). Then re-accept: `nix develop --command bash -c "cabal test golden --test-options=\"--accept -p '/49_Nova_RunCommand/'\""`; `cat tests/golden/eval/49_Nova_RunCommand.golden` to confirm all expected lines including the three guard outcomes.

- [ ] **Step 5: Full suite**

Run: `nix develop --command bash -c 'cabal test golden 2>&1 | tail -6'`
Expected: `All 228 tests passed`. Existing eval goldens unchanged.

- [ ] **Step 6: Commit**

```bash
git add prelude/Nova/Exec.lune examples/49_Nova_RunCommand.lune tests/golden/eval/49_Nova_RunCommand.golden tests/golden/core/49_Nova_RunCommand.golden tests/golden/parse/49_Nova_RunCommand.golden
git commit -m "feat(nova): executeApproved — guarded, approval-gated command execution"
```

---

## Done criteria

- `cabal test golden` green (228/228); existing eval goldens byte-identical.
- `Nova.Exec` exports `CommandResult`, `ApprovalDecision`, `Approve`, `runCommand`, `executeApproved`, `toolResultMessage`.
- `runCommand "echo hello"` returns `exitCode 0` + `hello` on stdout; `runCommand "exit 3"` returns `exitCode 3` as **data** (no Task failure).
- `executeApproved` runs an approved command, fails `ApprovalRequired` when the injected callback denies (command never runs), and fails `ToolNotAllowed` when the tool isn't allowed.
- `toolResultMessage` produces a `User` message with command + exit + output.
- The loop is closeable: a caller can `executeApproved` a proposed command and append `toolResultMessage` to the conversation for the next `toolMessages`/`askMessages` turn. (`n6` consumes this to de-mock HealthAgent.)
```
