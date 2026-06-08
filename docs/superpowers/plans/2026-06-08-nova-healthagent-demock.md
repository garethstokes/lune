# De-mock HealthAgent Execution — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the Nova agent actually run approved commands — a deterministic mock-backed loop example (`51`), and the real HealthAgent (`50`) executing approved commands via `Nova.Exec` instead of appending them to a file.

**Architecture:** `Nova.Exec.executeApproved` (from `n1`) is the execution primitive. `51_Nova_AgentLoop` is a self-contained, deterministic, mock-provider example proving propose→approve→execute→result (CI golden). `50_HealthAgent`'s approval loop replaces its file-append with `executeApproved`, gated by the existing human y/n prompt.

**Tech Stack:** Lune (compiler in Haskell), `cabal test golden` in the Nix dev shell. **Spec:** `docs/superpowers/specs/2026-06-08-nova-healthagent-demock-design.md`. **Branch:** `feat/nova-healthagent-demock`.

---

## Conventions

- All commands run in the Nix dev shell: `nix develop --command bash -c '...'` (`cabal update` done).
- Build: `cabal build all -v0`. Suite: `cabal test golden`. Baseline on this branch: **All 228 tests passed**. Never regress it.
- Run an example: `cabal run -v0 lune -- --eval examples/51_Nova_AgentLoop.lune`.
- Accept goldens (TASTY QUOTING GOTCHA — NO inner single quotes around the pattern): `nix develop --command bash -c 'cabal test golden --test-options="--accept -p /51_Nova_AgentLoop/"'`. For the Core group (existing-example churn) use `--accept` with NO `-p` (full accept; only the actually-differing goldens change) — confirm scope with `git status` first.
- BINDING INVARIANT: existing EVAL goldens stay byte-identical (`rtk proxy git status -s tests/golden/eval` shows only NEW `51` after Task 1, nothing after Task 2). CORE/PARSE goldens for `50_HealthAgent` will churn in Task 2 (it's edited) — that's expected; accept after confirming the source diff is just the execute rewrite.
- Shell hook `rtk` may garble `git`/`grep`; re-run prefixed with `rtk proxy ` if so.

## Orientation (confirmed)

- `Nova.Exec` (from `prelude/Nova/Exec.lune`): `CommandResult = { exitCode, stdout, stderr }`, `ApprovalDecision = Approved | Denied`, `Approve = ToolId -> String -> Task Unit ApprovalDecision`, `runCommand`, `executeApproved : RuntimeConfig -> Approve -> ToolId -> String -> Task ToolExecError CommandResult`, `toolResultMessage : String -> CommandResult -> Message`. `executeApproved` consults the `Approve` callback ONLY for tools in `cfg.requireApproval`; tools merely in `allowedTools` run without the callback.
- `Nova.Runtime`: `RuntimeConfig = { maxTurns : Int, toolTimeoutMs : Int, allowedTools : Set ToolId, requireApproval : Set ToolId }`, `ToolExecError(..)`.
- `Nova.Tool`: `ToolId`, `toolId : String -> String -> ToolId`, `eqToolId`.
- `Nova.Core`: `Provider`, `ProviderImpl`, `Message`, `Response`, `Error`, `provider : String -> Provider`, `dictInsert`, `defaultProviderRegistry`, `setEmpty`, `setFromList`, `eqToolId`(via Tool). `Nova` exposes `newClient`, `provider`, `providerEq`, `tool`, `showError`, `Client`.
- Mock-provider wiring pattern (from `examples/45_NovaV2.lune`): `mockProvider = provider "mock"`; `mockCall _cfg _model _messages = IO.pure (Ok { text = "...", usage = Nothing })`; `mockProviderImpl = { id = mockProvider, call = mockCall, capabilities = Core.setEmpty }`; in `main`: `let providers = Core.dictInsert Nova.providerEq mockProvider mockProviderImpl Core.defaultProviderRegistry` then `newClient providers mockProvider "<model>"`.
- `tool : Json.Decode a => Client -> String -> Task Error a` — pin `a` with a typed helper to avoid inference ambiguity (see Task 1).
- `@derive(Json)` on a record alias generates its `Json.Decode`/`Encode` instances (see `ProposedAction` in `50_HealthAgent.lune`).

## File structure

| File | Responsibility | Change |
|------|----------------|--------|
| `examples/51_Nova_AgentLoop.lune` (new) | deterministic loop proof | mock provider → tool → executeApproved → result |
| `tests/golden/{parse,core,eval}/51_Nova_AgentLoop.golden` (new) | golden | accepted |
| `examples/50_HealthAgent.lune` | real agent | execute stage → `executeApproved`; delete file-append; docstring |
| `tests/golden/{core,parse}/50_HealthAgent.golden` | golden | re-accepted (mechanical churn) |

---

## Task 1: `examples/51_Nova_AgentLoop.lune` — deterministic loop proof

**Files:** Create `examples/51_Nova_AgentLoop.lune`; accept its goldens.

- [ ] **Step 1: Write the example**

Create `examples/51_Nova_AgentLoop.lune`. Read `examples/45_NovaV2.lune` for the exact mock-provider import/wiring idioms and mirror them. Use a safe, fixed command.

```
module Main exposing (main)

import Lune.IO as IO
import Lune.Task as Task
import Lune.String as Str
import Lune.Json as RawJson exposing (Json)
import Nova exposing (newClient, provider, providerEq, tool, showError, Client)
import Nova.Core as Core exposing (Provider, ProviderImpl)
import Nova.Tool as Tool
import Nova.Runtime as Runtime exposing (RuntimeConfig)
import Nova.Exec as Exec

@derive(Json)
type alias Action =
  { command : String }

mockProvider : Provider
mockProvider = provider "mock"

-- no explicit type signature (matches 45_NovaV2's inferred mockCall, so it
-- unifies with ProviderImpl.call's concrete type)
mockCall _cfg _model _messages =
  IO.pure (Ok { text = "{\"command\":\"echo agent-ran\"}", usage = Nothing })

mockProviderImpl : ProviderImpl
mockProviderImpl =
  { id = mockProvider, call = mockCall, capabilities = Core.setEmpty }

runTool : Tool.ToolId
runTool = Tool.toolId "run" "1"

cfg : RuntimeConfig
cfg =
  { maxTurns = 1
  , toolTimeoutMs = 30000
  , allowedTools = Core.setFromList Tool.eqToolId [runTool]
  , requireApproval = Core.setFromList Tool.eqToolId [runTool]
  }

autoApprove : Exec.Approve
autoApprove _ _ = Task.succeed Exec.Approved

proposeAction : Client -> Task Core.Error Action
proposeAction client = tool client "propose a remediation command"

main : Task Unit Unit
main =
  do
    let providers =
      Core.dictInsert providerEq mockProvider mockProviderImpl Core.defaultProviderRegistry
    let client = newClient providers mockProvider "mock-action"
    actionResult <- Task.result (proposeAction client)
    case actionResult of
      Err e ->
        IO.println (Str.append "analysis error: " (showError e))
      Ok action ->
        do
          IO.println (Str.append "proposed: " action.command)
          execResult <- Task.result (Exec.executeApproved cfg autoApprove runTool action.command)
          case execResult of
            Err _ ->
              IO.println "execution error"
            Ok cr ->
              IO.println (Exec.toolResultMessage action.command cr).content
```
VERIFY against the real modules / `45_NovaV2.lune` (fix if names differ): `Core.Response`/`Core.Error` (the mock `call` return type — match `ProviderImpl.call`'s signature exactly, including `Maybe` for `usage = Nothing`; import `Maybe(..)` / `Result(..)` from `Lune.Prelude` if needed); `providerEq` (qualified `Nova.providerEq` vs exposed); `Core.dictInsert`/`Core.defaultProviderRegistry`/`Core.setEmpty`/`Core.setFromList`; that a `Message`'s `.content` field is accessed as `(expr).content` (or bind to a `let` first if the postfix-access parses awkwardly). If `@derive(Json)` needs an explicit import/pragma, copy how `50_HealthAgent.lune` declares its `@derive(Json)` types.

- [ ] **Step 2: Build**

Run: `nix develop --command bash -c 'cabal build all -v0 2>&1 | tail -8'` → clean build. Fix name/syntax errors against the real APIs.

- [ ] **Step 3: Run it; verify the command actually ran**

Run: `nix develop --command bash -c 'cabal run -v0 lune -- --eval examples/51_Nova_AgentLoop.lune'`
Expected (the proof — a real `echo` ran inside the agent loop):
```
proposed: echo agent-ran
Command: echo agent-ran
Exit: 0
stdout:
agent-ran
stderr:
```
The REQUIRED facts: `proposed: echo agent-ran` (decoded from the mock), then the `toolResultMessage` content showing `Exit: 0` and `agent-ran` on stdout (the command executed). If you see an error instead, the loop isn't wired right — fix before accepting.

- [ ] **Step 4: Determinism + accept goldens**

Run twice and diff for identical output:
`nix develop --command bash -c 'cabal run -v0 lune -- --eval examples/51_Nova_AgentLoop.lune > /tmp/a; cabal run -v0 lune -- --eval examples/51_Nova_AgentLoop.lune > /tmp/b; diff /tmp/a /tmp/b && echo IDENTICAL'`
Accept: `nix develop --command bash -c 'cabal test golden --test-options="--accept -p /51_Nova_AgentLoop/"'`; `cat tests/golden/eval/51_Nova_AgentLoop.golden` to eyeball.

- [ ] **Step 5: Full suite**

Run: `nix develop --command bash -c 'cabal test golden 2>&1 | tail -6'`
Expected: `All 231 tests passed` (228 + 3 new). `rtk proxy git status -s tests/golden/eval` shows ONLY the new `51` golden (existing eval goldens untouched).

- [ ] **Step 6: Commit**

```bash
git add examples/51_Nova_AgentLoop.lune tests/golden/parse/51_Nova_AgentLoop.golden tests/golden/core/51_Nova_AgentLoop.golden tests/golden/eval/51_Nova_AgentLoop.golden
git commit -m "test(nova): deterministic agent loop example (propose->approve->execute->result)"
```

---

## Task 2: De-mock `examples/50_HealthAgent.lune` execution

**Files:** Modify `examples/50_HealthAgent.lune`; re-accept its `core`/`parse` goldens.

- [ ] **Step 1: Add imports + runtime config + the approval-gated executor**

In `examples/50_HealthAgent.lune`:
- Add imports near the existing Nova import:
  ```
  import Nova.Exec as Exec
  import Nova.Runtime as Runtime exposing (RuntimeConfig)
  import Nova.Tool as Tool
  ```
- Add top-level definitions (near `makeClient`):
  ```
  runTool : Tool.ToolId
  runTool = Tool.toolId "run" "1"

  runtimeConfig : RuntimeConfig
  runtimeConfig =
    { maxTurns = 1
    , toolTimeoutMs = 30000
    , allowedTools = Core.setFromList Tool.eqToolId [runTool]
    , requireApproval = Core.setFromList Tool.eqToolId [runTool]
    }

  -- The human y/n at the prompt is the approval decision. runTool is in
  -- requireApproval, so executeApproved consults this callback (the gate fires);
  -- it relays the human's already-given approval (the Approved branch only runs
  -- when the user pressed y), so the command executes.
  loopApprove : Exec.Approve
  loopApprove _ _ = Task.succeed Exec.Approved

  showExecOutcome : String -> Result Runtime.ToolExecError Exec.CommandResult -> String
  showExecOutcome command outcome =
    case outcome of
      Ok cr ->
        (Exec.toolResultMessage command cr).content
      Err err ->
        Str.append "Execution failed: " (showToolExecError err)

  showToolExecError : Runtime.ToolExecError -> String
  showToolExecError err =
    case err of
      ToolNotAllowed _ -> "tool not allowed"
      ApprovalRequired _ -> "approval required"
      ToolTimedOut _ -> "timed out"
      MaxTurnsExceeded _ -> "max turns exceeded"
      ToolNotImplemented _ -> "not implemented"
      ToolFailed m -> Str.append "failed: " m
  ```
  NOTE: `ToolExecError(..)` constructors — import them so the patterns resolve: change the Runtime import to `import Nova.Runtime as Runtime exposing (RuntimeConfig, ToolExecError(..))` and match bare `ToolNotAllowed`/etc (qualified constructors don't resolve in patterns). `Core.setEmpty`/`Core.setFromList`/`Tool.eqToolId` are reachable via the existing `Nova.Core as Core` import (add `Tool`/`Runtime`/`Exec` imports as above).
  CONSTRUCTOR COLLISION: the local `ApprovalResult` has `Approved`; `Exec.ApprovalDecision` also has `Approved`. Reference `Exec.Approved` qualified in value position (as in `loopApprove`); never import `Exec`'s `ApprovalDecision(..)` unqualified. The loop's `case approval of Approved -> ...` still matches the local `ApprovalResult.Approved`.

- [ ] **Step 2: Rewrite the `Approved` branch of `processActionsGo` to execute**

Replace the existing `Approved ->` branch (which calls `recordApprovedCommand`):
```
          Approved ->
            do
              IO.println (render "Running: ${act.command}")
              outcome <- Task.result (Exec.executeApproved runtimeConfig loopApprove runTool act.command)
              IO.println (showExecOutcome act.command outcome)
              processActionsGo (Int.add current 1) total rest
```
Leave `Quit` and `Skipped` branches unchanged.

- [ ] **Step 3: Delete the file-append machinery + update the prompt + docstring**

- Delete `recordApprovedCommand` and `approvedCommandsPath` (and any now-unused `IO.writeFile`/`IO.readFile`-for-recording references they introduced — leave `collectHealthReport`'s reads alone).
- In `promptForApproval`, change the prompt text from `"Approve this action (record command to approved-commands.sh)? [y/n/q] "` to `"Approve this action (RUN this command)? [y/n/q] "`.
- In the module docstring, change architecture bullet 4 from `Action Recording - Approved commands are written to approved-commands.sh` to `Action Execution - Approved commands are run for real via Nova.Exec (human-gated)`, and drop the `approved-commands.sh` mention.

- [ ] **Step 4: Build**

Run: `nix develop --command bash -c 'cabal build all -v0 2>&1 | tail -10'` → clean build. (If `recordApprovedCommand` deletion leaves an unused import like `Lune.Json.Encode`, leave imports that are still used by other parts; only remove genuinely-unused ones the compiler flags.)

- [ ] **Step 5: Confirm the source diff is just the execute rewrite, then accept churned goldens**

`50_HealthAgent` has no eval golden (interactive + live ollama); its `core`/`parse` goldens churn from the edit.
- `rtk proxy git diff examples/50_HealthAgent.lune` — confirm the changes are only: the new imports/defs, the `Approved`-branch rewrite, the deleted file-append fns, and the prompt/docstring text. No unrelated logic touched.
- Run the suite: `nix develop --command bash -c 'cabal test golden 2>&1 | tail -8'`. The `50_HealthAgent` `Core`/`Parse` tests will FAIL (churn). Accept ONLY those: `rtk proxy git status -s tests/golden` first (expect only `core/50_HealthAgent.golden` and `parse/50_HealthAgent.golden` differing), then `nix develop --command bash -c 'cabal test golden --test-options="--accept"'` (full accept is safe — only the differing 50 goldens change; everything else, incl. the new 51 eval golden and all existing eval goldens, is byte-identical so unchanged).
- Re-confirm: `rtk proxy git status -s tests/golden` shows ONLY `core/50_HealthAgent.golden` + `parse/50_HealthAgent.golden` modified. If any EVAL golden or any OTHER example's golden changed, STOP and investigate.

- [ ] **Step 6: Full suite green**

Run: `nix develop --command bash -c 'cabal test golden 2>&1 | tail -6'`
Expected: `All 231 tests passed`. Existing eval goldens + the new `51` eval golden unchanged.

- [ ] **Step 7: Commit**

```bash
git add examples/50_HealthAgent.lune tests/golden/core/50_HealthAgent.golden tests/golden/parse/50_HealthAgent.golden
git commit -m "feat(nova): HealthAgent runs approved commands via Nova.Exec (de-mock execute stage)"
```

---

## Done criteria

- `cabal test golden` green (231/231); existing eval goldens byte-identical.
- `examples/51_Nova_AgentLoop.lune` deterministically proves propose→approve→**execute**→result (mock LLM proposes `echo agent-ran`; it runs; golden shows exit 0 + `agent-ran`).
- `examples/50_HealthAgent.lune` runs approved commands for real via `Exec.executeApproved` (human y/n gate); `recordApprovedCommand`/`approved-commands.sh` are gone; docstring updated.
- The flagship agent now *acts* instead of writing commands to a file; the loop is exercised in CI via `51` (substantially covering `n9`).
```
