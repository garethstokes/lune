# De-mock the HealthAgent: real command execution + a deterministic loop test

**Date:** 2026-06-08 · **Status:** Approved (design) · **Beads:** `lune-n6` (substantially covers `n9`)
**Author:** Gareth Stokes (with Claude)

## Problem

`examples/50_HealthAgent.lune` is a working decision pipeline with a **stubbed actuation stage**:
it collects real `/proc` metrics, asks ollama (`qwen3`) for a `HealthAnalysis` with proposed
`actions`, prompts the user y/n/q per action — and then, on approval, just **appends the command to
`approved-commands.sh` and never runs it** (`recordApprovedCommand`). `lune-n1` shipped the missing
primitive (`Nova.Exec.runCommand`/`executeApproved`/`toolResultMessage`). This spec closes the loop:
the HealthAgent should actually **run** approved commands, and we add a deterministic, safe,
CI-testable demonstration of the propose→approve→execute path (the HealthAgent itself can't be
golden-tested — it's interactive and needs a live LLM).

## Goal & non-goals

**Goal:** (A) replace the HealthAgent's file-append execute stage with real, guarded,
approval-gated execution via `Nova.Exec.executeApproved`; (B) add a minimal, deterministic,
mock-backed example that proves the full propose→approve→execute→result loop in CI, safely.

**Non-goals (deferred):** feeding tool results back as further LLM turns (true multi-step agentic
loop); a fully mock-injectable HealthAgent CI golden beyond what (B) covers (remainder of `n9`);
any change to `Nova.Exec`/`Nova.Runtime` (use them as-is); fixing `Nova.Runtime.withTimeout` (that's
`lune-n10`).

## Design decisions (settled in brainstorming)

1. **Integration via `executeApproved`** (full guards), not a bare `runCommand`. The existing y/n
   prompt becomes the injected `Approve` callback so the `RuntimeConfig` guard machinery is genuinely
   exercised.
2. **Two-part scope:** de-mock the real (`50`, interactive + ollama, compile-tested) AND add a
   deterministic mock-backed test (`51`, CI golden, safe `echo`).
3. **Execute-and-show**, no feedback turn. Print the real `CommandResult`; don't re-ask the LLM.
4. **Use `Nova.Exec.executeApproved`** (its `Lune.Task.withTimeout` path), NOT
   `Nova.Runtime.executeWithGuardsUsing` (which deadlocks on subprocess tasks — `lune-n10`).

## Part A — de-mock `examples/50_HealthAgent.lune`

Today (`processActionsGo`, the `Approved` branch):
```
Approved ->
  do
    result <- recordApprovedCommand act.command   -- appends to approved-commands.sh
    IO.println result
    ...
```

New design:
- Add imports `Nova.Exec as Exec`, `Nova.Runtime as Runtime`, `Nova.Tool as Tool` and definitions:
  ```
  runTool : Tool.ToolId
  runTool = Tool.toolId "run" "1"

  runtimeConfig : Runtime.RuntimeConfig
  runtimeConfig =
    { maxTurns = 1
    , toolTimeoutMs = 30000
    , allowedTools = Core.setFromList Tool.eqToolId [runTool]
    , requireApproval = Core.setFromList Tool.eqToolId [runTool]
    }
  ```
- The y/n/q prompt keeps loop control (`q` quits the whole loop, as today). For a non-quit answer the
  loop calls `executeApproved` with an `Approve` callback derived from that answer:
  ```
  -- y -> run (callback approves); n -> skip (callback denies)
  approveFrom : ApprovalResult -> Exec.Approve
  approveFrom decision _ _ =
    case decision of
      Approved -> Task.succeed Exec.Approved
      _        -> Task.succeed Exec.Denied
  ```
  (`requireApproval` contains `runTool`, so `executeApproved` consults this callback; `n`/`Denied`
  yields `ApprovalRequired` and the command never runs — exactly the existing "skip" semantics, but
  now routed through the guard.)
- On approval, run and show the real result:
  ```
  Approved ->
    do
      outcome <- Task.result (Exec.executeApproved runtimeConfig (approveFrom Approved) runTool act.command)
      IO.println (showExecOutcome act.command outcome)   -- exit + stdout/stderr, or the error
      processActionsGo (Int.add current 1) total rest
  ```
  where `showExecOutcome : String -> Result Runtime.ToolExecError Exec.CommandResult -> String`
  renders `Ok cr` (exit code + stdout/stderr, e.g. via `Exec.toolResultMessage`'s content or a local
  render) and `Err e` (the `ToolExecError`).
- **Delete** `recordApprovedCommand`, `approvedCommandsPath`, and the `approved-commands.sh`
  references. Update the docstring's architecture bullet 4 from "Action Recording → approved commands
  are written to approved-commands.sh" to "Action Execution → approved commands run via Nova.Exec
  (human-gated)". Note in the docstring that this runs real commands after explicit per-command
  approval.

`50` remains interactive (`IO.readLine`) and ollama-backed, so it stays **compile-tested only**
(its `core`/`parse` goldens re-churn from the edit; accept them). Real verification is a manual
`ollama`-backed run.

## Part B — `examples/51_Nova_AgentLoop.lune` (new, deterministic)

A minimal self-contained agent proving the loop with a mock provider (wiring modeled on
`45_NovaV2.lune`):
- A mock provider whose model id (e.g. `"mock-action"`) returns a **fixed** action JSON proposing a
  safe command: `{"command":"echo agent-ran"}`.
- A tiny local `type alias Action = { command : String }` + a decoder.
- `main`: register the mock provider → `tool client prompt` (decodes to `Action`) → an
  `autoApprove : Exec.Approve` that returns `Approved` → `Exec.executeApproved cfg autoApprove
  runTool action.command` → print the `CommandResult` (and/or `Exec.toolResultMessage`).
- A `RuntimeConfig` with `runTool` in `allowedTools` and `requireApproval`.

Fully deterministic: `echo agent-ran` → `exit 0`, stdout `agent-ran`; safe; no live LLM. Produces a
stable eval golden. This substantially covers `n9` (a CI-exercised agent execute loop).

## Data flow

```
50 (real):   /proc -> ollama qwen3 (tool) -> [actions] -> per action: y/n/q
               -> executeApproved (guard + y/n-as-Approve + sh -c) -> show CommandResult
51 (test):   mock provider -> tool -> Action{command:"echo agent-ran"}
               -> autoApprove -> executeApproved -> CommandResult -> toolResultMessage -> golden
```

## Error handling

`executeApproved` surfaces `ToolExecError` (`ToolNotAllowed`/`ApprovalRequired`/`ToolTimedOut`/
`ToolFailed`). In `50`, `Task.result` converts it; `showExecOutcome` renders both the `Ok` result and
each error variant; the loop continues to the next action. `50`'s existing `AgentError` flow is
unchanged. `51` only hits the `Ok`/`Approved` path (deterministic).

## Testing

1. **`51` eval golden** (the CI proof) — deterministic output showing the proposed command ran:
   exit 0 + `agent-ran` on stdout via `toolResultMessage`. Verify byte-stable across two runs before
   accepting.
2. **`50`** — compile-tested only (`core`/`parse` goldens re-churn from the source edit; accept after
   confirming the diff is just the execute-stage rewrite). No eval golden (interactive + live LLM).
3. Existing suite stays green (228 → **231**: `51` adds parse/core/eval). Existing eval goldens
   unchanged.

## Affected code

- `examples/50_HealthAgent.lune` — execute stage rewired to `executeApproved`; delete
  `recordApprovedCommand`/`approvedCommandsPath`; docstring update.
- `examples/51_Nova_AgentLoop.lune` (new) + `tests/golden/{parse,core,eval}/51_Nova_AgentLoop.golden`.
- `tests/golden/{core,parse}/50_HealthAgent.golden` — re-accepted (mechanical churn from the edit).

## Risks

- **`50` runs real LLM-proposed shell commands.** Mitigation is the mandatory per-command human
  approval (the user sees the command + risk and types `y`) — the designed human-in-the-loop safety
  model. Documented in the docstring.
- **`51` determinism** — only a fixed mock action + `echo`; no env-derived output. Verify byte-stable.
- **y/n → `Approve` mapping** must preserve the existing "n = skip, q = quit" semantics; `n` now
  yields `ApprovalRequired` (command not run) which `showExecOutcome` renders as a skip-equivalent.
- **`50` core golden churn** is large (373KB IR dump); confirm the source diff is only the execute
  rewrite before accepting.
- **Constructor name collision:** the HealthAgent's local `ApprovalResult` has an `Approved`
  constructor and `Nova.Exec.ApprovalDecision` also has `Approved`. To avoid ambiguity, do NOT import
  `Nova.Exec`'s `ApprovalDecision(..)` unqualified in `50`; reference `Exec.Approved`/`Exec.Denied`
  qualified in value position (qualified constructors resolve in value position; the known Lune
  gotcha is only about patterns). Pattern-match on the local unqualified `Approved`/`Skipped`/`Quit`.
