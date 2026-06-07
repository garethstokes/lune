# Nova Tool Execution (close the loop)

**Date:** 2026-06-07 · **Status:** Approved (design) · **Beads:** `lune-n1` (blocks `lune-n6`/`n7`)
**Author:** Gareth Stokes (with Claude)

## Problem

The Nova audit (`docs/nova-audit.md`) found the framework's blocking gap: **Nova never executes
tools.** The guard machinery exists (`Nova/Runtime.lune`: `guardTool` enforces allowed-tools /
`maxTurns` / approval, `withTimeout` races a fiber) but its executor is caller-supplied and defaults
to `ToolNotImplemented`. `Lune.Process` (real subprocess execution) is imported by no Nova module.
The HealthAgent *proposes* commands and then just **appends them to a file, never running them**
(`50_HealthAgent.lune` `recordApprovedCommand`). With the message model (`n4`) now landed, the
remaining piece is the execution primitive that runs a proposed command for real, under the guards
and an approval gate, and formats the result back as a conversation turn.

## Goal & non-goals

**Goal:** a small, reusable, deterministically-testable **execution primitive** — run a shell
command via `Lune.Process` under `RuntimeConfig` guards + a mandatory injectable approval step, and
format the result as a `User` turn to feed back to the LLM.

**Non-goals (deferred):** a full autonomous `runAgent` loop (LLM↔tool↔repeat) — the caller/`n6`
drives the loop using this primitive; multi-tool dispatch / a tool registry; argv-splitting/sandbox
hardening beyond the approval gate; streaming command output; the `Tool` message role (results are
`User` messages, per `n4`).

## Design decisions (settled in brainstorming)

1. **Scope = execution primitive**, not an autonomous loop. `n6` wires it into HealthAgent.
2. **`sh -c "<command>"`** execution (arbitrary shell strings — flags/pipes/redirects), gated by a
   mandatory approval callback. The shell's power is exactly why approval is required.
3. **A non-zero exit is data, not an error** — returned as a `CommandResult` with `exitCode ≠ 0` so
   the LLM sees the failure output. Only genuine launch/IO failures become `ToolExecError`.
4. **Approval is a first-class injectable callback** — interactive y/n is one implementation; tests
   inject auto-approve/deny.

## Architecture

A new module **`prelude/Nova/Exec.lune`** that isolates the `Lune.Process` dependency from the rest
of Nova (Core/Runtime/Tool stay process-free). It depends on `Lune.Process`, `Lune.Bytes`,
`Nova.Core` (for `Message`/`user`), `Nova.Runtime` (`RuntimeConfig`, `ToolExecError`, `withTimeout`),
and `Nova.Tool` (`ToolId`).

### Types

```
type alias CommandResult =
  { exitCode : Int
  , stdout : String
  , stderr : String
  }

type ApprovalDecision = Approved | Denied

-- Injectable approval gate; interactive prompt and test stubs both inhabit this.
type alias Approve = ToolId -> String -> Task Unit ApprovalDecision
```

### Functions

```
runCommand : String -> Task ToolExecError CommandResult
executeApproved : RuntimeConfig -> Approve -> ToolId -> String -> Task ToolExecError CommandResult
toolResultMessage : String -> CommandResult -> Message
```

`Nova/Runtime.lune` change: export `withTimeout` (currently private) so `Exec` can reuse it. No
behavior change to `Runtime`.

## Component detail

### `runCommand`

Builds `cmd "sh" |> arg "-c" |> arg command`, runs via `Lune.Process.runCapture`, and maps:
- **Success** (`runCapture` returns `{ status = Exited code, stdout, stderr }`, including non-zero
  `code`): `Ok { exitCode = code, stdout = Bytes.toString stdout, stderr = Bytes.toString stderr }`.
  (`runCapture` returns the status even on non-zero exit — only real failures are `ProcessError`.)
- **`ProcessError`** (`NotFound`/`PermissionDenied`/`SpawnFailed`/`IoError`): `Task.fail (ToolFailed
  <human-readable message including the program and error>)`.

### `executeApproved`

```
executeApproved cfg approve toolId command:
  1. guard: reuse guardTool's allowed-tools + maxTurns checks.
     - tool not in allowedTools        -> Task.fail (ToolNotAllowed toolId)
     - maxTurns <= 0                    -> Task.fail (MaxTurnsExceeded cfg.maxTurns)
  2. approval: if toolId in cfg.requireApproval, call (approve toolId command):
     - Denied   -> Task.fail (ApprovalRequired toolId)   (command NEVER runs)
     - Approved -> continue
     (tools not in requireApproval run without prompting)
  3. execute: withTimeout cfg.toolTimeoutMs toolId (runCommand command)
```

Note `guardTool` currently returns `ApprovalRequired` itself for tools in `requireApproval`;
`executeApproved` must **not** use that branch as a hard deny — it splits the allow/maxTurns check
from approval so the injectable callback decides. (Implementation: either call `guardTool` and treat
its `ApprovalRequired` result as "needs approval → run the callback", or replicate the allow/maxTurns
checks directly. Either is fine; the observable contract is the flow above.)

### `toolResultMessage`

`toolResultMessage command result` returns a `Nova.Core.user` message with a deterministic,
LLM-readable rendering, e.g.:

```
Command: <command>
Exit: <exitCode>
stdout:
<stdout>
stderr:
<stderr>
```

The caller appends `assistant <llm reply>` + `toolResultMessage command result` to the conversation
and re-calls `toolMessages`/`askMessages` — closing the loop (per `n4`'s append-and-recall pattern).

## Data flow (caller / n6 perspective)

```
LLM proposes ProposedAction { command, ... }   (already decoded today, e.g. HealthAgent)
  -> executeApproved cfg approve runCommandToolId action.command
       guards -> approval(approve) -> runCommand (sh -c) under timeout
  -> CommandResult
  -> append [ assistant reply, toolResultMessage command result ] to messages
  -> toolMessages/askMessages client messages    (next turn)
```

## Error handling

`ToolExecError` (existing, `Nova/Runtime.lune`): `ToolNotAllowed`, `ApprovalRequired`,
`ToolTimedOut`, `MaxTurnsExceeded`, `ToolNotImplemented`, `ToolFailed String`. `runCommand` only
produces `ToolFailed` (launch/IO errors) — never for a non-zero exit. `executeApproved` adds
`ToolNotAllowed` / `MaxTurnsExceeded` / `ApprovalRequired` (denied) / `ToolTimedOut` (via
`withTimeout`).

## Testing (deterministic, real subprocesses, no LLM)

`Lune.Process` works under `--eval` (the existing `tests/process/*` golden tests run via the
interpreter). `sh` is present in the dev shell. All test commands are fully deterministic.

1. `runCommand "echo hello"` → `exitCode = 0`, `stdout` contains `hello`.
2. `runCommand "exit 3"` → `exitCode = 3` returned as data (the Task does **not** fail).
3. `executeApproved` with a `requireApproval` tool:
   - auto-`Denied` approve → fails `ApprovalRequired`, command does not run.
   - auto-`Approved` approve → returns the `CommandResult`.
   - tool not in `allowedTools` → fails `ToolNotAllowed`.
4. `toolResultMessage` produces a `User`-role message containing the command, exit code, and output.
5. **Example + golden:** `examples/49_Nova_RunCommand.lune` runs only deterministic commands
   (`echo` a fixed string; an `exit N`) through `runCommand`/`executeApproved` and prints the
   results, with an eval golden stable on any machine. (No dates/hostnames/PIDs in the output.)

The existing suite (225) must stay green; the example adds 3 goldens (parse/core/eval). Existing
eval goldens unchanged.

## Affected code

- `prelude/Nova/Exec.lune` — **new** module (`CommandResult`, `ApprovalDecision`, `Approve`,
  `runCommand`, `executeApproved`, `toolResultMessage`).
- `prelude/Nova/Runtime.lune` — export `withTimeout` (add to module `exposing`); no behavior change.
- `lune.cabal` — none (prelude modules aren't cabal-listed; they're loaded from `LUNE_PRELUDE_PATH`).
- `examples/49_Nova_RunCommand.lune` (new) + its `parse`/`core`/`eval` goldens.

## Risks

- **Running arbitrary shell commands is inherently dangerous.** Mitigation is the mandatory approval
  gate for `requireApproval` tools; the design documents that `runCommand` (un-guarded) is for
  callers who have already gated execution. `n6` must place the run-command tool in `requireApproval`.
- **`withTimeout` + real subprocess interaction** — `withTimeout` races a timeout fiber against the
  task; a timed-out command's process may outlive the race (the fiber doesn't kill it). Acceptable
  for this primitive (the audit's guard already worked this way); note as a follow-up if it bites.
- **Eval-golden determinism** — only `echo`/`exit` with fixed arguments; no environment-derived
  output. Verify the golden is byte-stable across two runs before accepting.
