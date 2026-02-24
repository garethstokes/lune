# Process Execution

This document describes Lune’s external process execution API and its runtime/scheduler semantics.

## Module

`prelude/Lune/Process.lune` exposes `Lune.Process`.

## Public API

### Command description

`Cmd` is a structured command (no shell parsing):

- `program : String`
- `args : List String`
- `cwd : Maybe String`
- `env : List (String, String)` (key/value pairs)
- `clearEnv : Bool`

Builder helpers (`cmd`, `arg`, `args`, `cwd`, `env`, `clearEnv`) construct `Cmd` values.

### Exit status policy

- `run : Cmd -> Task ProcessError ExitStatus` returns `Ok` even for non-zero exit codes.
- `runOk : Cmd -> Task ProcessError Unit` converts non-zero exits into `Err (NonZeroExit ...)`.

### Capture policy

- `runCapture` and `runInputCapture` return raw `Bytes` for stdout/stderr.
- No implicit text decoding is performed by default.

### Deadlock avoidance

`runCapture` MUST drain stdout and stderr concurrently to avoid deadlocks when a child fills one pipe.

The runtime implementation achieves this by draining both stdout and stderr on background threads and
buffering output for the Lune fiber(s) to consume.

### Streaming API

`spawn` starts a process with piped stdin/stdout/stderr and returns an opaque `Process` handle.

Streaming primitives:

- `writeStdin`, `closeStdin`
- `readStdout`, `readStderr` (incremental reads; `Nothing` means EOF)
- `wait`, `kill`

## Scoped process management

`withProcess : Cmd -> (Process -> Task ProcessError a) -> Task ProcessError a` is a scoped helper that:

- spawns the process
- runs the callback
- always performs cleanup after the callback completes (success or failure)

It is intended for streaming use-cases where you want explicit, bounded process lifetimes without
changing the behavior of `spawn`.

## Resource lifetime

`withProcess` guarantees:

- stdin is closed (idempotently)
- the process is not left running accidentally
- the process is waited/reaped so it does not become a zombie

Stdout/stderr reader threads are allowed to terminate naturally once the process exits and its pipes
close. `withProcess` does not implicitly read/drain stdout/stderr into user-visible memory.

## Termination policy

Cleanup uses a graceful-then-forceful policy:

1. If the process is still running, send `SigTerm`.
2. Wait up to 300ms for exit.
3. If it is still running, send `SigKill`.
4. `wait` to reap the process.

### Scheduler / blocking behavior

Any operation that could block an OS thread is implemented so that the *scheduler thread* is never
blocked. Blocking waits/reads/writes occur on background threads; Lune fibers yield while waiting
for results.

On the Haskell runtime this relies on the executable being built with GHC’s threaded RTS (we pass
`-threaded` in `lune.cabal`) so background threads can safely block on OS IO without freezing the
entire runtime.

## Platform notes

Tests use common POSIX tools (`sh`, `echo`, `cat`, `sleep`). On Windows, the test suite should skip
these cases unless a compatible POSIX layer is present.
