# Go Runtime Target + Multi-Runtime Architecture

> **Status:** DRAFT  
> **Date:** 2026-02-12

## Overview

Add a new compilation target that **generates Go source** and builds it with `go build`.

This target is intended to be a pragmatic “native” runtime for Lune:

- Strong platform tooling (cross-compile, profiling, single binary output)
- GC + scheduler “for free”
- Great stdlib for networking/TLS/HTTP
- A straightforward place to host Lune’s runtime primitives

This doc also proposes a small refactor so Lune can target **multiple runtimes/backends** cleanly (interpreter, C, Go, LLVM later).

## Goals

- Add `--target go` (name TBD) that emits a runnable Go program.
- Implement Lune’s `Atomic/Shared` (STM) semantics in Go using `github.com/anacrolix/stm`.
- Keep the **primitive ABI** stable so multiple runtimes can coexist.
- Make it easy to add future targets (C+runtime, LLVM, WASM, etc.).

## Non-Goals (initially)

- Full performance parity with the evaluator (this is a correctness-first backend).
- `foreign import ccall` support on Go target (cgo is possible later, but not v0).
- Zero allocations / unboxed everything.
- Whole-program monomorphization (Go generics won’t cover Lune’s type system well).

---

## Multi-Runtime Model

### Terminology

- **Frontend**: Parse → desugar → resolve → typecheck → elaborate typeclasses.
- **Core IR**: `src/Lune/Core.hs` (strict, small, explicit `case`, records, dictionaries).
- **Target/backend**: consumes Core IR and produces artifacts (executable / source bundle).
- **Runtime**: per-target implementation of *primitives* + value representation needed by generated code.

### Stable “Primitive ABI”

The surface prelude (e.g. `prelude/Lune/IO.lune`, `prelude/Lune/Atomic.lune`) is just a thin layer over runtime-provided `prim_*` hooks.

Authoritative list of primitive names + types lives in:

- `src/Lune/Builtins.hs` (types in `builtinTypes`, and evaluator impls in `builtinEvalPrims`)

**Rule:** Every target must either:

1. Implement a primitive with the same meaning, or
2. Reject compilation with a clear “unsupported primitive on this target” error.

This lets us evolve features without coupling everything to one runtime.

### Backend Capability Gating

Add a minimal “capability” set per backend, e.g.:

- `SupportsSTM`
- `SupportsTasks`
- `SupportsSockets`
- `SupportsTls`
- `SupportsFFI(CCall)`

Frontend can keep producing Core, but codegen should fail early if Core contains unsupported primitives/features.

---

## Go Target: High-Level Compilation Plan

### Pipeline

1. Frontend produces Core.
2. Apply backend-normalization passes (recommended):
   - alpha-rename to unique binders
   - lambda-parameter pattern compilation (if not already done)
   - ANF (administrative normal form) to simplify codegen
3. Emit a Go module directory:
   - `go.mod`
   - `rt/` (runtime package)
   - `main.go` + additional generated `.go` files
4. Invoke `go build` to produce the executable.

### Name Mangling (required)

Core identifiers include characters illegal in Go identifiers:

- module qualifiers: `Lune.IO.println`
- internal prim names: `$primIOBind`

Define `mangleIdent :: Text -> Text` with a deterministic scheme, e.g.:

- `.` → `_`
- `$` → `_dollar_`
- other non `[A-Za-z0-9_]` → `_uXXXX_` (unicode codepoint)

Keep a debug map (original ↔ mangled) for stack traces / panic messages.

### Packaging Strategy

Prefer **self-contained output** (no network required at build time):

- Generated directory is a full Go module.
- Runtime lives at `<module>/rt`.
- Third-party deps are either:
  - vendored (recommended for reproducible builds), or
  - fetched via `go mod` (acceptable for local dev, not great for “offline build”).

Because `github.com/anacrolix/stm` is MIT licensed, vendoring is feasible.

---

## Go Runtime Design (`rt` package)

The Go runtime is not a VM; it’s a small library that generated code imports.

### Value Representation

For a correctness-first implementation, use a boxed representation:

- `type Value = any`

And a small set of wrapper types for “special” Lune values:

- `type Con struct { Name string; Args []Value }` (data constructors, supports partial application)
- `type Record map[string]Value`
- `type Fun` (callable values, see below)
- `type IO func() Value`
- `type Atomic func(*stm.Tx) Value`
- `type Task struct { ch chan Value }` (or similar)

The Go backend can later specialize/unbox primitives (Ints, Bools, etc.) without changing the surface ABI.

### Function Application + Partial Application

Lune Core uses unary `CApp` everywhere; constructors and primitives are also “callable” values.

Runtime should provide:

- `Apply(f, x Value) Value`

Supported callable forms:

- user lambdas (closures)
- primitives (arity + arg accumulation)
- constructors (accumulate args into `Con`)

This mirrors the evaluator’s behavior (`src/Lune/Eval/Runtime.hs`).

### Records

Core has:

- `CRecord [(fieldName, expr)]`
- `CSelect base fieldName`

Represent records as `map[string]Value`. `Select` should panic (or return a tagged runtime error) if the field is missing.

---

## STM / Atomic in Go (via `github.com/anacrolix/stm`)

Lune surface API (`prelude/Lune/Atomic.lune`) expects these primitives:

- `prim_atomically : Atomic a -> IO a`
- `prim_newTVar    : a -> Atomic (Shared a)`
- `prim_readTVar   : Shared a -> Atomic a`
- `prim_writeTVar  : Shared a -> a -> Atomic Unit`
- `prim_retry      : Atomic a`  (`Atomic.wait`)
- `prim_orElse     : Atomic a -> Atomic a -> Atomic a`

Additionally, Core uses internal STM monad ops:

- `$primSTMPure : a -> Atomic a`
- `$primSTMBind : Atomic a -> (a -> Atomic b) -> Atomic b`

### Mapping to anacrolix/stm

Use:

- `Shared a` / `TVar a` → `*stm.Var[rt.Value]`
- `Atomic a` → `func(*stm.Tx) rt.Value`

Implement the primitives as thin adapters:

```go
package rt

import "github.com/anacrolix/stm"

type Value = any
type IO func() Value
type Atomic func(*stm.Tx) Value
type TVar = *stm.Var[Value]

// Apply applies a Lune function value to an argument.
// This is part of the runtime's general function/constructor/primitive model.
// (Implementation omitted here; see "Function Application + Partial Application".)
func Apply(f, x Value) Value { panic("TODO") }

func prim_STMPure(v Value) Atomic {
	return func(*stm.Tx) Value { return v }
}

// Note: `k` is a *Lune function value*, not a Go function.
// We must apply it using the runtime's Apply.
func prim_STMBind(ma Atomic, k Value) Atomic {
	return func(tx *stm.Tx) Value {
		a := ma(tx)
		mb := Apply(k, a).(Atomic)
		return mb(tx)
	}
}

func prim_newTVar(v Value) Atomic {
	return func(*stm.Tx) Value { return stm.NewVar[Value](v) }
}

func prim_readTVar(tv Value) Atomic {
	return func(tx *stm.Tx) Value { return tv.(TVar).Get(tx) }
}

func prim_writeTVar(tv Value, v Value) Atomic {
	return func(tx *stm.Tx) Value {
		tv.(TVar).Set(tx, v)
		return struct{}{} // Unit
	}
}

var prim_retry Atomic = func(tx *stm.Tx) Value {
	tx.Retry()
	return nil // unreachable
}

func prim_orElse(a, b Atomic) Atomic {
	return stm.Select(a, b)
}

func prim_atomically(ma Atomic) IO {
	return func() Value { return stm.Atomically(ma) }
}
```

Notes:

- `anacrolix/stm` implements `Retry` via `panic`/`recover` internally; this is compatible with Lune’s `wait` semantics.
- `stm.Select` matches `orElse`: it discards writes from a retried branch and retries based on the union of read sets.
- **Immutability caveat:** STM correctness assumes values stored in TVars aren’t mutated outside transactions. The Go backend should treat Lune values as immutable (or ensure copies for slices/maps/etc.).

---

## Tasks / Fibers in Go

Lune surface API (`prelude/Lune/Task.lune`) expects:

- `prim_spawn : IO a -> IO (Task a)`
- `prim_await : Task a -> IO a`
- `prim_yield : IO Unit`

Go mapping:

- `Task a` → handle containing a channel for the result.
- `spawn` → `go func(){ ch <- io() }()`
- `await` → `<-ch`
- `yield` → `runtime.Gosched()` (best-effort; Go scheduler is preemptive anyway).

This is “good enough” for v0; later we can add cancellation and structured concurrency.

---

## Dependencies + Reproducibility

### Recommended: vendor STM

To keep `lune build --target go` reproducible/offline:

- Vendor `github.com/anacrolix/stm` + the minimal subset of its dependencies used at runtime (not tests).
- Preserve MIT license text in the vendored tree.

Alternative: depend on upstream module in generated `go.mod`, but this requires network access.

---

## Milestones (suggested)

1. **Go runtime skeleton**: `rt.Value`, `rt.Apply`, constructors, records, `IO.pure/bind/then`.
2. **Basic primitives**: ints/bools/strings + printing, enough for `examples/00_Hello.lune`.
3. **STM primitives**: implement `Atomic` layer over `anacrolix/stm`, run `examples/06_STM_Counter.lune` and `examples/13_STM_OrElse.lune`.
4. **Tasks**: `prim_spawn/await/yield`, run the concurrency examples.
5. Expand coverage: files, bytes, sockets, TLS, time, JSON.

---

## Open Questions

### Q1: FFI on Go target (cgo vs C/LLVM backend)

There are two realistic stories for `foreign import ccall`:

1. **cgo on the Go target**
   - Pros: call C libraries from Go output; can preserve “ccall” intent on a single target.
   - Cons: requires a C toolchain; cross-compilation becomes harder; build times increase; cgo has sharp edges (pointer passing rules, callbacks, OS threads); distribution can be less “single static binary”.

2. **Keep Go target pure-Go; support `ccall` on a C/LLVM target**
   - Pros: Go target stays portable/simple; C/LLVM is a more natural place for C ABI interop and low-level control; avoids cgo complexity entirely.
   - Cons: programs that rely on `ccall` can’t use the Go target (they must pick the C/LLVM target).

**MVP decision:** treat `foreign import ccall` as **unsupported on the Go target**. Add cgo later behind a flag if it proves valuable.

### Q2: What’s easiest to implement?

For an MVP Go target:

- **Easiest:** reject Core that contains `CForeignImport` (or any FFI-only primitive) with a clear compile error.
- Implement the built-in effects (`IO`, `Atomic`, `Task`) and the primitive set used by the prelude using Go stdlib + `anacrolix/stm`.

Implementing “full cgo FFI” is strictly harder than implementing “no FFI (yet)”.

### Q3: Value representation / performance

**MVP decision:** prioritize a correct boxed runtime:

- `Value = any`
- specialize hot paths later (unboxed `int64`, faster `Apply`, compact ADTs/records, etc.)

### Q4: Module packaging (single package vs per-module packages)

This is about how generated Go code is arranged:

- **Single Go package (recommended for MVP):**
  - Emit many `.go` files, all `package main` (plus `rt` as a separate package).
  - Lune module boundaries exist at *compile time* only; at runtime everything is just Go identifiers in one package.
  - Avoids Go import cycles, simplifies name resolution, simplifies init ordering.

- **Per-Lune-module Go packages (future):**
  - Emit one Go package per Lune module (directories + `import` graph).
  - Closer to Lune’s module system, potentially nicer for library builds.
  - Harder: Go forbids import cycles; Lune codebases may rely on mutual recursion across modules; export/private mapping becomes real.

**MVP decision:** generate a single Go package for the program (`package main`) plus a small runtime package (`package rt`).

### Q5: Error model (panic vs typed runtime errors)

The evaluator represents runtime failures as `EvalError` and stops execution. For compiled targets we can choose:

- **Panic on runtime errors (recommended for MVP):**
  - Simple, matches “fail fast” behavior.
  - Go stack traces are useful during early development.
  - User-facing recoverable errors should still be modeled explicitly with `Result` (as the prelude already does for things like file IO).

- **Thread an explicit runtime error through everything:**
  - More controlled failures, but infects signatures (`Value` becomes `Result Error Value` everywhere) and adds overhead/complexity.

**MVP decision:** panic (or call a central `rt.Crash(msg)` helper) for unrecoverable runtime errors; use `Result` for recoverable domain errors.
