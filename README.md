# Lune v0.1

A strict, Haskell-like language with do-notation, type classes, and Elm-style records.

## Project Structure

```
lune/
├── app/              # CLI entry point
├── src/Lune/         # Compiler implementation
│   ├── Parser.hs     # Megaparsec-based parser
│   ├── Syntax.hs     # Surface AST
│   ├── Desugar.hs    # do-notation desugaring
│   ├── Resolve.hs    # Name resolution
│   ├── Infer.hs      # Type inference
│   ├── Elaborate.hs  # Typeclass elaboration
│   ├── Core.hs       # Core IR
│   └── Eval/         # Tree-walking interpreter
├── prelude/          # Standard library modules
├── examples/         # Example programs
├── tests/            # Golden tests
└── spec/             # Language specifications
```

## Building

```bash
cabal build
```

## Running Examples

```bash
# Parse only (show AST)
cabal run lune -- examples/00_Hello.lune

# Evaluate
cabal run lune -- --eval examples/00_Hello.lune
```

## Current Features (v0.1)

| Feature | Status |
|---------|--------|
| Basic types (Int, String, Bool, Char) | Working |
| Algebraic data types | Working |
| Newtypes | Working |
| Elm-style records | Working |
| Pattern matching | Working |
| Do-notation | Working |
| Type classes (Functor, Applicative, Monad) | Working |
| Higher-kinded types | Working |
| Module system with imports | Working |
| JSON parsing/encoding | Working |
| Multi-line function application | Working |
| Concurrency (Tasks, STM) | Working |
| FFI | Not implemented |
| Native compilation | Partial |

## Documentation

- `spec/lune_language_report_v0_1.md` - Language specification
- `spec/lune_standard_library_v0_1.md` - Standard library API
- `spec/lune_json_v0_1.md` - JSON module specification
- `core/lune_core_ir_v0_1.md` - Core IR specification

## Tests

```bash
# Run all golden tests
cabal test golden

# Accept new golden output
cabal test golden --test-options="--accept"
```

## Example

```haskell
module Demo exposing (main)

import Lune.IO as IO
import Lune.Int as Int
import Lune.String as Str

main : IO Unit
main =
  do
    IO.println "Hello, Lune!"
    let x = Int.add 1 2
    IO.println (Str.append "1 + 2 = " (Str.fromInt x))
```

## Concurrency Example

```haskell
module ConcurrencyDemo exposing (main)

import Lune.IO as IO
import Lune.Atomic as Atomic
import Lune.Task as Task
import Lune.String as Str

main : IO Unit
main =
  do
    counter <- Atomic.commit (Atomic.new 0)
    t1 <- Task.start (increment counter)
    t2 <- Task.start (increment counter)
    _ <- Task.yield
    _ <- Task.yield
    _ <- Task.await t1
    _ <- Task.await t2
    n <- Atomic.commit (Atomic.read counter)
    IO.println (Str.append "Final: " (Str.fromInt n))

increment : Shared Int -> IO Unit
increment tv =
  do
    _ <- Atomic.commit
      (do
        n <- Atomic.read tv
        Atomic.write tv (Int.add n 1)
      )
    IO.println "Incremented"
```
