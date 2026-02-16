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

## Formatting

```bash
# Format file in place
cabal run lune -- --fmt path/to/File.lune

# Check formatting (non-zero exit if changes)
cabal run lune -- --fmt --check path/to/File.lune

# Print formatted output (no file changes)
cabal run lune -- --fmt --stdout path/to/File.lune
```

## Template Example

```haskell
module TemplateDemo exposing (main)

import Lune.IO as IO
import Template exposing (render, text, vcat, indent, Semigroup(..), Monoid(..))

main : Task Unit Unit
main =
  do
    let name = "world"
    IO.println (render "Hello, ${name}!")

    IO.println (render (text "a" <> ''b''))              -- block-aware newline
    IO.println (render (mconcat [''a'', ''b'', ''c'']))  -- Monoid instance
    IO.println (render (indent 2 (vcat [text "x", text "y"])))
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
| Concurrency (Fibers, STM) | Working |
| File I/O (read/write) | Working |
| TCP Sockets | Working |
| Web Framework (Api monad) | Basic (JSON APIs) |
| FFI (C interop via `foreign import ccall`) | Working |
| Native compilation | Partial |

## Documentation

- `spec/lune_language_report_v0_1.md` - Language specification
- `spec/lune_standard_library_v0_1.md` - Standard library API
- `spec/lune_json_v0_1.md` - JSON module specification
- `core/lune_core_ir_v0_1.md` - Core IR specification
- `docs/FFI.md` - Foreign Function Interface guide
- `docs/lsp.md` - Editor integration (LSP)

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

main : Task Unit Unit
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
import Lune.Fiber as Fiber
import Lune.String as Str

main : Task Unit Unit
main =
  do
    counter <- Atomic.commit (Atomic.new 0)
    f1 <- Fiber.spawn (increment counter)
    f2 <- Fiber.spawn (increment counter)
    _ <- Fiber.yield
    _ <- Fiber.yield
    _ <- Fiber.await f1
    _ <- Fiber.await f2
    n <- Atomic.commit (Atomic.read counter)
    IO.println (Str.append "Final: " (Str.fromInt n))

increment : Shared Int -> Task Unit Unit
increment tv =
  do
    _ <- Atomic.commit
      (do
        n <- Atomic.read tv
        Atomic.write tv (Int.add n 1)
      )
    IO.println "Incremented"
```

## File I/O Example

```haskell
module FileDemo exposing (main)

import Lune.IO as IO
import Lune.Task as Task
import Lune.String as Str

main : Task Unit Unit
main =
  Task.onError
    (do
      _ <- IO.writeFile "/tmp/test.txt" "Hello from Lune!"
      contents <- IO.readFile "/tmp/test.txt"
      IO.println (Str.append "Read: " contents)
    )
    (\_ -> IO.println "Error reading file")
```

## TCP Socket Example

```haskell
module EchoServer exposing (main)

import Lune.IO as IO
import Lune.Task as Task
import Lune.Net.Socket as Socket
import Lune.String as Str

main : Task Unit Unit
main =
  Task.onError
    (do
      sock <- Socket.listen 8080
      conn <- Socket.accept sock
      msg <- Socket.recv conn
      _ <- Socket.send conn (Str.append "Echo: " msg)
      _ <- Socket.closeConn conn
      _ <- Socket.closeSocket sock
      IO.println "Done"
    )
    (\_ -> IO.println "Socket error")
```

## Web API Example

```haskell
module ApiDemo exposing (main)

import Lune.IO as IO
import Lune.Http.Api as Api
import Lune.Http.Route as Route
import Lune.Http exposing (Request, Response, Method(..))
import Lune.Prelude exposing (Unit)

type AppError = NotFound String | BadRequest String

errorHandler : AppError -> Response
errorHandler err =
  case err of
    NotFound msg -> { status = 404, headers = [], body = msg }
    BadRequest msg -> { status = 400, headers = [], body = msg }

-- Handlers return Task e Response
healthHandler : Request -> {} -> Task AppError Response
healthHandler req ctx =
  Api.pure { status = 200, headers = [], body = "{\"status\":\"ok\"}" }

main : Task Error Unit
main =
  do
    let routes = Route.define [ Route.get "/health" healthHandler ]
    let config = { port = 8080, errorHandler = errorHandler, context = {} }
    Api.serve config routes
```

## FFI Example

```haskell
module FFI_Puts exposing (main)

import Lune.IO as IO
import Lune.Task as Task

foreign import ccall "puts" puts : String -> IO Int

main : Task Unit Unit
main =
  do
    _ <- Task.fromIO (puts "Hello from C!")
    IO.println "Hello from Lune!"
```

See `docs/FFI.md` for supported C functions, type mappings, and how to add new FFI bindings.
