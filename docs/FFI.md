# Foreign Function Interface (FFI)

Lune supports calling C functions through a Foreign Function Interface.

## Syntax

```lune
foreign import ccall "c_symbol_name" luneName : Type
```

The declaration can span multiple lines:

```lune
foreign import ccall "puts"
  puts : String -> IO Int
```

## Supported C Functions

Currently, only whitelisted C functions can be called:

| C Symbol | Type Signature | Description |
|----------|---------------|-------------|
| `puts` | `String -> IO Int` | Print string to stdout with newline |
| `strlen` | `String -> IO Int` | Get length of C string |

## Type Mapping

| Lune Type | C Type | Notes |
|-----------|--------|-------|
| `String` | `const char*` | Null-terminated, marshalled via `withCString` |
| `Int` | `CInt` | Platform-dependent width |
| `IO a` | Side effect | All FFI calls must return `IO` |

## Examples

### Calling puts

```lune
module FFI_Puts exposing (main)

import Lune.IO as IO

foreign import ccall "puts" puts : String -> IO Int

main : IO Unit
main =
  do
    _ <- puts "Hello from C!"
    IO.println "Hello from Lune!"
```

### Calling strlen

```lune
module FFI_Strlen exposing (main)

import Lune.IO as IO
import Lune.String as Str

foreign import ccall "strlen" strlen : String -> IO Int

main : IO Unit
main =
  do
    len <- strlen "Hello, World!"
    IO.println (Str.append "Length: " (Str.fromInt len))
```

## Safety

FFI calls bypass Lune's type safety at the boundary. Only whitelisted functions in `src/Lune/Eval/FFI.hs` can be called. Unknown symbols produce a runtime error.

## Adding New FFI Functions

To whitelist a new C function, edit `src/Lune/Eval/FFI.hs`:

1. Add a `foreign import ccall` binding
2. Write a marshalling wrapper (`[Value] -> IO (Either EvalError Value)`)
3. Register it in `ffiRegistry`
