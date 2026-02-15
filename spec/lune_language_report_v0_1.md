# The Lune Language Report (v0.1)

Lune is a strict, Haskell-like language designed around **do-first sequencing**, **type algebra**, and **practical deployment**.

Core goals:

- Strict evaluation (predictable performance)
- `do` notation as the primary control-flow abstraction
- Higher-kinded types (HKTs)
- Elm-like records and updates
- Explicit `Result`-based error handling (no exceptions)
- Green-thread concurrency with STM
- Simple, fast compilation to portable static binaries

---

## 1. Lexical Structure

### 1.1 Character Set
Source files are Unicode, but identifiers are restricted to:

- ASCII letters `a–z`, `A–Z`
- digits `0–9`
- underscore `_`

### 1.2 Tokens
Token categories:

- identifiers
- keywords
- literals
- punctuation

### 1.3 Keywords

Reserved words in Lune:

```
module import exposing as
type alias newtype class instance
case of let in
do
True False
```

### 1.4 Identifiers

- lowercase: values/functions (`mapM`, `parsePort`)
- uppercase: types/constructors (`Maybe`, `Just`)

Grammar:

```
lowerIdent ::= [a-z][a-zA-Z0-9_]*
upperIdent ::= [A-Z][a-zA-Z0-9_]*
```

### 1.5 Literals

Supported literals:

- integers: `0`, `42`, `-5`
- floats: `3.14`, `0.5`
- strings: `"hello"`
- characters: `'x'`
- lists: `[1, 2, 3]`, `[]`
- tuples: `(1, "hello")`, `(a, b, c)`

### 1.6 Comments

- line: `-- comment`
- block: `{- nested comments supported -}`

---

## 2. Layout / Indentation Rules

Lune uses indentation-sensitive layout like Elm/Haskell.

Example:

```haskell
main =
  do
    putStrLn "hi"
    x <- readInt
    putStrLn (showInt x)
```

Explicit braces `{}` are not supported in v0.1.

### 2.1 Source Formatting (`lune --fmt`)

The compiler includes a canonical, deterministic formatter:

- `lune --fmt <file.lune>` overwrites the file with formatted output.
- `lune --fmt --check <file.lune>` exits non-zero if formatting would change anything.
- `lune --fmt --stdout <file.lune>` prints formatted output without modifying the file.

The formatter is AST-based, semantics-preserving, idempotent, and enforces an 80-column maximum
using a “flat then expand” layout algorithm. See `docs/Formatting.md` for the full style summary.

---

## 3. Modules and Packages

### 3.1 Module Declaration

Each file begins with:

```haskell
module Name exposing (x, y, Type(..))
```

### 3.2 Exports

Exports are explicit:

- values: `mapM`
- types: `Maybe`
- constructors: `Maybe(..)`

### 3.3 Packages

A package is a collection of modules compiled together.

Cross-package linking uses interface files (§15).

Standard library surface API is defined in `spec/lune_standard_library_v0_1.md`.

---

## 4. Names, Imports, Exports

### 4.1 Imports

```haskell
import Data.List
import Data.List as List
import Data.List exposing (map, foldl)
```

### 4.2 Name Resolution

- local bindings shadow imports
- constructors share namespace with types
- values share namespace with functions

---

## 5. Expressions

Expressions in Lune are pure unless they return an effectful type (typically `Task`).

### 5.1 Expression Forms

```
e ::= literal
    | x
    | f e
    | \x -> e
    | let x = e in e
    | case e of ...
    | do { stmts }
    | [e, ...]
```

### 5.2 List Literals

List literals provide syntactic sugar for constructing lists:

```haskell
[1, 2, 3]         -- desugars to: Cons 1 (Cons 2 (Cons 3 Nil))
[]                -- desugars to: Nil
["a", "b"]        -- desugars to: Cons "a" (Cons "b" Nil)
[[1, 2], [3, 4]]  -- nested lists are supported
```

Multi-line lists use Elm-style leading commas:

```haskell
[ item1
, item2
, item3
]
```

Trailing commas are not permitted.

### 5.3 Tuple Literals

Tuple literals provide syntactic sugar for constructing tuples (2-5 elements):

```haskell
(1, 2)           -- desugars to: Pair 1 2
(a, b, c)        -- desugars to: Triple a b c
(1, 2, 3, 4)     -- desugars to: Quad 1 2 3 4
(a, b, c, d, e)  -- desugars to: Quint a b c d e
```

Tuples larger than 5 elements are not supported; use records instead.

Note: A single parenthesized expression `(x)` is NOT a tuple—it's just `x`.

### 5.4 Function Application

Application is left-associative:

```haskell
f x y = ((f x) y)
```

### 5.5 No Operators

Lune has **no infix operators** in v0.1.

All arithmetic and logic is prefix:

```haskell
addInt x y
and a b
```

---

## 6. let, case, lambda

### 6.1 let Bindings

Strict semantics:

```haskell
let x = expr in body
```

### 6.2 Lambdas

```haskell
\x -> expr
```

### 6.3 Case Expressions

Boolean branching uses:

```haskell
case cond of
  True -> ...
  False -> ...
```

No `if` syntax exists in Lune.

---

## 7. Do Blocks

`do` blocks are central in Lune.

### 7.1 Syntax

Statements include:

- binding: `x <- m`
- discard: `_ <- m`
- let: `let x = e`
- sequencing: `m; rest`

Example:

```haskell
do
  x <- readInt
  putStrLn (showInt x)
```

### 7.2 Restrictions

Pattern binds are not allowed in v0.1.

### 7.3 Desugaring

```haskell
do { x <- m; rest }
  = bindM m (\x -> do { rest })

do { m; rest }
  = thenM m (do { rest })

do { let x = e; rest }
  = let x = e in do { rest }

do { e }
  = e
```

---

## 8. Patterns

Patterns appear only in `case`.

Allowed forms:

- constructor patterns: `Just x`, `Cons h t`
- variable patterns: `x`
- wildcard: `_`
- list patterns: `[]`, `[x]`, `[a, b, c]`
- tuple patterns: `(x, y)`, `(a, _, c)`

### 8.1 List Patterns

List patterns provide syntactic sugar for matching lists:

```haskell
case xs of
  []        -> ...  -- matches empty list (Nil)
  [x]       -> ...  -- matches single-element list
  [a, b]    -> ...  -- matches exactly two elements
  [a, b, c] -> ...  -- matches exactly three elements
  Cons h t  -> ...  -- matches head and tail (not syntactic sugar)
```

List patterns desugar to nested `Cons`/`Nil` patterns:

```haskell
[x, y]  -- desugars to: Cons x (Cons y Nil)
```

Note: Rest/spread patterns like `[h, ...t]` are not supported. Use `Cons h t` for head-tail matching.

### 8.2 Tuple Patterns

Tuple patterns match tuple values:

```haskell
case point of
  (x, y) -> ...      -- matches Pair
  (a, b, c) -> ...   -- matches Triple
```

Tuple patterns desugar to constructor patterns:

```haskell
(x, y)  -- desugars to: Pair x y
```

---

## 9. Types and Kinds

### 9.1 Types

Lune supports:

- base types: `Int`, `Bool`, `String`
- ADTs: `Maybe a`
- functions: `a -> b`
- records: `{ x : Int, y : Int }`
- HKTs: `m a`

### 9.2 Kinds

Kinds include:

```
Type
Type -> Type
(Type -> Type) -> Type
```

Example:

```haskell
class Functor (f : Type -> Type)
```

---

## 10. Type Inference

Lune uses Hindley–Milner inference with constraints.

### 10.1 Required Annotations

Public exports must have signatures:

```haskell
mapM : Monad m => ...
```

---

## 11. Typeclasses

### 11.1 Class Declarations

```haskell
class Functor f where
  fmap : (a -> b) -> f a -> f b
```

Superclasses are supported:

```haskell
class Functor f => Applicative f where ...
class Applicative m => Monad m where ...
```

### 11.2 Coherence Rules

Lune enforces:

- no overlapping instances
- no orphan instances
- global deterministic resolution

---

## 12. Data Declarations

### 12.1 ADTs

```haskell
type Maybe a =
  Nothing
  | Just a
```

### 12.2 Records

Elm-style:

```haskell
type alias Point = { x : Int, y : Int }
{ p | x = 3 }
```

### 12.3 Newtypes

```haskell
newtype UserId =
  UserId Int
```

---

## 13. FFI

Lune uses explicit C ABI interop for calling C functions.

### 13.1 Foreign Import Declaration

```
ForeignImport ::= 'foreign' 'import' Convention StringLiteral Identifier ':' QualType

Convention ::= 'ccall'
```

Foreign import declarations bind a Lune name to a C function:

```haskell
foreign import ccall "puts" puts : String -> IO Int
```

The string literal is the C symbol name. The identifier is the Lune binding name. The type must have `IO` in the return position since all FFI calls perform side effects.

### 13.2 Example

```haskell
foreign import ccall "puts"
  puts : String -> IO Int
```

### 13.3 Safety

FFI calls bypass Lune's type safety at the boundary. Only whitelisted functions can be called; unknown symbols produce a runtime error. See `docs/FFI.md` for the full list of supported C functions and type mappings.

### 13.4 Known Limitations

FFI calls (like `puts`) write directly to C stdout, while `IO.println` buffers to `worldStdout`. In programs using both, the output order may not match the source code order. This is an inherent limitation of the world-passing IO model combined with real C side effects.

---

## 14. Runtime & Semantics

### 14.1 Evaluation Strategy

Lune is strict:

- eager arguments
- strict `let`
- no implicit laziness

### 14.2 Exceptions

Lune has **no runtime exceptions**.

All failure is via:

```haskell
Result e a
Task e a
```

`Task e a` is the primary user-facing effect type. Conceptually, it represents:

```haskell
IO (Result e a)
```

but is implemented as a distinct type constructor so it can have its own
`Functor`/`Applicative`/`Monad` instance. This allows `do`-notation to
short-circuit on `Err` automatically.

### 14.3 Task Model

Task is a monad:

```haskell
readFile : String -> Task Error String
writeFile : String -> String -> Task Error Unit
```

In `do`-notation, binding a `Task`:

- binds the value inside `Ok`
- short-circuits the whole `do` block on `Err`

```haskell
x <- readFile "hello.txt"
-- If readFile returns Err e, the rest of the block is skipped and the
-- whole Task returns Err e.
```

### 14.4 Primitive IO

`IO a` is the primitive runtime effect used by low-level primitives and FFI.
User code typically uses `Task` instead. You can observe a task as an IO
result via:

```haskell
attempt : Task e a -> IO (Result e a)
```

### 14.5 Network IO

TCP socket operations are available via `Lune.Net.Socket`:

```haskell
listen  : Int -> Task Error Socket
accept  : Socket -> Task Error Connection
connect : String -> Int -> Task Error Connection
recv    : Connection -> Task Error String
send    : Connection -> String -> Task Error Unit
```

### 14.6 Concurrency

Lune supports:

- green threads
- STM transactions

Core primitives:

```haskell
spawn : Task e a -> Task e2 (Fiber (Result e a))
commit : Atomic a -> Task e a
```

---

## 15. Compilation Model

### 15.1 Separate Compilation

Modules compile independently into:

- object code
- `.iface` interface summaries

### 15.2 Interface Files

Interfaces include:

- exported types/kinds
- instance tables
- ABI hashes

---

## 16. Standard Library

Lune v0.1 prelude includes:

- `Bool`, `Int`, `String`
- `List`, `Maybe`, `Result`
- `Functor`, `Applicative`, `Monad`
- `Task`, `Atomic`, `Shared`

Primitives:

```haskell
addInt : Int -> Int -> Int
appendString : String -> String -> String
```

---

## 17. Compatibility Notes vs Haskell

| Feature | Haskell | Lune |
|--------|---------|------|
| Evaluation | Lazy | Strict |
| Operators | Yes | None (prefix only) |
| if/then/else | Yes | Removed (`case Bool`) |
| Exceptions | Yes | None (`Result` only) |
| Pattern binds in do | Yes | Disallowed |
| Records | Limited | Elm-style |
| Concurrency | RTS threads | Green threads + STM |

---

**End of Lune Language Report v0.1**
