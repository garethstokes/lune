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
- strings: `"hello"`
- characters: `'x'`

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

Expressions in Lune are pure unless they return `IO`.

### 5.1 Expression Forms

```
e ::= literal
    | x
    | f e
    | \x -> e
    | let x = e in e
    | case e of ...
    | do { stmts }
```

### 5.2 Function Application

Application is left-associative:

```haskell
f x y = ((f x) y)
```

### 5.3 No Operators

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

- constructor patterns
- variable patterns
- wildcard `_`

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

Lune uses explicit C ABI interop:

```haskell
foreign import ccall "puts"
  puts : String -> IO (Result IOError Int)
```

All errors must be explicit.

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
IO (Result e a)
```

### 14.3 IO Model

IO is a monad:

```haskell
readFile : String -> IO (Result IOError String)
```

### 14.4 Concurrency

Lune supports:

- green threads
- STM transactions

Core primitives:

```haskell
spawn : IO a -> IO (Fiber a)
atomically : STM a -> IO a
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
- `IO`, `STM`, `TVar`, `Fiber`

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
