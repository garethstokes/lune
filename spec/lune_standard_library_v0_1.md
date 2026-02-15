# Lune Standard Library Surface (v0.1, Atomic-first)

This document specifies the **user-facing standard library API** for Lune v0.1.  
It is normative and intended to align with the *Lune Language Report (v0.1)*.

This specification defines **names, modules, and exports** only.  
It does not define runtime representation or compiler internals.

---

## 1. Scope and intent

The Lune standard library is designed to:

- Support the language goals defined in the Language Report:
  - strict evaluation
  - do-notation as primary control flow
  - explicit effects
  - Result-based error handling
  - predictable concurrency
- Provide a **small, stable, and readable** surface for user programs
- Avoid exposing compiler or runtime implementation details

All names defined here are considered **public API**.

---

## 2. Naming conventions

The following conventions are normative:

1. **Module-qualified names**
   Functions are grouped by domain and accessed via modules
   (e.g. `Int.add`, `IO.println`, `Atomic.commit`).

2. **Intent-first terminology**
   Names describe observable behavior, not implementation strategy.

3. **Verb-oriented functions**
   Especially for effects (`read`, `write`, `commit`, `start`).

4. **No operators**
   All functionality is expressed through prefix function application,
   consistent with the core language.

5. **No exposure of compiler plumbing**
   Dictionary values, primitive hooks, and desugaring helpers are not part
   of the standard library surface.

---

## 3. Prelude module

### 3.1 Lune.Prelude

`Lune.Prelude` is implicitly imported into every module unless explicitly disabled.

It re-exports the core types and typeclasses required for most programs.

#### Exported types and constructors

- `Unit`, `unit`
- `Bool(..)`
- `Int`, `Float`, `Char`, `String`
- `List(..)`
- `Maybe(..)`
- `Result(..)`
- `Pair(..)`, `Triple(..)`, `Quad(..)`, `Quint(..)`

#### Exported effect and concurrency types

- `Atomic`, `Shared`
- `Task`

#### Exported typeclasses

- `Functor(..)`
- `Applicative(..)`
- `Monad(..)`

#### Exported functions

- `fst : Pair a b -> a`
- `snd : Pair a b -> b`

---

## 4. Numeric and boolean modules

### 4.1 Lune.Int

Exports:

- `add : Int -> Int -> Int`
- `sub : Int -> Int -> Int`
- `mul : Int -> Int -> Int`
- `eq  : Int -> Int -> Bool`
- `gte : Int -> Int -> Bool`
- `lte : Int -> Int -> Bool`

### 4.2 Lune.Float

Exports:

- `add : Float -> Float -> Float`
- `sub : Float -> Float -> Float`
- `mul : Float -> Float -> Float`
- `div : Float -> Float -> Float`
- `eq  : Float -> Float -> Bool`
- `gt  : Float -> Float -> Bool`
- `lt  : Float -> Float -> Bool`
- `gte : Float -> Float -> Bool`
- `lte : Float -> Float -> Bool`
- `fromInt  : Int -> Float`
- `truncate : Float -> Int`

### 4.3 Lune.Bool

Exports:

- `and : Bool -> Bool -> Bool`
- `or  : Bool -> Bool -> Bool`
- `not : Bool -> Bool`

Note: These functions are strict and do not short-circuit.

---

## 5. String and Char modules

### 5.1 Lune.String

Exports:

- `append     : String -> String -> String`
- `eq         : String -> String -> Bool`
- `fromInt    : Int -> String`
- `fromFloat  : Float -> String`
- `toInt      : String -> Result String Int`
- `toChars    : String -> List Char`
- `fromChars  : List Char -> String`
- `split      : String -> String -> List String`
- `join       : String -> List String -> String`
- `toLower    : String -> String`
- `indexOf    : String -> String -> Maybe Int`
- `startsWith : String -> String -> Bool`
- `isEmpty    : String -> Bool`
- `take       : Int -> String -> String`
- `drop       : Int -> String -> String`
- `trim       : String -> String`
- `length     : String -> Int`

### 5.2 Lune.Char

Exports:

- `toInt   : Char -> Int`
- `fromInt : Int -> Char`
- `toLower : Char -> Char`
- `toUpper : Char -> Char`
- `isSpace : Char -> Bool`
- `eq      : Char -> Char -> Bool`

---

## 6. IO module

### 6.1 Lune.IO

Exports:

- `println  : String -> Task e Unit`
- `readLine : Task e String`
- `readInt  : Task e Int`
- `sleepMs  : Int -> Task e Unit`
- `readFile : String -> Task Error String`
- `writeFile : String -> String -> Task Error Unit`

Types:

- `type Error`

The `Lune.IO` module provides user-facing world interaction via `Task`.
The primitive `IO` type still exists for low-level primitives and FFI, but
most user code should use `Task`.

---

## 6.2 Lune.Net.Socket

Exports:

Types:
- `type Socket`
- `type Connection`

Functions:
- `listen      : Int -> Task Error Socket`
- `accept      : Socket -> Task Error Connection`
- `connect     : String -> Int -> Task Error Connection`
- `recv        : Connection -> Task Error String`
- `send        : Connection -> String -> Task Error Unit`
- `closeConn   : Connection -> Task Error Unit`
- `closeSocket : Socket -> Task Error Unit`

The `Socket` type represents a listening TCP socket bound to a port.
The `Connection` type represents an established TCP connection.

All socket operations use `Task Error a` to allow graceful error handling.

---

## 7. Result and Maybe modules

### 7.1 Lune.Result

Exports:

- `map : (a -> b) -> Result e a -> Result e b`
- `mapError : (e -> f) -> Result e a -> Result f a`
- `andThen : (a -> Result e b) -> Result e a -> Result e b`
- `withDefault : a -> Result e a -> a`

### 7.2 Lune.Maybe

Exports:

- `map : (a -> b) -> Maybe a -> Maybe b`
- `andThen : (a -> Maybe b) -> Maybe a -> Maybe b`
- `withDefault : a -> Maybe a -> a`

---

## 8. List module

### 8.1 Lune.List

Exports (minimal set for v0.1):

- `map : (a -> b) -> List a -> List b`
- `foldl : (b -> a -> b) -> b -> List a -> b`

Additional list operations may be added in later versions.

---

## 9. Atomic shared state

### 9.1 Atomic effect

```
Atomic a
```

`Atomic` represents a transactional computation over shared state.
Atomic computations are pure until committed.

### 9.2 Shared state handle

```
Shared a
```

A `Shared a` value refers to shared mutable state of type `a`.
Shared state may only be accessed within the `Atomic` effect.

### 9.3 Running atomic computations

Exports:

- `commit : Atomic a -> Task e a`

`commit` executes an atomic computation and makes its effects visible
atomically within `Task`.

### 9.4 Shared state operations

Exports:

- `new   : a -> Atomic (Shared a)`
- `read  : Shared a -> Atomic a`
- `write : Shared a -> a -> Atomic Unit`

### 9.5 Transaction control

Exports:

- `wait   : Atomic a`
- `orElse : Atomic a -> Atomic a -> Atomic a`

`wait` aborts the current transaction and suspends execution until one of
the observed shared values changes.

---

## 10. Task effect

### 10.1 Lune.Task

Exports:

Types:
- `Task e a`

Functions:
- `succeed : a -> Task e a`
- `fail : e -> Task e a`
- `map : (a -> b) -> Task e a -> Task e b`
- `andThen : Task e a -> (a -> Task e b) -> Task e b`
- `then : Task e a -> Task e b -> Task e b`
- `mapError : (e -> e2) -> Task e a -> Task e2 a`
- `onError : Task e a -> (e -> Task e2 a) -> Task e2 a`
- `fromIO : IO a -> Task e a`
- `attempt : Task e a -> IO (Result e a)`

`Task e a` represents suspendable world interaction with typed failure.

In `do`-notation, binding a `Task` short-circuits the block on `Err`.

---

## 11. Fibers

### 11.1 Lune.Fiber

Exports:

Types:
- `Fiber a`

Functions:
- `spawn : Task e a -> Task e2 (Fiber (Result e a))`
- `await : Fiber (Result e a) -> Task e a`
- `yield : Task e Unit`

---

## 12. Typeclasses

### 11.1 Functor

```
class Functor f where
  map : (a -> b) -> f a -> f b
```

### 11.2 Applicative

```
class Functor f => Applicative f where
  pure  : a -> f a
  apply : f (a -> b) -> f a -> f b
```

### 11.3 Monad

```
class Applicative m => Monad m where
  andThen : m a -> (a -> m b) -> m b
  then    : m a -> m b -> m b
```

These method names define the standard sequencing vocabulary used by
do-notation.

---

## 13. Non-goals and internal identifiers

The following are explicitly **not** part of the standard library API:

- Dictionary values (`$dict*`)
- Primitive runtime hooks (`$prim*`, `prim_*`)
- Backend declarations (`extern`)
- Host entrypoints (`runMain`)
- Compiler desugaring helpers

These identifiers may exist internally but are not visible to user code.

---

## 14. Example

```
increment : Shared Int -> Task e Unit
increment counter =
  Atomic.commit do
    x <- Atomic.read counter
    Atomic.write counter (Int.add x 1)
```

---

## 15. Versioning

This document defines the standard library surface for **Lune v0.1**.
Changes to exported names or semantics require a new version of this
specification.
