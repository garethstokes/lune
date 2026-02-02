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
- `Int`, `Char`, `String`
- `List(..)`
- `Maybe(..)`
- `Result(..)`

#### Exported effect and concurrency types

- `IO`
- `Atomic`, `Shared`
- `Task`

#### Exported typeclasses

- `Functor(..)`
- `Applicative(..)`
- `Monad(..)`

No functions other than constructors and typeclass methods are exported
directly from `Lune.Prelude`.

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

### 4.2 Lune.Bool

Exports:

- `and : Bool -> Bool -> Bool`
- `or  : Bool -> Bool -> Bool`
- `not : Bool -> Bool`

Note: These functions are strict and do not short-circuit.

---

## 5. String module

### 5.1 Lune.String

Exports:

- `append  : String -> String -> String`
- `fromInt : Int -> String`
- `toInt   : String -> Result String Int`

---

## 6. IO module

### 6.1 Lune.IO

Exports:

- `println  : String -> IO Unit`
- `readLine : IO String`
- `readInt  : IO Int`
- `sleepMs  : Int -> IO Unit`
- `readFile : String -> IO (Result IO.Error String)`
- `writeFile : String -> String -> IO (Result IO.Error Unit)`

Types:

- `type Error`

The `IO` effect represents interaction with the external world.
All observable side effects must occur within `IO`.

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

- `commit : Atomic a -> IO a`

`commit` executes an atomic computation and makes its effects visible
atomically within `IO`.

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

## 10. Concurrency tasks

### 10.1 Lune.Task

Exports:

Types:
- `Task a`

Functions:
- `start : IO a -> IO (Task a)`
- `await : Task a -> IO a`
- `yield : IO Unit`

Tasks represent lightweight concurrent computations scheduled by the runtime.

---

## 11. Typeclasses

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

## 12. Non-goals and internal identifiers

The following are explicitly **not** part of the standard library API:

- Dictionary values (`$dict*`)
- Primitive runtime hooks (`$prim*`, `prim_*`)
- Backend declarations (`extern`)
- Host entrypoints (`runMain`)
- Compiler desugaring helpers

These identifiers may exist internally but are not visible to user code.

---

## 13. Example

```
increment : Shared Int -> IO Unit
increment counter =
  Atomic.commit do
    x <- Atomic.read counter
    Atomic.write counter (Int.add x 1)
```

---

## 14. Versioning

This document defines the standard library surface for **Lune v0.1**.
Changes to exported names or semantics require a new version of this
specification.
