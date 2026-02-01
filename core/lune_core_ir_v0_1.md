# Lune Core IR Specification (v0.1 draft)

This document defines **Lune Core**, the compiler's internal language after parsing,
renaming, typechecking, and desugaring (including `do`).

Lune Core is designed to be:
- Small and orthogonal
- Easy to typecheck
- Easy to optimize
- Straightforward to lower to LLVM or native code
- Strict (no implicit laziness)

---

## 1. Kinds

Kinds classify types.

```
k ::= Type
    | k -> k
```

---

## 2. Types

```
t ::= a                      -- type variable
    | T                      -- type constructor (Int, Bool, IO, List, ...)
    | t1 t2                  -- type application
    | t1 -> t2               -- function type (strict)
    | forall (a : k). t      -- universal quantification
    | { l1 : t1, ... }       -- record type (row-polymorphism is v2)
```

Notes:
- `->` is strict.
- `forall` appears after elaboration; surface polymorphism elaborates to `forall`.
- HKTs are expressed via kinds and type application.

---

## 3. Terms

```
e ::= x
    | lit
    | \(x : t). e
    | e1 e2
    | Λ(a : k). e            -- type abstraction
    | e [t]                  -- type application
    | let x = e1 in e2
    | letrec { f1 = e1; ... } in e2
    | Con K (e1, ..., en)
    | case e of { alt1; ...; altn }
    | prim op (e1, ..., en)
    | dictSelect e "method"
```

### 3.1 Literals
- integers, chars, strings, unit

### 3.2 Case alternatives
```
alt ::= K (p1, ..., pn) -> e
      | DEFAULT -> e
```

Patterns `pi` are *Core patterns*:
- variables
- wildcard `_`

Nested patterns are compiled away earlier into nested `case`.

---

## 4. Strict Semantics

Evaluation is call-by-value:
- In `e1 e2`, evaluate `e1` then `e2` then apply.
- In `let x = e1 in e2`, evaluate `e1` then bind `x` then evaluate `e2`.

---

## 5. Desugaring from Surface Lune

### 5.1 `do` blocks

Surface:

```
do
  x <- m
  n
```

Core conceptually becomes:

```
bindM m (\x -> n)
```

More precisely (with dictionaries):

If `bindM` is a method of `Monad m`, then Core is:

```
\(dictMonad : MonadDict m).
  (dictSelect dictMonad "bindM") m (\x -> ...)
```

However, for built-in monads (IO/STM) the compiler may lower `bindM` to primitives.

### 5.2 No operators

Surface has no infix operators; function application is explicit in Core.

### 5.3 Pattern compilation

Surface multi-arg patterns in function definitions are converted to lambdas + `case`.

---

## 6. Typeclasses in Core (Dictionary Passing)

Each class becomes:
- a dictionary type (record) containing methods
- instance declarations become dictionary values
- constraints become extra parameters

Example surface:

```
class Functor f where
  fmap : (a -> b) -> f a -> f b
```

Core type:

```
FunctorDict f = { fmap : forall a. forall b. (a -> b) -> f a -> f b }
```

Constraint usage:

Surface:
```
mapM : Monad m => ...
```

Core:
```
mapM : forall m. MonadDict m -> ...
```

Instance selection is resolved at compile time, producing a concrete dictionary value to pass.

---

## 7. Optimization Passes (recommended v0.1)

Keep compile times fast by using a small number of cheap passes:

1. **Simplify**
   - inline trivial wrappers (especially dictionary selectors)
   - beta-reduce small lambdas
   - eliminate dead `let` bindings

2. **Bind/Then rewrite rules**
   - `thenM (pureM unit) x  ==> x`
   - `bindM (pureM v) k     ==> k v`

3. **Case-of-known-constructor**
   - `case (Con K ...) of ...` reduces immediately

---

## 8. Lowering Targets

Two practical lowering strategies:

- Core -> ANF -> LLVM IR
- Core -> CPS-lite -> native code

The compiler must emit GC metadata for allocations (closures, ADTs, records).

---

End of Lune Core IR v0.1 draft
