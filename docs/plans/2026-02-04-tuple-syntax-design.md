# Tuple Syntax Design

## Overview

Add tuple syntax to Lune for pattern matching on multiple values, returning multiple values, and lightweight grouping without named fields.

## Design Decisions

- **Arities**: 2-5 tuples (Pair through Quint)
- **Implementation**: Prelude-defined types with parser syntactic sugar (same approach as list literals)
- **Constructor names**: `Pair`, `Triple`, `Quad`, `Quint`

## Types in Prelude

```lune
type Pair a b = Pair a b

type Triple a b c = Triple a b c

type Quad a b c d = Quad a b c d

type Quint a b c d e = Quint a b c d e
```

Accessor functions for pairs:

```lune
fst : Pair a b -> a
fst p = case p of Pair x _ -> x

snd : Pair a b -> b
snd p = case p of Pair _ y -> y
```

## Parser Desugaring

**Expressions:**
```
(x, y)       →  Pair x y
(x, y, z)    →  Triple x y z
(a, b, c, d) →  Quad a b c d
(a, b, c, d, e) → Quint a b c d e
```

**Patterns:**
```
(x, y)       →  Pair x y
(a, b, c)    →  Triple a b c
```

**Types:**
```
(Int, String)        →  Pair Int String
(Int, String, Bool)  →  Triple Int String Bool
```

The parser will:
1. Distinguish tuples from parenthesized expressions by checking for commas
2. Count elements to pick the right constructor
3. Error if more than 5 elements

## Files to Modify

1. **`prelude/Lune/Prelude.lune`** — Add tuple types and export them
2. **`src/Lune/Parser.hs`** — Update `parseAtom`, `patternAtom`, and type parsing
3. **`spec/lune_language_report_v0_1.md`** — Document tuple syntax
4. **`examples/`** — Add or update example demonstrating tuples

No changes needed to:
- AST (`Syntax.hs`) — desugaring at parse time
- Type inference — tuples are regular ADTs
- Core IR — constructor applications
- Evaluator — already handles ADTs

## Test Cases

**Parsing:**
- `(1, 2)` → Pair expression
- `(x, y, z)` → Triple expression
- `((1, 2), 3)` → nested tuples
- `(x)` → parenthesized expression, NOT a 1-tuple

**Patterns:**
- `case p of (a, b) -> a`
- `case t of (x, _, z) -> x` — with wildcards

**Types:**
- `f : (Int, String) -> Int`
- `g : a -> (a, a)` — polymorphic

**Errors:**
- `(a, b, c, d, e, f)` → error: tuples limited to 5 elements

**Integration:**
- Fix `prelude/Lune/Api.lune` which currently uses unsupported tuple patterns
