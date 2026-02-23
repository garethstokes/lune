# Lune JSON Specification (v0.1)

This document specifies JSON support in **Lune**, following the design
principles of Elm’s `Json.Decode` and `Json.Encode`.

The design emphasizes:

- explicitness
- composability
- predictable error handling
- backend portability (native + JS)

JSON decoding failures are represented as ordinary values, not runtime
exceptions.

---

## 1. Overview

Lune JSON support consists of three modules:

- `Lune.Json`
- `Lune.Json.Decode`
- `Lune.Json.Encode`

These modules provide:

- an opaque JSON value type
- parsing and serialization
- Elm-style decoder combinators
- explicit JSON encoders

This specification defines **library behavior**, not new language features.

---

## 2. `Lune.Json`

### 2.1 Types

```haskell
type Json = Json#
```

- `Json` is an opaque type representing a JSON value.
- Its internal representation is runtime-backed and not visible to user code.

### 2.2 Functions

```haskell
parse : String -> Result String Json
stringify : Json -> String
```

`parse`:

- Parses a JSON string.
- Returns `Err message` on invalid JSON.
- Must not throw runtime exceptions.

`stringify`:

- Serializes a `Json` value into valid JSON text.
- Canonical formatting (whitespace, key order) is unspecified in v0.1.

---

## 3. `Lune.Json.Decode`

### 3.1 Core Types

```haskell
type Decoder a =
  Json -> Result Error a

type PathItem
  = Field String
  | Index Int

type Error =
  Error { message : String, path : List PathItem }
```

- Error paths are ordered **root → leaf**.
- Descending into structures appends path items.

### 3.2 Running Decoders

```haskell
decodeValue : Decoder a -> Json -> Result Error a
```

Equivalent to applying the decoder to the JSON value.

### 3.3 Primitive Decoders

```haskell
succeed : a -> Decoder a
fail : String -> Decoder a

bool   : Decoder Bool
int    : Decoder Int
float  : Decoder Float
string : Decoder String

null : a -> Decoder a
```

`null defaultValue`:

- Succeeds with the default value if the input is JSON `null`.
- Fails otherwise.

### 3.4 Structural Decoders

```haskell
list : Decoder a -> Decoder (List a)
maybe : Decoder a -> Decoder (Maybe a)
```

`list decoder`:

- Fails unless the input is a JSON array.
- On element failure, appends `Index i`.

`maybe decoder`:

- `null` → `Ok Nothing`
- Otherwise decodes the value and returns `Just a` or failure.

### 3.5 Object Access

```haskell
field : String -> Decoder a -> Decoder a
index : Int -> Decoder a -> Decoder a
at : List String -> Decoder a -> Decoder a
```

`field name decoder`:

- Requires a JSON object containing the given field.
- On failure, appends `Field name`.

`index i decoder`:

- Requires a JSON array with an element at the given index.
- On failure, appends `Index i`.

`at path decoder`:

- Equivalent to nested `field` access.

### 3.6 Decoder Composition

```haskell
map  : (a -> b) -> Decoder a -> Decoder b
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map3 : ...
map4 : ...
map5 : ...

andThen : (a -> Decoder b) -> Decoder a -> Decoder b
oneOf : List (Decoder a) -> Decoder a
```

`mapN` evaluation order:

- Decoders are evaluated left-to-right.
- The first failure is returned.

`andThen` semantics:

- Run the first decoder on the input.
- If successful, run the resulting decoder on the same original JSON value.

`oneOf` semantics:

- Tries decoders in order.
- Returns the first success.
- If all fail, returns the last error.

---

## 4. `Lune.Json.Encode`

### 4.1 Types

```haskell
type Value = Json
```

### 4.2 Primitive Encoders

```haskell
null   : Value
bool   : Bool -> Value
int    : Int -> Value
float  : Float -> Value
string : String -> Value
```

### 4.3 Structural Encoders

```haskell
list : (a -> Value) -> List a -> Value
object : List { key : String, value : Value } -> Value
```

`object`:

- If duplicate keys exist, last writer wins.

---

## 5. Runtime Semantics

### 5.1 JSON Representation

Internally, JSON values are represented by a runtime-backed structure supporting:

- null
- boolean
- integer
- float (IEEE 754 double)
- string
- array
- object

This representation is opaque to user code.

### 5.2 Errors and Failures

| Situation | Representation |
| --- | --- |
| Invalid JSON syntax | `Err message` (from `parse`) |
| Missing field | `Err (Error { ... })` (from decoding) |
| Type mismatch | `Err (Error { ... })` (from decoding) |
| Wrong primitive usage | `EvalError` |

JSON decoding failures must never produce `EvalError`.

---

## 6. Compiler Derive: `@derive(Json)`

The Lune compiler supports a templating/derive feature `@derive(Json)` which can
generate JSON encoders and decoders for common data shapes.

### 6.1 Supported Forms

`@derive(Json)` is supported for:

- **Record type aliases** (product types)
- **ADTs / sum types** (e.g. `type Model = A | B Int | C { ... }`)

It is not supported for other declaration forms.

### 6.2 Generated Names

Given a type name `Product` or `Model`, the compiler generates:

- `productDecoder : D.Decoder Product`
- `productEncoder : Product -> E.Value`
- `modelDecoder : D.Decoder Model`
- `modelEncoder : Model -> E.Value`

### 6.3 Encoding Rules

**Records**

- Encoded as JSON objects with one key per record field.

**ADTs**

- Encoded as a JSON object with:
  - `"tag"`: the constructor name as a string
  - `"fields"`: a JSON array of constructor arguments (positional), possibly empty

Examples:

```json
{ "tag": "Echo", "fields": [] }
{ "tag": "Add", "fields": [1, 2] }
{ "tag": "User", "fields": [ { "id": 1, "name": "A" } ] }
```

---

## 6. Conformance Requirements

An implementation must ensure:

- No JSON failure causes a runtime exception.
- Error paths are accurate and ordered root → leaf.
- `mapN` decoders evaluate left-to-right.
- `andThen` uses the original input value.
- `oneOf` returns the last error on total failure.

---

## 7. Recommended Tests

- Missing field produces path `[Field "x"]`.
- List element failure produces `[Index i]`.
- Nested `at` accumulates paths correctly.
- `oneOf` respects ordering.
- Encode → `stringify` smoke test.

---

## Appendix A: Canonical JSON Wire Representation

This appendix documents the standard JSON representation for Lune types when
serialized via `Lune.Json.Encode` or expected by decoder helpers in
`Lune.Json.Decode.Extra`.

### A.1 Records

Lune records map directly to JSON objects:

```
{ name : String, age : Int }  →  {"name":"Alice","age":30}
```

### A.2 Lists

Lune lists map to JSON arrays:

```
Cons 1 (Cons 2 (Cons 3 Nil))  →  [1,2,3]
```

### A.3 Algebraic Data Types (ADTs)

ADTs are represented as tagged objects:

```
{ "tag": "<Constructor>", "fields": [...] }
```

Examples:

| Lune Value | JSON |
| --- | --- |
| `Nothing` | `{"tag":"Nothing","fields":[]}` |
| `Just 42` | `{"tag":"Just","fields":[42]}` |
| `Ok "hi"` | `{"tag":"Ok","fields":["hi"]}` |
| `Err "bad"` | `{"tag":"Err","fields":["bad"]}` |

Zero-arity constructors may omit the `fields` array:

```
{"tag":"Nothing"}
```

### A.4 Newtypes

Newtypes are encoded transparently as their underlying value:

```
newtype UserId = UserId Int
UserId 123  →  123
```

### A.5 Duplicate Object Keys

When encoding objects with duplicate keys, **last writer wins**:

```
object [{ key = "x", value = int 1 }, { key = "x", value = int 2 }]
  →  {"x":2}
```

This matches the behavior specified in `Lune.Json.Encode.object`.
