# Float Type and JSON Improvements

> **Status:** APPROVED
> **Date:** 2026-02-04

## Overview

Add `Float` as a primitive type to Lune and update the JSON API with float support. Simplify JSON examples to clearly demonstrate encoding and decoding.

## 1. Float Type Foundation

### `src/Lune/Eval/Types.hs`

Add `VFloat Double` to the `Value` data type:

```haskell
data Value
  = VInt Integer
  | VFloat Double    -- NEW
  | VString Text
  ...
```

### `prelude/Lune/Prelude.lune`

Add `Float` to exports and declare primitive type:

```
module Lune.Prelude exposing (
  ...
  Int, Char, String, Float,
  ...
)

type Float = Float#
```

### `prelude/Lune/Float.lune` (new file)

```
module Lune.Float exposing (
  add, sub, mul, div,
  eq, gt, lt, gte, lte,
  fromInt, truncate
)

import Lune.Prelude exposing (Bool, Int, Float)

add : Float -> Float -> Float
add = prim_addFloat

sub : Float -> Float -> Float
sub = prim_subFloat

mul : Float -> Float -> Float
mul = prim_mulFloat

div : Float -> Float -> Float
div = prim_divFloat

eq : Float -> Float -> Bool
eq = prim_eqFloat

gt : Float -> Float -> Bool
gt = prim_gtFloat

lt : Float -> Float -> Bool
lt = prim_ltFloat

gte : Float -> Float -> Bool
gte = prim_geFloat

lte : Float -> Float -> Bool
lte = prim_leFloat

fromInt : Int -> Float
fromInt = prim_fromIntFloat

truncate : Float -> Int
truncate = prim_truncateFloat
```

---

## 2. Builtins Implementation

### `src/Lune/Builtins.hs` - Type Schemes

Add to `builtinSchemes`:

```haskell
-- Float primitives
, ("prim_addFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Float"))))
, ("prim_subFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Float"))))
, ("prim_mulFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Float"))))
, ("prim_divFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Float"))))
, ("prim_eqFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
, ("prim_gtFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
, ("prim_ltFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
, ("prim_geFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
, ("prim_leFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
, ("prim_fromIntFloat", Forall [] [] (TArrow (TCon "Int") (TCon "Float")))
, ("prim_truncateFloat", Forall [] [] (TArrow (TCon "Float") (TCon "Int")))
, ("prim_showFloat", Forall [] [] (TArrow (TCon "Float") (TCon "String")))

-- JSON float support
, ("prim_jsonFloat", Forall [] [] (TArrow (TCon "Float") (TCon "Json")))
, ("prim_jsonToFloat", Forall [] [] (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Float"))))
```

### `src/Lune/Builtins.hs` - Eval Primitives

Add to `builtinEvalPrims`:

```haskell
, ("prim_addFloat", 2, \case
    [VFloat a, VFloat b] -> Right (VFloat (a + b))
    _ -> Left ...)
, ("prim_subFloat", 2, \case
    [VFloat a, VFloat b] -> Right (VFloat (a - b))
    _ -> Left ...)
, ("prim_mulFloat", 2, \case
    [VFloat a, VFloat b] -> Right (VFloat (a * b))
    _ -> Left ...)
, ("prim_divFloat", 2, \case
    [VFloat a, VFloat b] -> Right (VFloat (a / b))
    _ -> Left ...)
, ("prim_eqFloat", 2, \case
    [VFloat a, VFloat b] -> Right (boolCon (a == b))
    _ -> Left ...)
, ("prim_gtFloat", 2, \case
    [VFloat a, VFloat b] -> Right (boolCon (a > b))
    _ -> Left ...)
, ("prim_ltFloat", 2, \case
    [VFloat a, VFloat b] -> Right (boolCon (a < b))
    _ -> Left ...)
, ("prim_geFloat", 2, \case
    [VFloat a, VFloat b] -> Right (boolCon (a >= b))
    _ -> Left ...)
, ("prim_leFloat", 2, \case
    [VFloat a, VFloat b] -> Right (boolCon (a <= b))
    _ -> Left ...)
, ("prim_fromIntFloat", 1, \case
    [VInt n] -> Right (VFloat (fromIntegral n))
    _ -> Left ...)
, ("prim_truncateFloat", 1, \case
    [VFloat f] -> Right (VInt (truncate f))
    _ -> Left ...)
, ("prim_showFloat", 1, \case
    [VFloat f] -> Right (VString (T.pack (show f)))
    _ -> Left ...)
, ("prim_jsonFloat", 1, \case
    [VFloat f] -> Right (VJson (JFloat f))
    _ -> Left ...)
, ("prim_jsonToFloat", 1, \case
    [VJson jv] -> case jv of
      JFloat f -> Right (resultOk (VFloat f))
      JInt n -> Right (resultOk (VFloat (fromIntegral n)))
      _ -> Right (resultErr "expected number")
    _ -> Left ...)
```

### `src/Lune/Eval/Types.hs` - JsonValue

Add `JFloat` to `JsonValue`:

```haskell
data JsonValue
  = JNull
  | JBool Bool
  | JInt Integer
  | JFloat Double    -- NEW
  | JString Text
  | JArray [JsonValue]
  | JObject [(Text, JsonValue)]
```

---

## 3. JSON Module Updates

### `prelude/Lune/Json/Encode.lune`

Add to exports and implement:

```
float : Float -> Value
float = prim_jsonFloat
```

### `prelude/Lune/Json/Decode.lune`

Add to exports and implement:

```
float : Decoder Float
float =
  \json ->
    case prim_jsonToFloat json of
      Ok f -> Ok f
      Err msg -> Err { message = msg, path = Nil }
```

### `prelude/Lune/String.lune`

Add float-to-string conversion:

```
fromFloat : Float -> String
fromFloat = prim_showFloat
```

---

## 4. Updated Examples

### `examples/10_Json_Encode.lune` (new)

```
module JsonEncode exposing (main)

import Lune.IO as IO
import Lune.Json as Json
import Lune.Json.Encode as E
import Lune.Prelude exposing (IO, Unit, List(..), Bool(..))

main : IO Unit
main =
  do
    IO.println "=== JSON Encoding ==="
    IO.println (Json.stringify userJson)
    IO.println (Json.stringify scoresJson)

userJson : Json.Json
userJson =
  E.object
    (Cons { key = "name", value = E.string "Alice" }
      (Cons { key = "age", value = E.int 30 }
        (Cons { key = "score", value = E.float 95.5 }
          (Cons { key = "active", value = E.bool True } Nil))))

scoresJson : Json.Json
scoresJson =
  E.list E.float (Cons 3.14 (Cons 2.71 (Cons 1.41 Nil)))
```

### `examples/10_Json_Decode.lune` (rewritten)

```
module JsonDecode exposing (main)

import Lune.IO as IO
import Lune.Json as Json
import Lune.Json.Decode as D
import Lune.String as Str
import Lune.Prelude exposing (IO, Unit, Result(..), List(..), Maybe(..))

main : IO Unit
main =
  do
    IO.println "=== JSON Decoding ==="
    decodeUser
    decodeNested
    decodeWithMaybe

decodeUser : IO Unit
decodeUser =
  case Json.parse "{\"name\":\"Bob\",\"score\":87.5}" of
    Err msg -> IO.println (Str.append "Parse error: " msg)
    Ok json ->
      case D.decodeValue userDecoder json of
        Err err -> IO.println (Str.append "Decode error: " err.message)
        Ok user -> IO.println (Str.append "User: " user.name)

userDecoder : D.Decoder { name : String, score : Float }
userDecoder =
  D.map2 (\n s -> { name = n, score = s })
    (D.field "name" D.string)
    (D.field "score" D.float)

decodeNested : IO Unit
decodeNested =
  case Json.parse "{\"data\":{\"value\":42.0}}" of
    Err _ -> IO.println "Parse failed"
    Ok json ->
      case D.decodeValue (D.at (Cons "data" (Cons "value" Nil)) D.float) json of
        Err err -> IO.println (Str.append "Error: " err.message)
        Ok v -> IO.println (Str.append "Nested value: " (Str.fromFloat v))

decodeWithMaybe : IO Unit
decodeWithMaybe =
  case Json.parse "{\"required\":1,\"optional\":null}" of
    Err _ -> IO.println "Parse failed"
    Ok json ->
      case D.decodeValue (D.field "optional" (D.maybe D.int)) json of
        Err err -> IO.println (Str.append "Error: " err.message)
        Ok Nothing -> IO.println "Optional field is null"
        Ok (Just n) -> IO.println "Has value"
```

### Files to Delete

- `examples/10_Json_Parse.lune` (merged into Encode example)
- `examples/10_Json_OneOf_EdgeCase.lune` (edge case belongs in tests)

---

## 5. Spec Update

Update `spec/lune_json_v0_1.md` to document:

- `float : Float -> Value` in Encode section
- `float : Decoder Float` in Decode section

---

## Summary

| File | Change |
|------|--------|
| `src/Lune/Eval/Types.hs` | Add `VFloat Double`, `JFloat Double` |
| `src/Lune/Builtins.hs` | Add float primitive schemes + implementations |
| `prelude/Lune/Prelude.lune` | Export `Float` type |
| `prelude/Lune/Float.lune` | New module with float operations |
| `prelude/Lune/String.lune` | Add `fromFloat` |
| `prelude/Lune/Json/Encode.lune` | Add `float` encoder |
| `prelude/Lune/Json/Decode.lune` | Add `float` decoder |
| `spec/lune_json_v0_1.md` | Document float support |
| `examples/10_Json_Encode.lune` | New succinct encode example |
| `examples/10_Json_Decode.lune` | Rewritten succinct decode example |
| Delete `examples/10_Json_Parse.lune` | Merged into Encode example |
| Delete `examples/10_Json_OneOf_EdgeCase.lune` | Edge case, not example |
