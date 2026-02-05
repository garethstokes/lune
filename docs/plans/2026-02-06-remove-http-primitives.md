# Remove HTTP Primitives Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove all 11 HTTP-specific Haskell primitives from Builtins.hs and reimplement them as pure Lune code, using 4 new general-purpose char/string primitives as the foundation.

**Architecture:** Add `prim_stringToChars`, `prim_charsToString`, `prim_charToInt`, `prim_intToChar` as the only new Haskell primitives. Build string utilities (split, toLower, startsWith, etc.) in pure Lune on top of those. Rewrite all HTTP parsing, formatting, request/response handling, and the Api monad as pure Lune. Remove the `VApi` value constructor from the evaluator. Reorganise HTTP modules under `Lune.Http.*` and delete `Lune.Api.*`.

**Tech Stack:** Haskell (compiler), Lune (prelude/stdlib)

---

### Task 1: Add 4 char/string primitives to Builtins.hs

**Files:**
- Modify: `src/Lune/Builtins.hs` (builtinSchemes, builtinEvalPrims, and implementation sections)
- Modify: `src/Lune/Eval/Types.hs` (no changes expected, VChar already exists)

**Step 1: Add type schemes to `builtinSchemes`**

In `src/Lune/Builtins.hs`, after the existing string primitive schemes (around line 89-92), add:

```haskell
    , ("prim_stringToChars", Forall [] [] (TArrow (TCon "String") (TApp (TCon "List") (TCon "Char"))))
    , ("prim_charsToString", Forall [] [] (TArrow (TApp (TCon "List") (TCon "Char")) (TCon "String")))
    , ("prim_charToInt", Forall [] [] (TArrow (TCon "Char") (TCon "Int")))
    , ("prim_intToChar", Forall [] [] (TArrow (TCon "Int") (TCon "Char")))
```

**Step 2: Add eval prim entries to `builtinEvalPrims`**

After the existing string prim entries (around line 570):

```haskell
    , ("prim_stringToChars", BuiltinPrim 1 primStringToChars)
    , ("prim_charsToString", BuiltinPrim 1 primCharsToString)
    , ("prim_charToInt", BuiltinPrim 1 primCharToInt)
    , ("prim_intToChar", BuiltinPrim 1 primIntToChar)
```

**Step 3: Implement the 4 primitives**

Add near the other string primitive implementations:

```haskell
-- | prim_stringToChars : String -> List Char
primStringToChars :: [Value] -> Either EvalError Value
primStringToChars args =
  case args of
    [VString s] ->
      Right (listToValue (map VChar (T.unpack s)))
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primStringToChars args))

-- | prim_charsToString : List Char -> String
primCharsToString :: [Value] -> Either EvalError Value
primCharsToString args =
  case args of
    [v] ->
      case valueToList v of
        Nothing -> Left (NotARecord v)
        Just vals ->
          let chars = mapMaybe extractChar vals
          in Right (VString (T.pack chars))
    _ ->
      Left (NotAFunction (VPrim 1 primCharsToString args))
  where
    extractChar (VChar c) = Just c
    extractChar _ = Nothing

-- | prim_charToInt : Char -> Int
primCharToInt :: [Value] -> Either EvalError Value
primCharToInt args =
  case args of
    [VChar c] -> Right (VInt (fromIntegral (Char.ord c)))
    _ -> Left (NotAFunction (VPrim 1 primCharToInt args))

-- | prim_intToChar : Int -> Char
primIntToChar :: [Value] -> Either EvalError Value
primIntToChar args =
  case args of
    [VInt n] -> Right (VChar (Char.chr (fromIntegral n)))
    _ -> Left (NotAFunction (VPrim 1 primIntToChar args))
```

Ensure `Data.Char` is imported in Builtins.hs.

**Step 4: Build and verify the compiler compiles**

Run: `cabal build`
Expected: Successful build.

**Step 5: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat: add prim_stringToChars, prim_charsToString, prim_charToInt, prim_intToChar"
```

---

### Task 2: Build string utilities in `Lune.String` and `Lune.Char`

**Files:**
- Modify: `prelude/Lune/String.lune`
- Create: `prelude/Lune/Char.lune`

**Step 1: Create `Lune.Char` module**

Create `prelude/Lune/Char.lune`:

```lune
module Lune.Char exposing (
  toInt,
  fromInt,
  toLower,
  toUpper,
  isSpace,
  eq
)

import Lune.Prelude exposing (Int, Char, Bool(..))

toInt : Char -> Int
toInt = prim_charToInt

fromInt : Int -> Char
fromInt = prim_intToChar

toLower : Char -> Char
toLower c =
  let code = toInt c
  in case prim_and (prim_geInt code 65) (prim_leInt code 90) of
    True -> fromInt (prim_addInt code 32)
    False -> c

toUpper : Char -> Char
toUpper c =
  let code = toInt c
  in case prim_and (prim_geInt code 97) (prim_leInt code 122) of
    True -> fromInt (prim_subInt code 32)
    False -> c

isSpace : Char -> Bool
isSpace c =
  let code = toInt c
  in case prim_eqInt code 32 of
    True -> True
    False ->
      case prim_eqInt code 9 of
        True -> True
        False ->
          case prim_eqInt code 10 of
            True -> True
            False ->
              case prim_eqInt code 13 of
                True -> True
                False -> False

eq : Char -> Char -> Bool
eq a b = prim_eqInt (toInt a) (toInt b)
```

**Step 2: Expand `Lune.String` with string utilities**

Add to `prelude/Lune/String.lune`:

```lune
module Lune.String exposing (
  append, eq, fromInt, fromFloat, toInt,
  toChars, fromChars,
  length, isEmpty,
  split, join,
  toLower, toUpper,
  startsWith, endsWith, contains,
  trim, trimLeft, trimRight,
  drop, take,
  indexOf,
  lines, unlines,
  fromChar
)

import Lune.Prelude exposing (Bool(..), Int, Float, Char, String, Result(..), List(..), Maybe(..))
import Lune.Char as Char

append : String -> String -> String
append = prim_appendString

eq : String -> String -> Bool
eq = prim_eqString

fromInt : Int -> String
fromInt = prim_showInt

fromFloat : Float -> String
fromFloat = prim_showFloat

toInt : String -> Result String Int
toInt = prim_parseInt

toChars : String -> List Char
toChars = prim_stringToChars

fromChars : List Char -> String
fromChars = prim_charsToString

fromChar : Char -> String
fromChar c = fromChars (Cons c Nil)

length : String -> Int
length s = listLength (toChars s)

isEmpty : String -> Bool
isEmpty s =
  case toChars s of
    Nil -> True
    _ -> False

toLower : String -> String
toLower s = fromChars (listMap Char.toLower (toChars s))

toUpper : String -> String
toUpper s = fromChars (listMap Char.toUpper (toChars s))

startsWith : String -> String -> Bool
startsWith prefix s =
  charsStartsWith (toChars prefix) (toChars s)

endsWith : String -> String -> Bool
endsWith suffix s =
  charsStartsWith (listReverse (toChars suffix)) (listReverse (toChars s))

contains : String -> String -> Bool
contains needle haystack =
  charsContains (toChars needle) (toChars haystack)

split : String -> String -> List String
split delimiter s =
  case isEmpty delimiter of
    True -> Cons s Nil
    False ->
      splitHelper (toChars delimiter) (toChars s) Nil

join : String -> List String -> String
join sep parts =
  case parts of
    Nil -> ""
    Cons first rest ->
      listFoldl (\acc x -> append (append acc sep) x) first rest

trim : String -> String
trim s = trimLeft (trimRight s)

trimLeft : String -> String
trimLeft s = fromChars (dropWhileChars Char.isSpace (toChars s))

trimRight : String -> String
trimRight s = fromChars (listReverse (dropWhileChars Char.isSpace (listReverse (toChars s))))

drop : Int -> String -> String
drop n s = fromChars (listDrop n (toChars s))

take : Int -> String -> String
take n s = fromChars (listTake n (toChars s))

indexOf : String -> String -> Maybe Int
indexOf needle haystack =
  indexOfHelper (toChars needle) (toChars haystack) 0

lines : String -> List String
lines s = split "\n" s

unlines : List String -> String
unlines ls = join "\n" ls

fromChar : Char -> String
fromChar c = fromChars (Cons c Nil)

-- ===== Internal helpers =====

charsStartsWith : List Char -> List Char -> Bool
charsStartsWith prefix str =
  case prefix of
    Nil -> True
    Cons p ps ->
      case str of
        Nil -> False
        Cons s ss ->
          case Char.eq p s of
            True -> charsStartsWith ps ss
            False -> False

charsContains : List Char -> List Char -> Bool
charsContains needle haystack =
  case needle of
    Nil -> True
    _ ->
      case haystack of
        Nil -> False
        Cons _ rest ->
          case charsStartsWith needle haystack of
            True -> True
            False -> charsContains needle rest

splitHelper : List Char -> List Char -> List Char -> List String
splitHelper delim chars acc =
  case chars of
    Nil -> Cons (fromChars (listReverse acc)) Nil
    Cons c rest ->
      case charsStartsWith delim chars of
        True ->
          Cons (fromChars (listReverse acc))
               (splitHelper delim (listDrop (listLength delim) chars) Nil)
        False ->
          splitHelper delim rest (Cons c acc)

indexOfHelper : List Char -> List Char -> Int -> Maybe Int
indexOfHelper needle haystack idx =
  case needle of
    Nil -> Just idx
    _ ->
      case haystack of
        Nil -> Nothing
        Cons _ rest ->
          case charsStartsWith needle haystack of
            True -> Just idx
            False -> indexOfHelper needle rest (prim_addInt idx 1)

dropWhileChars : (Char -> Bool) -> List Char -> List Char
dropWhileChars pred chars =
  case chars of
    Nil -> Nil
    Cons c rest ->
      case pred c of
        True -> dropWhileChars pred rest
        False -> Cons c rest

listLength : List a -> Int
listLength xs =
  case xs of
    Nil -> 0
    Cons _ rest -> prim_addInt 1 (listLength rest)

listMap : (a -> b) -> List a -> List b
listMap f xs =
  case xs of
    Nil -> Nil
    Cons x rest -> Cons (f x) (listMap f rest)

listReverse : List a -> List a
listReverse xs = listReverseHelper xs Nil

listReverseHelper : List a -> List a -> List a
listReverseHelper xs acc =
  case xs of
    Nil -> acc
    Cons x rest -> listReverseHelper rest (Cons x acc)

listDrop : Int -> List a -> List a
listDrop n xs =
  case prim_leInt n 0 of
    True -> xs
    False ->
      case xs of
        Nil -> Nil
        Cons _ rest -> listDrop (prim_subInt n 1) rest

listTake : Int -> List a -> List a
listTake n xs =
  case prim_leInt n 0 of
    True -> Nil
    False ->
      case xs of
        Nil -> Nil
        Cons x rest -> Cons x (listTake (prim_subInt n 1) rest)

listFoldl : (b -> a -> b) -> b -> List a -> b
listFoldl f acc xs =
  case xs of
    Nil -> acc
    Cons x rest -> listFoldl f (f acc x) rest
```

**Step 3: Write a simple test example to verify string functions work**

Create `examples/38_String_Utils.lune`:

```lune
module StringUtils exposing (main)

import Lune.IO as IO
import Lune.String as Str
import Lune.Prelude exposing (IO, Unit, Maybe(..), List(..))

main : IO Unit
main =
  do
    IO.println (Str.toLower "HELLO")
    IO.println (Str.toUpper "hello")
    case Str.startsWith "GET" "GET /users" of
      True -> IO.println "startsWith: ok"
      False -> IO.println "startsWith: FAIL"
    case Str.split "&" "a=1&b=2&c=3" of
      Cons a (Cons b (Cons c Nil)) ->
        do
          IO.println a
          IO.println b
          IO.println c
      _ -> IO.println "split: FAIL"
    IO.println (Str.trim "  hello  ")
    IO.println (Str.join ", " (Cons "a" (Cons "b" (Cons "c" Nil))))
```

**Step 4: Run the test**

Run: `cabal run lune -- run examples/38_String_Utils.lune`
Expected output:
```
hello
HELLO
startsWith: ok
a=1
b=2
c=3
hello
a, b, c
```

**Step 5: Commit**

```bash
git add prelude/Lune/String.lune prelude/Lune/Char.lune examples/38_String_Utils.lune
git commit -m "feat: add Lune.Char module and expand Lune.String with split, toLower, trim, etc."
```

---

### Task 3: Rewrite HTTP parsing/formatting in pure Lune

This task replaces `prim_parseHttpRequest` and `prim_formatHttpResponse` with pure Lune implementations.

**Files:**
- Create: `prelude/Lune/Http/Internal.lune` (parsing/formatting helpers used by Server)

**Step 1: Create `Lune.Http.Internal`**

Create `prelude/Lune/Http/Internal.lune`:

```lune
module Lune.Http.Internal exposing (
  parseHttpRequest,
  formatHttpResponse,
  statusText,
  parseMethod
)

import Lune.Prelude exposing (String, Int, Result(..), List(..), Maybe(..), Bool(..))
import Lune.Http exposing (Request, Response, Method(..))
import Lune.String as Str

parseHttpRequest : String -> Result String Request
parseHttpRequest raw =
  let rawLines = Str.lines raw
      cleanLines = listMap stripCR rawLines
  in case cleanLines of
    Nil -> Err "empty request"
    Cons requestLine rest ->
      case parseRequestLine requestLine of
        Err e -> Err e
        Ok { method, path } ->
          let { headers, body } = splitHeadersBody rest
          in Ok { method = method
                , path = path
                , headers = headers
                , body = Str.trim body
                }

formatHttpResponse : Response -> String
formatHttpResponse resp =
  let statusLine = Str.append "HTTP/1.1 "
                     (Str.append (Str.fromInt resp.status)
                       (Str.append " " (statusText resp.status)))
      contentLength = Str.append "Content-Length: "
                        (Str.fromInt (Str.length resp.body))
      headerLines = listMap formatHeader resp.headers
      allLines = Cons statusLine
                   (Cons contentLength
                     (listAppend headerLines
                       (Cons "" (Cons resp.body Nil))))
  in Str.join "\n" allLines

statusText : Int -> String
statusText code =
  case prim_eqInt code 200 of
    True -> "OK"
    False -> case prim_eqInt code 201 of
      True -> "Created"
      False -> case prim_eqInt code 204 of
        True -> "No Content"
        False -> case prim_eqInt code 400 of
          True -> "Bad Request"
          False -> case prim_eqInt code 401 of
            True -> "Unauthorized"
            False -> case prim_eqInt code 403 of
              True -> "Forbidden"
              False -> case prim_eqInt code 404 of
                True -> "Not Found"
                False -> case prim_eqInt code 409 of
                  True -> "Conflict"
                  False -> case prim_eqInt code 500 of
                    True -> "Internal Server Error"
                    False -> "Unknown"

parseMethod : String -> Method
parseMethod m =
  let upper = Str.toUpper m
  in case Str.eq upper "GET" of
    True -> GET
    False -> case Str.eq upper "POST" of
      True -> POST
      False -> case Str.eq upper "PUT" of
        True -> PUT
        False -> case Str.eq upper "PATCH" of
          True -> PATCH
          False -> case Str.eq upper "DELETE" of
            True -> DELETE
            False -> GET

-- ===== Internal helpers =====

parseRequestLine : String -> Result String { method : Method, path : String }
parseRequestLine line =
  case splitOnSpace line of
    Cons methodStr (Cons path _) ->
      Ok { method = parseMethod methodStr, path = path }
    _ -> Err "invalid request line"

splitHeadersBody : List String -> { headers : List { key : String, value : String }, body : String }
splitHeadersBody lines =
  splitHBHelper lines Nil

splitHBHelper : List String -> List { key : String, value : String } -> { headers : List { key : String, value : String }, body : String }
splitHBHelper lines acc =
  case lines of
    Nil -> { headers = listReverse acc, body = "" }
    Cons line rest ->
      case Str.isEmpty (Str.trim line) of
        True -> { headers = listReverse acc, body = Str.join "\n" rest }
        False ->
          case parseHeaderLine line of
            Nothing -> splitHBHelper rest acc
            Just header -> splitHBHelper rest (Cons header acc)

parseHeaderLine : String -> Maybe { key : String, value : String }
parseHeaderLine line =
  case Str.indexOf ":" line of
    Nothing -> Nothing
    Just idx ->
      let key = Str.trim (Str.take idx line)
          value = Str.trim (Str.drop (prim_addInt idx 1) line)
      in Just { key = key, value = value }

formatHeader : { key : String, value : String } -> String
formatHeader h = Str.append h.key (Str.append ": " h.value)

splitOnSpace : String -> List String
splitOnSpace s = Str.split " " s

stripCR : String -> String
stripCR s =
  case Str.endsWith "\r" s of
    True -> Str.take (prim_subInt (Str.length s) 1) s
    False -> s

listMap : (a -> b) -> List a -> List b
listMap f xs =
  case xs of
    Nil -> Nil
    Cons x rest -> Cons (f x) (listMap f rest)

listReverse : List a -> List a
listReverse xs = listReverseHelper xs Nil

listReverseHelper : List a -> List a -> List a
listReverseHelper xs acc =
  case xs of
    Nil -> acc
    Cons x rest -> listReverseHelper rest (Cons x acc)

listAppend : List a -> List a -> List a
listAppend xs ys =
  case xs of
    Nil -> ys
    Cons x rest -> Cons x (listAppend rest ys)
```

**Step 2: Verify it compiles**

Run: `cabal run lune -- typecheck examples/17_Http_Parse.lune`
(We can't run it yet because the example still uses `prim_parseHttpRequest` directly.)

**Step 3: Commit**

```bash
git add prelude/Lune/Http/Internal.lune
git commit -m "feat: add Lune.Http.Internal with pure Lune HTTP parsing/formatting"
```

---

### Task 4: Rewrite query/header helpers in pure Lune

Replace `prim_parseQueryString`, `prim_getHeader`, and `prim_matchPath` usage in `Lune.Http.Request` and create a new `Lune.Http.Route` with path matching.

**Files:**
- Modify: `prelude/Lune/Http/Request.lune`

**Step 1: Rewrite `Lune.Http.Request` without primitives**

Replace `prelude/Lune/Http/Request.lune`:

```lune
module Lune.Http.Request exposing (
  query,
  header,
  queryParams
)

import Lune.Prelude exposing (String, Maybe(..), List(..), Bool(..))
import Lune.Http exposing (Request)
import Lune.String as Str

query : String -> Request -> Maybe String
query key req =
  findParam key (parseQueryString req.path)

header : String -> Request -> Maybe String
header name req =
  findHeader (Str.toLower name) req.headers

queryParams : Request -> List { key : String, value : String }
queryParams req =
  parseQueryString req.path

-- ===== Internal =====

parseQueryString : String -> List { key : String, value : String }
parseQueryString path =
  case Str.indexOf "?" path of
    Nothing -> Nil
    Just idx ->
      let qs = Str.drop (prim_addInt idx 1) path
      in case Str.isEmpty qs of
        True -> Nil
        False -> listMap parsePair (Str.split "&" qs)

parsePair : String -> { key : String, value : String }
parsePair p =
  case Str.indexOf "=" p of
    Nothing -> { key = p, value = "" }
    Just idx ->
      { key = Str.take idx p
      , value = Str.drop (prim_addInt idx 1) p
      }

findParam : String -> List { key : String, value : String } -> Maybe String
findParam key params =
  case params of
    Nil -> Nothing
    Cons p rest ->
      case Str.eq key p.key of
        True -> Just p.value
        False -> findParam key rest

findHeader : String -> List { key : String, value : String } -> Maybe String
findHeader lowerName headers =
  case headers of
    Nil -> Nothing
    Cons h rest ->
      case Str.eq lowerName (Str.toLower h.key) of
        True -> Just h.value
        False -> findHeader lowerName rest

listMap : (a -> b) -> List a -> List b
listMap f xs =
  case xs of
    Nil -> Nil
    Cons x rest -> Cons (f x) (listMap f rest)
```

**Step 2: Verify compilation**

Run: `cabal run lune -- typecheck examples/35_Request_Helpers.lune`
Expected: Typecheck succeeds.

**Step 3: Commit**

```bash
git add prelude/Lune/Http/Request.lune
git commit -m "feat: rewrite Lune.Http.Request without primitives"
```

---

### Task 5: Make Api a concrete type and move to `Lune.Http.Api`

The Api monad is currently `type Api e a = Api#` backed by the `VApi` Haskell value constructor. We need to make it a concrete Lune function type and remove `VApi` entirely.

**The key design decision:** `Api e a` becomes `type alias Api e a = ctx -> IO (Result e a)`. However, Lune's type alias system doesn't support functions with an unbound `ctx` type variable. Instead, we can use an opaque wrapper:

Actually, looking at the current Api usage, `ctx` is always provided via `run`. The simplest approach: keep `Api` as an opaque type (`Api#`) in the type system but change the runtime representation. The `VApi` constructor wraps `Value -> World -> IO (Either EvalError (World, Value))` — we can represent this as a Lune closure `ctx -> IO (Result e a)` instead.

**Simpler approach:** Since `Api` operations just chain IO + Result, we can define:

```
type Api e a = Api#
```

But implement the primitives in Lune using closures. The runtime still needs to know how to evaluate Api computations though.

**Simplest correct approach:** Redefine Api as a newtype wrapping a function. But Lune doesn't have newtypes — only algebraic types.

**Practical approach:** Define `Api` as a single-constructor ADT:

```lune
type Api e a = MkApi (ctx -> IO (Result e a))
```

Then all operations unwrap/rewrap. But `ctx` is unbound here — it needs to be a parameter. This gets complicated.

**Recommended approach:** Since the evaluator is a tree-walker and closures are first-class, the simplest path is:
1. Keep `type Api e a = Api#` in the Lune source (opaque runtime type)
2. Replace the Haskell `VApi` constructor with plain `VClosure`/`VPrim` — make apiPure, apiFail, etc. return closures that the runtime treats as regular values
3. `apiRun` applies the closure to the context and executes the resulting IO action
4. `apiAndThen` chains closures

Actually, the cleanest approach: **make Api a regular function type** by changing the Lune definition to a type alias and implementing everything with IO + Result. The Api "monad" becomes functions that return `IO (Result e a)`, and `run` just applies the context.

But we can't do `type alias Api e a = ctx -> IO (Result e a)` because `ctx` isn't bound.

**Final approach: Use a record to capture the context dependency.**

The most practical path given Lune's type system:

1. Remove `Api#` as a magic runtime type
2. Remove `VApi` from the evaluator
3. Remove `("Api", KArr KType (KArr KType KType))` from Kind.hs
4. Remove `Api` Functor/Applicative/Monad instances from Builtins.hs
5. Define `Api` as a **user-space type** in `Lune.Http.Api` using existing Lune features
6. Since `Api` is used with `do`-notation which desugars to `andThen`/`then`, we need it to be a Monad — but without typeclass instances, we can't use `do`. So `Api`-based code would need to use explicit `andThen` calls.

**Wait — re-check:** Looking at the examples, `Api` is used with explicit `Api.pure`, `Api.andThen`, `Api.run` — NOT with do-notation. The do-notation is used for `IO`, not `Api`. So we don't need `Api` to be a Monad instance at all. The typeclass instances in Builtins.hs are there but the examples don't rely on them through `do`.

So the plan is:
- Remove `Api#`, `VApi`, and the `Api` typeclass instances
- Remove `Api` from the kind environment
- Define `Api` as a simple function type: in Lune.Http.Api, all the Api "monad" operations just work with `IO (Result e a)` directly, and `run` provides the context.

Looking more carefully at the actual usage:
- `Api.run ctx computation` — runs computation with ctx
- `Api.pure value` — wraps a value
- `Api.fail error` — wraps an error
- `Api.jsonBody toErr decoder req` — parses, returns Api computation
- `Api.context` — gets the context
- `Api.andThen f ma` — chains computations

The `context` operation is the tricky one — it needs access to the `ctx` value that's provided by `run`. Without a runtime-backed closure, we need to thread `ctx` explicitly.

**Simplest pure-Lune approach:**

Remove the Api abstraction entirely. Instead:
- Handlers take `(Request, ctx)` and return `IO (Result e Response)`
- `jsonBody` returns a `Result` not an `Api`
- No separate Api monad at all

This is the cleanest YAGNI approach. The Api monad was wrapping IO+Result+reader, but since the context is already passed as a parameter to handlers, it's redundant.

**Files:**
- Create: `prelude/Lune/Http/Api.lune` — simplified, no Api monad. Just utility functions.
- Create: `prelude/Lune/Http/Route.lune` — route builders
- Modify: `src/Lune/Builtins.hs` — remove Api primitives, VApi, Api type schemes, Api instances, Api kind
- Modify: `src/Lune/Eval/Types.hs` — remove VApi constructor
- Modify: `src/Lune/Kind.hs` — remove Api from kind env
- Delete: `prelude/Lune/Api.lune`
- Delete: `prelude/Lune/Api/Route.lune`

**Step 1: Design the new handler pattern**

The new approach: handlers are just `Request -> ctx -> IO (Result e Response)`.

In `prelude/Lune/Http/Api.lune`:

```lune
module Lune.Http.Api exposing (
  serve,
  ServerConfig,
  Route,
  Routes,
  jsonBody,
  orFail,
  pure,
  fail
)

import Lune.Prelude exposing (IO, Result(..), Maybe(..), Unit, List(..), Int, Bool(..), String, unit)
import Lune.IO as IO
import Lune.Http exposing (Request, Response, Method(..))
import Lune.Http.Server as Server
import Lune.String as Str
import Lune.Json as Json
import Lune.Json.Decode as D

type alias ServerConfig e ctx =
  { port : Int
  , errorHandler : e -> Response
  , context : ctx
  }

type alias Route e ctx =
  { method : Method
  , pattern : String
  , handler : Request -> ctx -> IO (Result e Response)
  }

type alias Routes e ctx = List (Route e ctx)

pure : a -> IO (Result e a)
pure a = IO.pure (Ok a)

fail : e -> IO (Result e a)
fail e = IO.pure (Err e)

orFail : e -> Maybe a -> IO (Result e a)
orFail err maybe =
  case maybe of
    Nothing -> fail err
    Just a -> pure a

jsonBody : (String -> e) -> D.Decoder a -> Request -> Result e a
jsonBody toErr decoder request =
  case Json.parse request.body of
    Err msg -> Err (toErr msg)
    Ok json ->
      case D.decodeValue decoder json of
        Err decodeErr -> Err (toErr decodeErr.message)
        Ok value -> Ok value

serve : ServerConfig e ctx -> Routes e ctx -> IO Unit
serve config routes =
  Server.serve config.port (handleRequest config routes)

handleRequest : ServerConfig e ctx -> Routes e ctx -> Request -> IO Response
handleRequest config routes request =
  case findMatchingRoute request routes of
    Nothing ->
      IO.pure notFoundResponse
    Just route ->
      do
        result <- route.handler request config.context
        case result of
          Err e -> IO.pure (config.errorHandler e)
          Ok response -> IO.pure response

findMatchingRoute : Request -> Routes e ctx -> Maybe (Route e ctx)
findMatchingRoute request routes =
  case routes of
    Nil -> Nothing
    Cons route rest ->
      case methodMatches request.method route.method of
        False -> findMatchingRoute request rest
        True ->
          case matchPath route.pattern request.path of
            False -> findMatchingRoute request rest
            True -> Just route

methodMatches : Method -> Method -> Bool
methodMatches m1 m2 =
  case m1 of
    GET -> case m2 of
      GET -> True
      _ -> False
    POST -> case m2 of
      POST -> True
      _ -> False
    PUT -> case m2 of
      PUT -> True
      _ -> False
    PATCH -> case m2 of
      PATCH -> True
      _ -> False
    DELETE -> case m2 of
      DELETE -> True
      _ -> False

matchPath : String -> String -> Bool
matchPath pattern rawPath =
  let path = stripQueryString rawPath
      patternParts = filterEmpty (Str.split "/" pattern)
      pathParts = filterEmpty (Str.split "/" path)
  in matchParts patternParts pathParts

stripQueryString : String -> String
stripQueryString path =
  case Str.indexOf "?" path of
    Nothing -> path
    Just idx -> Str.take idx path

matchParts : List String -> List String -> Bool
matchParts ps vs =
  case ps of
    Nil ->
      case vs of
        Nil -> True
        _ -> False
    Cons p prest ->
      case vs of
        Nil -> False
        Cons v vrest ->
          case Str.startsWith ":" p of
            True -> matchParts prest vrest
            False ->
              case Str.eq p v of
                True -> matchParts prest vrest
                False -> False

filterEmpty : List String -> List String
filterEmpty xs =
  case xs of
    Nil -> Nil
    Cons x rest ->
      case Str.isEmpty x of
        True -> filterEmpty rest
        False -> Cons x (filterEmpty rest)

notFoundResponse : Response
notFoundResponse =
  { status = 404
  , headers = [{ key = "Content-Type", value = "application/json" }]
  , body = "{\"error\":\"Not Found\"}"
  }
```

**Step 2: Create `Lune.Http.Route`**

Create `prelude/Lune/Http/Route.lune`:

```lune
module Lune.Http.Route exposing (
  get,
  post,
  put,
  patch,
  delete,
  define
)

import Lune.Prelude exposing (String, IO, Result, List)
import Lune.Http exposing (Method(..), Request, Response)
import Lune.Http.Api exposing (Route, Routes)

get : String -> (Request -> ctx -> IO (Result e Response)) -> Route e ctx
get pattern handler =
  { method = GET, pattern = pattern, handler = handler }

post : String -> (Request -> ctx -> IO (Result e Response)) -> Route e ctx
post pattern handler =
  { method = POST, pattern = pattern, handler = handler }

put : String -> (Request -> ctx -> IO (Result e Response)) -> Route e ctx
put pattern handler =
  { method = PUT, pattern = pattern, handler = handler }

patch : String -> (Request -> ctx -> IO (Result e Response)) -> Route e ctx
patch pattern handler =
  { method = PATCH, pattern = pattern, handler = handler }

delete : String -> (Request -> ctx -> IO (Result e Response)) -> Route e ctx
delete pattern handler =
  { method = DELETE, pattern = pattern, handler = handler }

define : List (Route e ctx) -> Routes e ctx
define routes = routes
```

**Step 3: Remove Api primitives from Builtins.hs**

In `src/Lune/Builtins.hs`:

1. Remove from `builtinSchemes`: `prim_apiRun`, `prim_apiContext`, `prim_apiFail`, `prim_apiMapError`, `prim_apiPure`, `prim_apiAndThen`, `prim_matchPath`, `prim_parseQueryString`, `prim_getHeader`, `prim_parseHttpRequest`, `prim_formatHttpResponse`
2. Remove from `builtinEvalPrims`: all 11 HTTP entries (lines 639-652)
3. Remove from `builtinInstanceDicts`: the 3 Api entries (Functor/Applicative/Monad for Api)
4. Remove from `builtinCoreDecls`: `dictFunctorApi`, `dictApplicativeApi`, `dictMonadApi`
5. Remove `httpRequestType`, `httpResponseType`, `httpHeaderType` helper definitions
6. Remove all implementation functions: `primParseHttpRequest`, `primFormatHttpResponse`, `parseHttpRequest`, `parseMethod`, `parseHeader`, `headerToValue`, `valueToHeaderList`, `formatHttpResponse`, `statusText`, `primApiRun`, `primApiContext`, `primApiFail`, `primApiMapError`, `primApiPure`, `primApiAndThen`, `primMatchPath`, `primParseQueryString`, `primGetHeader`

**Step 4: Remove VApi from Eval/Types.hs**

In `src/Lune/Eval/Types.hs`:
- Remove `| VApi (Value -> World -> IO (Either EvalError (World, Value)))` from the `Value` data type
- Remove the `VApi _` case from the `Show` instance

**Step 5: Remove Api from Kind.hs**

In `src/Lune/Kind.hs`:
- Remove `("Api", KArr KType (KArr KType KType))` from the builtin kind environment

**Step 6: Build and verify**

Run: `cabal build`
Expected: Successful build with no references to removed primitives.

**Step 7: Commit**

```bash
git add src/Lune/Builtins.hs src/Lune/Eval/Types.hs src/Lune/Kind.hs prelude/Lune/Http/Api.lune prelude/Lune/Http/Route.lune
git commit -m "feat: remove Api primitives and VApi, reimplement as pure Lune"
```

---

### Task 6: Update `Lune.Http.Server` to use pure Lune parsing

**Files:**
- Modify: `prelude/Lune/Http/Server.lune`

**Step 1: Replace prim calls with Lune.Http.Internal calls**

Replace `prelude/Lune/Http/Server.lune`:

```lune
module Lune.Http.Server exposing (
  serve,
  Handler
)

import Lune.Prelude exposing (IO, Unit, Result(..), Int, String, unit)
import Lune.IO as IO
import Lune.Net.Socket as Socket
import Lune.Net.Socket exposing (Socket, Connection)
import Lune.Http exposing (Request, Response)
import Lune.Http.Internal as Internal
import Lune.String as Str

type alias Handler = Request -> IO Response

serve : Int -> Handler -> IO Unit
serve port handler =
  do
    IO.println (Str.append "Starting HTTP server on port " (Str.fromInt port))
    socketResult <- Socket.listen port
    case socketResult of
      Err _ ->
        IO.println "Failed to start server"
      Ok sock ->
        acceptLoop sock handler

acceptLoop : Socket -> Handler -> IO Unit
acceptLoop sock handler =
  do
    connResult <- Socket.accept sock
    case connResult of
      Err _ ->
        IO.println "Accept failed, stopping server"
      Ok conn ->
        do
          handleConnection conn handler
          acceptLoop sock handler

handleConnection : Connection -> Handler -> IO Unit
handleConnection conn handler =
  do
    recvResult <- Socket.recv conn
    case recvResult of
      Err _ ->
        do
          _ <- Socket.closeConn conn
          IO.pure unit
      Ok rawRequest ->
        case Internal.parseHttpRequest rawRequest of
          Err _ ->
            do
              _ <- Socket.send conn "HTTP/1.1 400 Bad Request\r\n\r\n"
              _ <- Socket.closeConn conn
              IO.pure unit
          Ok request ->
            do
              response <- handler request
              let rawResponse = Internal.formatHttpResponse response
              _ <- Socket.send conn rawResponse
              _ <- Socket.closeConn conn
              IO.pure unit
```

**Step 2: Commit**

```bash
git add prelude/Lune/Http/Server.lune
git commit -m "feat: update Http.Server to use pure Lune parsing"
```

---

### Task 7: Delete old modules and update examples

**Files:**
- Delete: `prelude/Lune/Api.lune`
- Delete: `prelude/Lune/Api/Route.lune`
- Modify: `examples/17_Http_Parse.lune`
- Modify: `examples/18_Api_Server.lune`
- Modify: `examples/35_Request_Helpers.lune`
- Modify: `examples/37_Api_JsonBody.lune`
- (36_Response_Helpers.lune — no changes needed, it only uses `Lune.Http` which is unchanged)

**Step 1: Delete old modules**

```bash
rm prelude/Lune/Api.lune
rm -r prelude/Lune/Api/
```

**Step 2: Update example 17 — Http Parse**

The example currently calls `prim_parseHttpRequest` directly. Update it to use `Lune.Http.Internal`:

```lune
module HttpParseTest exposing (main)

import Lune.IO as IO
import Lune.Http as Http
import Lune.Http.Internal as Internal
import Lune.String as Str
import Lune.Prelude exposing (Result(..), IO, Unit)

main : IO Unit
main =
  do
    IO.println "=== HTTP Parse Test ==="
    testParseGet
    testParsePost

testParseGet : IO Unit
testParseGet =
  case Internal.parseHttpRequest "GET /users/123 HTTP/1.1\r\nHost: localhost\r\n\r\n" of
    Err e -> IO.println (Str.append "Parse error: " e)
    Ok req ->
      do
        IO.println (Str.append "Path: " req.path)
        IO.println "GET request parsed successfully"

testParsePost : IO Unit
testParsePost =
  case Internal.parseHttpRequest "POST /users HTTP/1.1\r\nContent-Type: application/json\r\n\r\n{\"name\":\"Alice\"}" of
    Err e -> IO.println (Str.append "Parse error: " e)
    Ok req ->
      do
        IO.println (Str.append "Path: " req.path)
        IO.println (Str.append "Body: " req.body)
        IO.println "POST request parsed successfully"
```

**Step 3: Update example 18 — Api Server**

The handler type changes from `Api e Response` to `IO (Result e Response)`:

```lune
module ApiServer exposing (main)

import Lune.IO as IO
import Lune.Http.Api as Api
import Lune.Http.Api exposing (ServerConfig, Routes)
import Lune.Http.Route as Route
import Lune.Http exposing (Request, Response, Method(..))
import Lune.Json as Json
import Lune.Json.Encode as E
import Lune.String as Str
import Lune.Prelude exposing (IO, Result(..), Unit, Maybe(..), Int, String)

type AppError =
  NotFound String
  | BadRequest String

type alias Context =
  { appName : String
  }

type alias User =
  { id : Int
  , name : String
  }

encodeUser : User -> Json.Json
encodeUser user =
  E.object
    [ { key = "id", value = E.int user.id }
    , { key = "name", value = E.string user.name }
    ]

type alias HealthStatus =
  { status : String
  }

encodeHealth : HealthStatus -> Json.Json
encodeHealth h =
  E.object
    [ { key = "status", value = E.string h.status }
    ]

type alias ErrorResponse =
  { error : String
  }

encodeError : ErrorResponse -> Json.Json
encodeError e =
  E.object
    [ { key = "error", value = E.string e.error }
    ]

jsonResponse : Int -> Json.Json -> Response
jsonResponse status body =
  { status = status
  , headers = [{ key = "Content-Type", value = "application/json" }]
  , body = Json.stringify body
  }

errorToResponse : AppError -> Response
errorToResponse err =
  case err of
    NotFound msg ->
      jsonResponse 404 (encodeError { error = msg })
    BadRequest msg ->
      jsonResponse 400 (encodeError { error = msg })

routes : Routes AppError Context
routes =
  Route.define
    [ Route.get "/health" healthHandler
    , Route.get "/users/:id" getUserHandler
    , Route.post "/users" createUserHandler
    ]

healthHandler : Request -> Context -> IO (Result AppError Response)
healthHandler request ctx =
  Api.pure (jsonResponse 200 (encodeHealth { status = "ok" }))

getUserHandler : Request -> Context -> IO (Result AppError Response)
getUserHandler request ctx =
  Api.pure (jsonResponse 200 (encodeUser { id = 123, name = "Alice" }))

createUserHandler : Request -> Context -> IO (Result AppError Response)
createUserHandler request ctx =
  Api.pure (jsonResponse 201 (encodeUser { id = 456, name = "Created" }))

config : ServerConfig AppError Context
config =
  { port = 8080
  , errorHandler = errorToResponse
  , context = { appName = "MyApp" }
  }

main : IO Unit
main = Api.serve config routes
```

**Step 4: Update example 35 — Request Helpers**

No changes needed — it imports `Lune.Http.Request` which has the same API.

**Step 5: Update example 37 — Api JsonBody**

The `Api.run` and `Api.jsonBody` API changes. `jsonBody` now returns `Result` not `Api`, and there's no `Api.run`:

```lune
module ApiJsonBody exposing (main)

import Lune.IO as IO
import Lune.Http.Api as Api
import Lune.Http exposing (Method(..))
import Lune.Json as Json
import Lune.Json.Decode as D
import Lune.String as Str
import Lune.Prelude exposing (IO, Unit, Result(..), Int, String)

type AppError = ParseError String

userDecoder : D.Decoder { name : String, age : Int }
userDecoder =
  D.map2 (\n a -> { name = n, age = a })
    (D.field "name" D.string)
    (D.field "age" D.int)

main : IO Unit
main =
  do
    let goodReq = { method = GET, path = "/", headers = [], body = "{\"name\":\"Alice\",\"age\":30}" }
    case Api.jsonBody ParseError userDecoder goodReq of
      Ok user -> IO.println (Str.append user.name (Str.append " age " (Str.fromInt user.age)))
      Err (ParseError msg) -> IO.println (Str.append "ERROR: " msg)
    let badReq = { method = GET, path = "/", headers = [], body = "{\"name\":\"Bob\"}" }
    case Api.jsonBody ParseError userDecoder badReq of
      Ok user -> IO.println "unexpected success"
      Err (ParseError msg) -> IO.println "parse failed as expected"
```

**Step 6: Run all examples to verify**

Run each example:
```bash
cabal run lune -- run examples/17_Http_Parse.lune
cabal run lune -- run examples/35_Request_Helpers.lune
cabal run lune -- run examples/36_Response_Helpers.lune
cabal run lune -- run examples/37_Api_JsonBody.lune
```

Expected: Same output as before (check against golden files).

Note: Example 18 (ApiServer) starts a server and doesn't terminate, so just typecheck it:
```bash
cabal run lune -- typecheck examples/18_Api_Server.lune
```

**Step 7: Commit**

```bash
git add -A
git commit -m "feat: delete Lune.Api modules, update examples for new Lune.Http.Api"
```

---

### Task 8: Regenerate golden test files

**Files:**
- Modify: All golden files under `tests/golden/core/` and `tests/golden/parse/` for affected examples

**Step 1: Regenerate golden files**

The golden tests compare compiler output. Since module paths and prim references changed, regenerate:

```bash
cabal run lune -- core examples/17_Http_Parse.lune > tests/golden/core/17_Http_Parse.golden
cabal run lune -- core examples/18_Api_Server.lune > tests/golden/core/18_Api_Server.golden
cabal run lune -- core examples/35_Request_Helpers.lune > tests/golden/core/35_Request_Helpers.golden
cabal run lune -- core examples/36_Response_Helpers.lune > tests/golden/core/36_Response_Helpers.golden
cabal run lune -- core examples/37_Api_JsonBody.lune > tests/golden/core/37_Api_JsonBody.golden

cabal run lune -- parse examples/17_Http_Parse.lune > tests/golden/parse/17_Http_Parse.golden
cabal run lune -- parse examples/18_Api_Server.lune > tests/golden/parse/18_Api_Server.golden
cabal run lune -- parse examples/35_Request_Helpers.lune > tests/golden/parse/35_Request_Helpers.golden
cabal run lune -- parse examples/36_Response_Helpers.lune > tests/golden/parse/36_Response_Helpers.golden
cabal run lune -- parse examples/37_Api_JsonBody.lune > tests/golden/parse/37_Api_JsonBody.golden
```

Also generate golden files for the new string utils test:
```bash
cabal run lune -- core examples/38_String_Utils.lune > tests/golden/core/38_String_Utils.golden
cabal run lune -- parse examples/38_String_Utils.lune > tests/golden/parse/38_String_Utils.golden
```

**Step 2: Run the full test suite**

```bash
cabal test
```

Expected: All tests pass.

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: regenerate golden files after HTTP primitive removal"
```

---

### Task 9: Clean up — remove any remaining references

**Files:**
- Check: `spec/lune_web_framework_v0_1.md` — update if references are stale
- Check: `docs/plans/2026-02-05-web-framework-enhancements.md` — leave as historical

**Step 1: Search for any remaining references to removed primitives**

```bash
grep -r "prim_parseHttpRequest\|prim_formatHttpResponse\|prim_apiRun\|prim_apiContext\|prim_apiFail\|prim_apiMapError\|prim_apiPure\|prim_apiAndThen\|prim_matchPath\|prim_parseQueryString\|prim_getHeader\|VApi\|Api#" src/ prelude/ examples/
```

Expected: No matches.

**Step 2: Search for any remaining imports of old modules**

```bash
grep -r "import Lune.Api" prelude/ examples/
```

Expected: No matches (should all be `import Lune.Http.Api` now).

**Step 3: Run the full build and test suite one final time**

```bash
cabal build && cabal test
```

Expected: Clean build, all tests pass.

**Step 4: Commit any remaining cleanup**

```bash
git add -A
git commit -m "chore: remove remaining references to HTTP primitives"
```

---

## Summary of Changes

| What | Before | After |
|------|--------|-------|
| **New Haskell prims** | — | 4 (`stringToChars`, `charsToString`, `charToInt`, `intToChar`) |
| **Removed Haskell prims** | 11 HTTP-specific | 0 |
| **Removed Haskell types** | `VApi` constructor | Gone |
| **Removed kind entries** | — | `Api` removed from kind env |
| **Removed typeclass dicts** | — | `Functor/Applicative/Monad Api` removed |
| **New Lune modules** | — | `Lune.Char`, `Lune.Http.Internal`, `Lune.Http.Api`, `Lune.Http.Route` |
| **Deleted Lune modules** | — | `Lune.Api`, `Lune.Api.Route` |
| **Modified Lune modules** | — | `Lune.String`, `Lune.Http.Request`, `Lune.Http.Server` |
| **Api monad** | Opaque `Api#` + `VApi` + 6 prims | Removed. Handlers return `IO (Result e Response)` directly |
| **Net primitive count** | +4 general, -11 domain-specific = **7 fewer primitives** |
