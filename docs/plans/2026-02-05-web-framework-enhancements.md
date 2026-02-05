# Web Framework Enhancements Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Bring the Lune web framework from "basic JSON API" to "production-usable" by adding middleware, request body/query parsing, response helpers, CORS, and route groups.

**Architecture:** All new features are pure Lune prelude modules layered on existing primitives. Two new Haskell primitives are needed: `prim_parseQueryString` (URL query parsing) and `prim_getHeader` (case-insensitive header lookup). A bug fix is also needed in `prim_matchPath` to strip query strings before matching. Middleware is a function type `(Request -> Api e Response) -> Request -> Api e Response` composed via plain function application. No new runtime value types are needed.

**Tech Stack:** Lune prelude modules, Haskell runtime primitives (in `src/Lune/Builtins.hs`), golden tests

---

## Task 1: Fix `prim_matchPath` to strip query strings

The current `prim_matchPath` splits the raw path on `/` without stripping `?query=string`. A request to `/users?page=1` won't match route pattern `/users`.

**Files:**
- Modify: `src/Lune/Builtins.hs:2340-2365` (the `primMatchPath` function)

**Step 1: Write a golden test**

Create `examples/32_Query_Route_Match.lune`:

```lune
module QueryRouteMatch exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Unit, Maybe(..))

main : IO Unit
main =
  do
    let result = prim_matchPath "/users/:id" "/users/42?format=json"
    case result of
      Nothing -> IO.println "NO MATCH"
      Just params -> IO.println "MATCHED"
```

**Step 2: Run test to verify it currently fails**

Run: `cabal run -v0 lune -- --eval examples/32_Query_Route_Match.lune`
Expected: `NO MATCH` (the bug — it should say `MATCHED`)

**Step 3: Fix `primMatchPath` to strip query string before matching**

In `src/Lune/Builtins.hs`, modify `primMatchPath`:

```haskell
primMatchPath :: [Value] -> Either EvalError Value
primMatchPath args =
  case args of
    [VString pattern, VString rawPath] ->
      -- Strip query string before matching
      let path = fst (T.breakOn "?" rawPath)
          patternParts = filter (not . T.null) $ T.splitOn "/" pattern
          pathParts = filter (not . T.null) $ T.splitOn "/" path
      in case matchParts patternParts pathParts of
        Nothing -> Right (VCon (preludeCon "Nothing") [])
        Just params -> Right (VCon (preludeCon "Just") [listToValue (map paramToValue params)])
    _ ->
      Left (NotAFunction (VPrim 2 primMatchPath args))
  where
    -- (matchParts, paramToValue unchanged)
```

**Step 4: Run test to verify it passes**

Run: `cabal run -v0 lune -- --eval examples/32_Query_Route_Match.lune`
Expected: `MATCHED`

**Step 5: Generate golden files and commit**

```bash
cabal test golden --test-options="--accept"
cabal test golden
git add -A && git commit -m "fix: strip query string in prim_matchPath before route matching"
```

---

## Task 2: Add `prim_parseQueryString` and `prim_getHeader` primitives

These two primitives enable query parameter access and header lookup from Lune code. Implementing them in Haskell avoids needing string splitting primitives in Lune.

**Files:**
- Modify: `src/Lune/Builtins.hs` (add to `builtinSchemes`, `builtinEvalEnv`, and implementation section)

**Step 1: Write a golden test for query parsing**

Create `examples/33_Query_Params.lune`:

```lune
module QueryParams exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Unit, Maybe(..), List(..))

main : IO Unit
main =
  do
    let params = prim_parseQueryString "/search?q=hello&page=2&sort=asc"
    printParams params

printParams : List { key : String, value : String } -> IO Unit
printParams ps =
  case ps of
    [] -> IO.println "done"
    Cons p rest ->
      do
        IO.println (prim_appendString p.key (prim_appendString "=" p.value))
        printParams rest
```

**Step 2: Write a golden test for header lookup**

Create `examples/34_Get_Header.lune`:

```lune
module GetHeader exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Unit, Maybe(..))

main : IO Unit
main =
  do
    let headers =
          [ { key = "Content-Type", value = "application/json" }
          , { key = "Authorization", value = "Bearer abc123" }
          ]
    case prim_getHeader "content-type" headers of
      Nothing -> IO.println "not found"
      Just v -> IO.println v
    case prim_getHeader "authorization" headers of
      Nothing -> IO.println "not found"
      Just v -> IO.println v
    case prim_getHeader "x-missing" headers of
      Nothing -> IO.println "not found"
      Just v -> IO.println v
```

**Step 3: Run tests to verify they fail**

Run: `cabal build`
Expected: Compilation errors — primitives don't exist yet.

**Step 4: Add type signatures to `builtinSchemes`**

In `src/Lune/Builtins.hs`, add after the `prim_matchPath` type signature (around line 207):

```haskell
    , ("prim_parseQueryString", Forall [] []
        (TArrow (TCon "String")
          (TApp (TCon "List")
            (TRecord [("key", TCon "String"), ("value", TCon "String")]))))
    , ("prim_getHeader", Forall [] []
        (TArrow (TCon "String")
          (TArrow
            (TApp (TCon "List")
              (TRecord [("key", TCon "String"), ("value", TCon "String")]))
            (TApp (TCon "Maybe") (TCon "String")))))
```

**Step 5: Add entries to `builtinEvalEnv`**

In the eval env list (around line 638):

```haskell
    , ("prim_parseQueryString", BuiltinPrim 1 primParseQueryString)
    , ("prim_getHeader", BuiltinPrim 2 primGetHeader)
```

**Step 6: Implement the primitives**

Add after the `primMatchPath` function:

```haskell
-- | prim_parseQueryString : String -> List { key : String, value : String }
-- Parses query parameters from a URL path like "/foo?a=1&b=2"
-- Returns the key-value pairs. If no query string, returns empty list.
primParseQueryString :: [Value] -> Either EvalError Value
primParseQueryString args =
  case args of
    [VString rawPath] ->
      let queryStr = case T.breakOn "?" rawPath of
            (_, rest) | T.null rest -> ""
                      | otherwise -> T.drop 1 rest
          pairs = if T.null queryStr then []
                  else map parsePair (T.splitOn "&" queryStr)
      in Right (listToValue (map pairToValue pairs))
    _ ->
      Left (NotAFunction (VPrim 1 primParseQueryString args))
  where
    parsePair :: Text -> (Text, Text)
    parsePair p =
      case T.breakOn "=" p of
        (k, rest) | T.null rest -> (k, "")
                  | otherwise -> (k, T.drop 1 rest)

    pairToValue :: (Text, Text) -> Value
    pairToValue (k, v) = VRecord $ Map.fromList [("key", VString k), ("value", VString v)]

-- | prim_getHeader : String -> List { key : String, value : String } -> Maybe String
-- Case-insensitive header lookup.
primGetHeader :: [Value] -> Either EvalError Value
primGetHeader args =
  case args of
    [VString name, headersVal] ->
      let headers = valueToHeaderList headersVal
          lowerName = T.toLower name
          found = lookup lowerName [(T.toLower k, v) | (k, v) <- headers]
      in case found of
        Nothing -> Right (VCon (preludeCon "Nothing") [])
        Just v -> Right (VCon (preludeCon "Just") [VString v])
    _ ->
      Left (NotAFunction (VPrim 2 primGetHeader args))
```

**Step 7: Build and run the tests**

```bash
cabal build
cabal run -v0 lune -- --eval examples/33_Query_Params.lune
cabal run -v0 lune -- --eval examples/34_Get_Header.lune
```

Expected for 33: `q=hello`, `page=2`, `sort=asc`, `done`
Expected for 34: `application/json`, `Bearer abc123`, `not found`

**Step 8: Generate golden files and commit**

```bash
cabal test golden --test-options="--accept"
cabal test golden
git add -A && git commit -m "feat: add prim_parseQueryString and prim_getHeader primitives"
```

---

## Task 3: Add `Lune.Http.Request` helper module

Expose query params, header lookup, and path params as a clean Lune API that handlers can use.

**Files:**
- Create: `prelude/Lune/Http/Request.lune`

**Step 1: Write a golden test**

Create `examples/35_Request_Helpers.lune`:

```lune
module RequestHelpers exposing (main)

import Lune.IO as IO
import Lune.Http.Request as Req
import Lune.Prelude exposing (IO, Unit, Maybe(..))

main : IO Unit
main =
  do
    let req =
          { method = prim_unused
          , path = "/users/42?format=json&verbose=true"
          , headers =
              [ { key = "Content-Type", value = "application/json" }
              , { key = "Authorization", value = "Bearer token123" }
              ]
          , body = "{\"name\":\"Alice\"}"
          }
    -- Test query params
    case Req.query "format" req of
      Nothing -> IO.println "format: not found"
      Just v -> IO.println (prim_appendString "format: " v)
    case Req.query "verbose" req of
      Nothing -> IO.println "verbose: not found"
      Just v -> IO.println (prim_appendString "verbose: " v)
    case Req.query "missing" req of
      Nothing -> IO.println "missing: not found"
      Just v -> IO.println (prim_appendString "missing: " v)
    -- Test header lookup
    case Req.header "content-type" req of
      Nothing -> IO.println "ct: not found"
      Just v -> IO.println (prim_appendString "ct: " v)
    case Req.header "authorization" req of
      Nothing -> IO.println "auth: not found"
      Just v -> IO.println (prim_appendString "auth: " v)
```

Note: This example may need adjustment depending on whether we can construct a partial Request record without a valid Method value. If the typechecker requires a full record, use `GET` from `Lune.Http`:

```lune
import Lune.Http exposing (Method(..))
-- ...
    let req =
          { method = GET
          , path = "/users/42?format=json&verbose=true"
```

**Step 2: Run test to verify it fails**

Run: `cabal run -v0 lune -- --eval examples/35_Request_Helpers.lune`
Expected: Module not found error for `Lune.Http.Request`.

**Step 3: Create the module**

Create `prelude/Lune/Http/Request.lune`:

```lune
module Lune.Http.Request exposing (
  query,
  header,
  queryParams
)

import Lune.Prelude exposing (String, Maybe(..), List(..))
import Lune.Http exposing (Request)

{-| Look up a query parameter by key from the request URL.
    Returns Nothing if the parameter is not present.
-}
query : String -> Request -> Maybe String
query key req =
  findParam key (prim_parseQueryString req.path)

{-| Look up a header value by name (case-insensitive).
-}
header : String -> Request -> Maybe String
header name req =
  prim_getHeader name req.headers

{-| Get all query parameters from the request URL.
-}
queryParams : Request -> List { key : String, value : String }
queryParams req =
  prim_parseQueryString req.path

findParam : String -> List { key : String, value : String } -> Maybe String
findParam key params =
  case params of
    [] -> Nothing
    Cons p rest ->
      case prim_eqString key p.key of
        True -> Just p.value
        False -> findParam key rest
```

**Step 4: Run test to verify it passes**

Run: `cabal run -v0 lune -- --eval examples/35_Request_Helpers.lune`
Expected output:
```
format: json
verbose: true
missing: not found
ct: application/json
auth: Bearer token123
```

**Step 5: Generate golden files and commit**

```bash
cabal test golden --test-options="--accept"
cabal test golden
git add -A && git commit -m "feat: add Lune.Http.Request module with query, header, queryParams"
```

---

## Task 4: Add response helpers to `Lune.Http`

Add convenience constructors for common HTTP responses. These are all pure functions, no runtime changes needed.

**Files:**
- Modify: `prelude/Lune/Http.lune` (add new exports and functions)

**Step 1: Write a golden test**

Create `examples/36_Response_Helpers.lune`:

```lune
module ResponseHelpers exposing (main)

import Lune.IO as IO
import Lune.Http as Http
import Lune.Json.Encode as E
import Lune.Json as Json
import Lune.String as Str
import Lune.Prelude exposing (IO, Unit)

main : IO Unit
main =
  do
    let r1 = Http.ok "hello"
    IO.println (Str.fromInt r1.status)
    let r2 = Http.created "new resource"
    IO.println (Str.fromInt r2.status)
    let r3 = Http.noContent
    IO.println (Str.fromInt r3.status)
    let r4 = Http.notFound "gone"
    IO.println (Str.fromInt r4.status)
    let r5 = Http.badRequest "nope"
    IO.println (Str.fromInt r5.status)
    let r6 = Http.unauthorized "denied"
    IO.println (Str.fromInt r6.status)
    let r7 = Http.redirect "/new-place"
    IO.println (Str.fromInt r7.status)
    let json = E.object [{ key = "id", value = E.int 1 }]
    let r8 = Http.jsonOk json
    IO.println (Str.fromInt r8.status)
    IO.println r8.body
```

**Step 2: Run test to verify it fails**

Run: `cabal run -v0 lune -- --eval examples/36_Response_Helpers.lune`
Expected: Unbound variable errors — `Http.ok` etc. don't exist yet.

**Step 3: Add response helpers to `Lune.Http`**

Modify `prelude/Lune/Http.lune` — update the exposing list and add:

```lune
module Lune.Http exposing (
  Method(..),
  Request,
  Response,
  response,
  jsonResponse,
  ok,
  created,
  noContent,
  badRequest,
  unauthorized,
  forbidden,
  notFound,
  conflict,
  serverError,
  redirect,
  jsonOk,
  jsonCreated,
  withHeader
)

import Lune.Prelude exposing (String, Int, List(..), Maybe)
import Lune.Json exposing (Json)

-- (existing Method, Request, Response, response, jsonResponse unchanged)

ok : String -> Response
ok body = response 200 body

created : String -> Response
created body = response 201 body

noContent : Response
noContent = { status = 204, headers = [], body = "" }

badRequest : String -> Response
badRequest body = response 400 body

unauthorized : String -> Response
unauthorized body = response 401 body

forbidden : String -> Response
forbidden body = response 403 body

notFound : String -> Response
notFound body = response 404 body

conflict : String -> Response
conflict body = response 409 body

serverError : String -> Response
serverError body = response 500 body

redirect : String -> Response
redirect url =
  { status = 302
  , headers = [{ key = "Location", value = url }]
  , body = ""
  }

jsonOk : Json -> Response
jsonOk json = jsonResponse 200 json

jsonCreated : Json -> Response
jsonCreated json = jsonResponse 201 json

withHeader : String -> String -> Response -> Response
withHeader key value resp =
  { status = resp.status
  , headers = Cons { key = key, value = value } resp.headers
  , body = resp.body
  }
```

**Step 4: Run test to verify it passes**

Run: `cabal run -v0 lune -- --eval examples/36_Response_Helpers.lune`
Expected:
```
200
201
204
404
400
401
302
200
{"id":1}
```

**Step 5: Generate golden files and commit**

```bash
cabal test golden --test-options="--accept"
cabal test golden
git add -A && git commit -m "feat: add response helpers (ok, created, notFound, redirect, etc.) to Lune.Http"
```

---

## Task 5: Add JSON body parsing to `Lune.Api`

Add `jsonBody` that parses the request body as JSON using a decoder, failing with the application's error type if parsing fails.

**Files:**
- Modify: `prelude/Lune/Api.lune` (add `jsonBody` export and function)

**Step 1: Write a golden test**

Create `examples/37_Api_JsonBody.lune`:

```lune
module ApiJsonBody exposing (main)

import Lune.IO as IO
import Lune.Api as Api
import Lune.Json as Json
import Lune.Json.Decode as D
import Lune.String as Str
import Lune.Prelude exposing (IO, Unit, Result(..))

type AppError = ParseError String

userDecoder : D.Decoder { name : String, age : Int }
userDecoder =
  D.map2 (\n a -> { name = n, age = a })
    (D.field "name" D.string)
    (D.field "age" D.int)

main : IO Unit
main =
  do
    -- Test successful parse
    let goodBody = "{\"name\":\"Alice\",\"age\":30}"
    result1 <- Api.run {} (Api.jsonBody ParseError userDecoder { method = GET, path = "/", headers = [], body = goodBody })
    case result1 of
      Ok user -> IO.println (Str.append user.name (Str.append " age " (Str.fromInt user.age)))
      Err (ParseError msg) -> IO.println (Str.append "ERROR: " msg)
    -- Test failed parse
    let badBody = "{\"name\":\"Bob\"}"
    result2 <- Api.run {} (Api.jsonBody ParseError userDecoder { method = GET, path = "/", headers = [], body = badBody })
    case result2 of
      Ok user -> IO.println "unexpected success"
      Err (ParseError msg) -> IO.println "parse failed as expected"
```

Note: The `import Lune.Http exposing (Method(..))` may be needed for `GET`. Adjust as needed.

**Step 2: Run test to verify it fails**

Run: `cabal run -v0 lune -- --eval examples/37_Api_JsonBody.lune`
Expected: Unbound variable `Api.jsonBody`.

**Step 3: Add `jsonBody` to Api module**

In `prelude/Lune/Api.lune`, add to exposing list and add function:

```lune
-- Add to exposing: jsonBody

import Lune.Json as Json
import Lune.Json.Decode as D

{-| Parse the request body as JSON using the given decoder.
    On decode failure, calls `toErr` to convert the error message
    to the application's error type.
-}
jsonBody : (String -> e) -> D.Decoder a -> Request -> Api e a
jsonBody toErr decoder request =
  case Json.parse request.body of
    Err msg -> fail (toErr msg)
    Ok json ->
      case D.decodeValue decoder json of
        Err decodeErr -> fail (toErr decodeErr.message)
        Ok value -> pure value
```

**Step 4: Run test to verify it passes**

Run: `cabal run -v0 lune -- --eval examples/37_Api_JsonBody.lune`
Expected:
```
Alice age 30
parse failed as expected
```

**Step 5: Generate golden files and commit**

```bash
cabal test golden --test-options="--accept"
cabal test golden
git add -A && git commit -m "feat: add Api.jsonBody for typed JSON request body parsing"
```

---

## Task 6: Add middleware support

Middleware is a function type that wraps a handler. This task adds the type alias, a composition helper, and integrates middleware into `Api.serve`.

**Files:**
- Create: `prelude/Lune/Api/Middleware.lune`
- Modify: `prelude/Lune/Api.lune` (update `serve` to accept middleware, add `serveWith`)

**Step 1: Write a golden test**

Create `examples/38_Middleware.lune`:

```lune
module MiddlewareTest exposing (main)

import Lune.IO as IO
import Lune.Api as Api
import Lune.Api.Route as Route
import Lune.Api.Middleware as Middleware
import Lune.Http exposing (Request, Response, Method(..))
import Lune.Http as Http
import Lune.String as Str
import Lune.Prelude exposing (IO, Unit, Result(..), String)

type AppError = AppErr String

errorHandler : AppError -> Response
errorHandler err =
  case err of
    AppErr msg -> Http.badRequest msg

-- A middleware that adds a custom header to every response
addHeader : Middleware.Middleware AppError Context
addHeader next request =
  do
    resp <- next request
    Api.pure (Http.withHeader "X-Powered-By" "Lune" resp)

type alias Context = {}

handler : Request -> Context -> Api.Api AppError Response
handler req ctx =
  Api.pure (Http.ok "hello")

main : IO Unit
main =
  do
    -- Simulate running the middleware + handler directly (no server)
    let wrappedHandler = addHeader (\req -> handler req {})
    let fakeReq = { method = GET, path = "/test", headers = [], body = "" }
    result <- Api.run {} (wrappedHandler fakeReq)
    case result of
      Err _ -> IO.println "error"
      Ok resp ->
        do
          IO.println (Str.fromInt resp.status)
          IO.println resp.body
          printHeaders resp.headers

printHeaders : List { key : String, value : String } -> IO Unit
printHeaders hs =
  case hs of
    [] -> IO.println "done"
    Cons h rest ->
      do
        IO.println (Str.append h.key (Str.append ": " h.value))
        printHeaders rest
```

**Step 2: Run test to verify it fails**

Run: `cabal run -v0 lune -- --eval examples/38_Middleware.lune`
Expected: Module not found error for `Lune.Api.Middleware`.

**Step 3: Create the middleware module**

Create `prelude/Lune/Api/Middleware.lune`:

```lune
module Lune.Api.Middleware exposing (
  Middleware,
  apply,
  compose
)

import Lune.Http exposing (Request, Response)
import Lune.Api exposing (Api)
import Lune.Prelude exposing (List(..))

{-| A middleware transforms a handler into a new handler.
    It can inspect/modify the request, call the inner handler,
    and inspect/modify the response.

    Type: (Request -> Api e Response) -> Request -> Api e Response
-}
type alias Middleware e ctx = (Request -> Api e Response) -> Request -> Api e Response

{-| Apply a single middleware to a handler. -}
apply : Middleware e ctx -> (Request -> Api e Response) -> (Request -> Api e Response)
apply mw handler =
  mw handler

{-| Compose a list of middleware into a single middleware.
    Middleware are applied in list order (first in list = outermost).
-}
compose : List (Middleware e ctx) -> Middleware e ctx
compose middlewares handler =
  case middlewares of
    [] -> handler
    Cons mw rest -> mw (compose rest handler)
```

**Step 4: Run test to verify it passes**

Run: `cabal run -v0 lune -- --eval examples/38_Middleware.lune`
Expected:
```
200
hello
X-Powered-By: Lune
done
```

**Step 5: Generate golden files and commit**

```bash
cabal test golden --test-options="--accept"
cabal test golden
git add -A && git commit -m "feat: add Lune.Api.Middleware module with Middleware type, apply, compose"
```

---

## Task 7: Integrate middleware into `Api.serve`

Add `Api.serveWith` that accepts a middleware list, and update the request handling pipeline.

**Files:**
- Modify: `prelude/Lune/Api.lune`

**Step 1: Add `serveWith` to Api module**

In `prelude/Lune/Api.lune`, add to exposing list and add:

```lune
import Lune.Api.Middleware as Middleware

{-| Serve with middleware applied to all routes. -}
serveWith : ServerConfig e ctx -> List (Middleware.Middleware e ctx) -> Routes e ctx -> IO Unit
serveWith config middlewares routes =
  Server.serve config.port (handleRequestWith config middlewares routes)

handleRequestWith : ServerConfig e ctx -> List (Middleware.Middleware e ctx) -> Routes e ctx -> Request -> IO Response
handleRequestWith config middlewares routes request =
  case findMatchingRoute request routes of
    Nothing ->
      pure notFoundResponse
    Just route ->
      let baseHandler = \req -> route.handler req config.context
          wrappedHandler = Middleware.compose middlewares baseHandler
      in
        do
          result <- run config.context (wrappedHandler request)
          case result of
            Err e -> pure (config.errorHandler e)
            Ok response -> pure response
```

**Step 2: Write a test and verify**

This is tested indirectly by the example from Task 6. Additionally verify the build passes:

```bash
cabal build
```

**Step 3: Commit**

```bash
git add -A && git commit -m "feat: add Api.serveWith for middleware-enabled server"
```

---

## Task 8: Add CORS middleware

A practical, commonly-needed middleware that validates the middleware system works end-to-end.

**Files:**
- Create: `prelude/Lune/Api/Middleware/Cors.lune`

**Step 1: Write a golden test**

Create `examples/39_Cors_Middleware.lune`:

```lune
module CorsMiddleware exposing (main)

import Lune.IO as IO
import Lune.Api as Api
import Lune.Api.Middleware.Cors as Cors
import Lune.Http exposing (Request, Response, Method(..))
import Lune.Http as Http
import Lune.String as Str
import Lune.Prelude exposing (IO, Unit, Result(..), String)

type AppError = AppErr String

handler : Request -> Api.Api AppError Response
handler req = Api.pure (Http.ok "data")

main : IO Unit
main =
  do
    let cors = Cors.allowAll
    let wrapped = cors handler
    let fakeReq = { method = GET, path = "/api", headers = [], body = "" }
    result <- Api.run {} (wrapped fakeReq)
    case result of
      Err _ -> IO.println "error"
      Ok resp ->
        do
          IO.println (Str.fromInt resp.status)
          printHeaders resp.headers

printHeaders : List { key : String, value : String } -> IO Unit
printHeaders hs =
  case hs of
    [] -> IO.println "done"
    Cons h rest ->
      do
        IO.println (Str.append h.key (Str.append ": " h.value))
        printHeaders rest
```

**Step 2: Run test to verify it fails**

Run: `cabal run -v0 lune -- --eval examples/39_Cors_Middleware.lune`
Expected: Module not found error.

**Step 3: Create the CORS module**

Create `prelude/Lune/Api/Middleware/Cors.lune`:

```lune
module Lune.Api.Middleware.Cors exposing (
  allowAll,
  withOrigins
)

import Lune.Http exposing (Request, Response)
import Lune.Http as Http
import Lune.Api exposing (Api)
import Lune.Api.Middleware exposing (Middleware)
import Lune.Prelude exposing (String, List(..))

{-| CORS middleware that allows all origins.
    Adds Access-Control-Allow-Origin: *
    and Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE, OPTIONS
    and Access-Control-Allow-Headers: Content-Type, Authorization
-}
allowAll : Middleware e ctx
allowAll next request =
  do
    resp <- next request
    Api.pure (addCorsHeaders "*" resp)

{-| CORS middleware that allows specific origins.
    Checks the Origin header against the allowed list.
    If the origin matches, reflects it back. Otherwise, no CORS headers are added.
-}
withOrigins : List String -> Middleware e ctx
withOrigins origins next request =
  do
    resp <- next request
    case prim_getHeader "origin" request.headers of
      Nothing -> Api.pure resp
      Just origin ->
        case memberOf origin origins of
          True -> Api.pure (addCorsHeaders origin resp)
          False -> Api.pure resp

addCorsHeaders : String -> Response -> Response
addCorsHeaders origin resp =
  Http.withHeader "Access-Control-Allow-Origin" origin
    (Http.withHeader "Access-Control-Allow-Methods" "GET, POST, PUT, PATCH, DELETE, OPTIONS"
      (Http.withHeader "Access-Control-Allow-Headers" "Content-Type, Authorization"
        resp))

memberOf : String -> List String -> Bool
memberOf target xs =
  case xs of
    [] -> False
    Cons x rest ->
      case prim_eqString target x of
        True -> True
        False -> memberOf target rest
```

**Step 4: Run test to verify it passes**

Run: `cabal run -v0 lune -- --eval examples/39_Cors_Middleware.lune`
Expected output should show status 200 and the CORS headers.

**Step 5: Generate golden files and commit**

```bash
cabal test golden --test-options="--accept"
cabal test golden
git add -A && git commit -m "feat: add CORS middleware (allowAll, withOrigins)"
```

---

## Task 9: Add route groups with shared prefix

Allow grouping routes under a common path prefix.

**Files:**
- Modify: `prelude/Lune/Api/Route.lune` (add `group` function)

**Step 1: Write a golden test**

Create `examples/40_Route_Groups.lune`:

```lune
module RouteGroups exposing (main)

import Lune.IO as IO
import Lune.Api as Api
import Lune.Api.Route as Route
import Lune.Http exposing (Request, Response, Method(..))
import Lune.Http as Http
import Lune.Prelude exposing (IO, Unit, Result(..), String)

type AppError = AppErr String

healthHandler : Request -> {} -> Api.Api AppError Response
healthHandler req ctx = Api.pure (Http.ok "healthy")

usersListHandler : Request -> {} -> Api.Api AppError Response
usersListHandler req ctx = Api.pure (Http.ok "user list")

usersGetHandler : Request -> {} -> Api.Api AppError Response
usersGetHandler req ctx = Api.pure (Http.ok "single user")

routes : Route.Routes AppError {}
routes =
  Route.define
    [ Route.get "/health" healthHandler
    , Route.group "/api/v1"
        [ Route.get "/users" usersListHandler
        , Route.get "/users/:id" usersGetHandler
        ]
    ]

main : IO Unit
main =
  do
    -- Simulate matching by checking prim_matchPath manually
    case prim_matchPath "/api/v1/users" "/api/v1/users" of
      Nothing -> IO.println "NO MATCH for /api/v1/users"
      Just _ -> IO.println "MATCHED /api/v1/users"
    case prim_matchPath "/api/v1/users/:id" "/api/v1/users/42" of
      Nothing -> IO.println "NO MATCH for /api/v1/users/42"
      Just _ -> IO.println "MATCHED /api/v1/users/42"
```

**Step 2: Run test to verify it fails**

Run: `cabal run -v0 lune -- --eval examples/40_Route_Groups.lune`
Expected: Unbound variable `Route.group`.

**Step 3: Add `group` to Route module**

In `prelude/Lune/Api/Route.lune`, add to exposing list and add:

```lune
import Lune.String as Str

{-| Group routes under a common path prefix.
    Route.group "/api/v1" [ Route.get "/users" handler ]
    produces a route matching "/api/v1/users".
-}
group : String -> List (Route e ctx) -> List (Route e ctx)
group prefix routes =
  mapRoutes (prefixRoute prefix) routes

prefixRoute : String -> Route e ctx -> Route e ctx
prefixRoute prefix route =
  { method = route.method
  , pattern = Str.append prefix route.pattern
  , handler = route.handler
  }

mapRoutes : (Route e ctx -> Route e ctx) -> List (Route e ctx) -> List (Route e ctx)
mapRoutes f routes =
  case routes of
    [] -> []
    Cons r rest -> Cons (f r) (mapRoutes f rest)
```

Also update `Route.define` to flatten nested lists, or keep it simple — `group` returns a `List (Route e ctx)` which the user can concatenate. The `define` function currently just passes through. The user would use it like:

```lune
routes = Route.define
  (Cons (Route.get "/health" healthHandler)
    (Route.group "/api/v1"
      [ Route.get "/users" usersListHandler
      , Route.get "/users/:id" usersGetHandler
      ]))
```

But the sugar `[ a, Route.group ... [...] ]` would produce a list containing a route and a list — which is a type error. We need `group` to return `List (Route e ctx)` and provide `concat` for flattening:

Actually, the simplest approach: `Route.define` accepts `List (Route e ctx)`. `group` returns `List (Route e ctx)`. The user concatenates them with a helper. Add `Route.concat` to combine route lists:

```lune
{-| Concatenate multiple route lists into one. -}
concat : List (Routes e ctx) -> Routes e ctx
concat routeLists =
  case routeLists of
    [] -> []
    Cons rs rest -> appendRoutes rs (concat rest)

appendRoutes : List a -> List a -> List a
appendRoutes xs ys =
  case xs of
    [] -> ys
    Cons x rest -> Cons x (appendRoutes rest ys)
```

Then usage:

```lune
routes =
  Route.define (Route.concat
    [ [ Route.get "/health" healthHandler ]
    , Route.group "/api/v1"
        [ Route.get "/users" usersListHandler
        , Route.get "/users/:id" usersGetHandler
        ]
    ])
```

**Step 4: Run test to verify it passes**

Run: `cabal run -v0 lune -- --eval examples/40_Route_Groups.lune`
Expected:
```
MATCHED /api/v1/users
MATCHED /api/v1/users/42
```

**Step 5: Generate golden files and commit**

```bash
cabal test golden --test-options="--accept"
cabal test golden
git add -A && git commit -m "feat: add Route.group and Route.concat for route grouping with path prefixes"
```

---

## Task 10: Update spec, docs, and README

Update documentation to reflect the new web framework capabilities.

**Files:**
- Modify: `spec/lune_web_framework_v0_1.md` (add sections for middleware, query params, body parsing, response helpers, route groups)
- Modify: `README.md` (update feature table: "Web Framework" from "Basic (JSON APIs)" to "Working")
- Modify: `docs/FFI.md` — no changes needed

**Step 1: Update the web framework spec**

Add new sections to `spec/lune_web_framework_v0_1.md`:

- Section 8: Request Helpers (`Lune.Http.Request`) — query, header, queryParams
- Section 9: Response Helpers — ok, created, notFound, redirect, jsonOk, withHeader
- Section 10: JSON Body Parsing — `Api.jsonBody`
- Section 11: Middleware — type, apply, compose, serveWith
- Section 12: CORS Middleware — allowAll, withOrigins
- Section 13: Route Groups — group, concat

Update Section 7 "Future Work" to remove items that are now implemented.

**Step 2: Update README**

Change the feature table row from:
```
| Web Framework (Api monad) | Basic (JSON APIs) |
```
to:
```
| Web Framework (Api monad, middleware, CORS) | Working |
```

**Step 3: Commit**

```bash
git add -A && git commit -m "docs: update web framework spec and README for middleware, query params, CORS, route groups"
```

---

## Task 11: Run full test suite

Final verification that everything works together.

**Files:** None (verification only)

**Step 1: Build and run all golden tests**

```bash
cabal build
cabal test golden
```

Expected: All tests pass.

**Step 2: Spot-check the API server example still works**

```bash
cabal run -v0 lune -- --eval examples/18_Api_Server.lune &
sleep 1
curl http://localhost:8080/health
curl http://localhost:8080/users/42
kill %1
```

Expected: JSON responses with 200 status.

If any failures, fix and re-run.

**Step 3: Final commit if any fixups needed**

```bash
git add -A && git commit -m "test: verify full test suite passes after web framework enhancements"
```

---

## Summary

| Task | What | New Files | Modified Files |
|------|------|-----------|----------------|
| 1 | Fix query string in path matching | `examples/32_*` | `Builtins.hs` |
| 2 | `prim_parseQueryString`, `prim_getHeader` | `examples/33_*`, `examples/34_*` | `Builtins.hs` |
| 3 | `Lune.Http.Request` module | `prelude/Lune/Http/Request.lune`, `examples/35_*` | — |
| 4 | Response helpers | `examples/36_*` | `prelude/Lune/Http.lune` |
| 5 | `Api.jsonBody` | `examples/37_*` | `prelude/Lune/Api.lune` |
| 6 | Middleware type + compose | `prelude/Lune/Api/Middleware.lune`, `examples/38_*` | — |
| 7 | `Api.serveWith` | — | `prelude/Lune/Api.lune` |
| 8 | CORS middleware | `prelude/Lune/Api/Middleware/Cors.lune`, `examples/39_*` | — |
| 9 | Route groups | `examples/40_*` | `prelude/Lune/Api/Route.lune` |
| 10 | Documentation | — | `spec/lune_web_framework_v0_1.md`, `README.md` |
| 11 | Final verification | — | — |
