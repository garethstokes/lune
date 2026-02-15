# Lune Web Framework Specification v0.1

## Overview

The Lune web framework provides a type-safe, JSON-focused foundation for building RESTful APIs. It is built on explicit effects, Result-based error handling, and user-defined context.

### Design Principles

1. **Type-safe routing** - Route patterns with method matching
2. **Explicit context** - No hidden global state; dependencies flow through a user-defined context
3. **Composable errors** - User defines error types and maps them to HTTP responses
4. **Focused scope** - JSON APIs only; HTML rendering is future work
5. **Pure Lune implementation** - HTTP parsing, routing, and response formatting are implemented in pure Lune

---

## 1. Module Structure

```
Lune.Http              -- HTTP types (Method, Request, Response)
Lune.Http.Server       -- Low-level HTTP server
Lune.Http.Api          -- Server entry point and helpers
Lune.Http.Route        -- Route definition helpers
Lune.Http.Request      -- Request helpers (query, header, queryParams)
Lune.Http.Response     -- Response helpers (ok, created, notFound, etc.)
Lune.Http.Internal     -- HTTP parsing/formatting (internal)
```

---

## 2. HTTP Types

### Lune.Http

```
type Method =
  GET
  | POST
  | PUT
  | PATCH
  | DELETE

type alias Request =
  { method : Method
  , path : String
  , headers : List { key : String, value : String }
  , body : String
  }

type alias Response =
  { status : Int
  , headers : List { key : String, value : String }
  , body : String
  }
```

### Lune.Http.Response Helpers

```
ok : String -> Response
created : String -> Response
noContent : Response
badRequest : String -> Response
unauthorized : String -> Response
forbidden : String -> Response
notFound : String -> Response
serverError : String -> Response
redirect : String -> Response
```

### Lune.Http.Request Helpers

```
query : String -> Request -> Maybe String
header : String -> Request -> Maybe String
queryParams : Request -> List { key : String, value : String }
```

---

## 3. Handler Pattern

Handlers return `Task e Response`:

```
handler : Request -> ctx -> Task e Response
```

This pattern:
- Uses Task for effectful operations with typed failure
- Receives both the request and user-defined context
- Returns a Response on success or error type `e` on failure

### Helper Functions (Lune.Http.Api)

```
-- Wrap a successful response
pure : a -> Task e a

-- Create a failure
fail : e -> Task e a

-- Fail if Maybe is Nothing
orFail : e -> Maybe a -> Task e a

-- Parse JSON request body
jsonBody : (String -> e) -> Decoder a -> Request -> Result e a
```

---

## 4. Route Definition

### Lune.Http.Route

```
type alias Route e ctx =
  { method : Method
  , pattern : String
  , handler : Request -> ctx -> Task e Response
  }

type alias Routes e ctx = List (Route e ctx)
```

### Route Constructors

```
get : String -> (Request -> ctx -> Task e Response) -> Route e ctx
post : String -> (Request -> ctx -> Task e Response) -> Route e ctx
put : String -> (Request -> ctx -> Task e Response) -> Route e ctx
patch : String -> (Request -> ctx -> Task e Response) -> Route e ctx
delete : String -> (Request -> ctx -> Task e Response) -> Route e ctx

define : List (Route e ctx) -> Routes e ctx
```

### Path Patterns

Path patterns use `:param` syntax for path parameters:

```
Route.get "/users/:id" getUserHandler
Route.get "/users/:userId/posts/:postId" getPostHandler
```

**Note:** In v0.1, path parameters are matched but not automatically extracted into typed records. Handlers receive the raw `Request` and must parse path parameters manually if needed.

---

## 5. Server Configuration

```
type alias ServerConfig e ctx =
  { port : Int
  , errorHandler : e -> Response
  , context : ctx
  }

serve : ServerConfig e ctx -> Routes e ctx -> Task Error Unit
```

### Error Handler

The `errorHandler` function converts your application error type to an HTTP response:

```
type AppError =
  NotFound String
  | BadRequest String

errorToResponse : AppError -> Response
errorToResponse err =
  case err of
    NotFound msg ->
      { status = 404
      , headers = [{ key = "Content-Type", value = "application/json" }]
      , body = "{\"error\":\"" ++ msg ++ "\"}"
      }
    BadRequest msg ->
      { status = 400
      , headers = [{ key = "Content-Type", value = "application/json" }]
      , body = "{\"error\":\"" ++ msg ++ "\"}"
      }
```

---

## 6. Complete Example

```
module ApiServer exposing (main)

import Lune.IO as IO
import Lune.Http.Api as Api
import Lune.Http.Route as Route
import Lune.Http exposing (Request, Response, Method(..))
import Lune.String as Str
import Lune.Prelude exposing (Task, Unit, List(..), Maybe(..))

-- Error type
type AppError =
  NotFound String
  | BadRequest String

-- Context (your application dependencies)
type alias Context =
  { appName : String
  }

-- Error handler
errorToResponse : AppError -> Response
errorToResponse err =
  case err of
    NotFound msg ->
      { status = 404
      , headers = [{ key = "Content-Type", value = "application/json" }]
      , body = Str.append "{\"error\":\"" (Str.append msg "\"}")
      }
    BadRequest msg ->
      { status = 400
      , headers = [{ key = "Content-Type", value = "application/json" }]
      , body = Str.append "{\"error\":\"" (Str.append msg "\"}")
      }

-- Routes
routes : Api.Routes AppError Context
routes =
  Route.define
    [ Route.get "/health" healthHandler
    , Route.get "/users/:id" getUserHandler
    , Route.post "/users" createUserHandler
    ]

-- Handlers return Task e Response
healthHandler : Request -> Context -> Task AppError Response
healthHandler request ctx =
  Api.pure
    { status = 200
    , headers = [{ key = "Content-Type", value = "application/json" }]
    , body = "{\"status\":\"ok\"}"
    }

getUserHandler : Request -> Context -> Task AppError Response
getUserHandler request ctx =
  Api.pure
    { status = 200
    , headers = [{ key = "Content-Type", value = "application/json" }]
    , body = "{\"id\":123,\"name\":\"Alice\"}"
    }

createUserHandler : Request -> Context -> Task AppError Response
createUserHandler request ctx =
  Api.pure
    { status = 201
    , headers = [{ key = "Content-Type", value = "application/json" }]
    , body = "{\"id\":456,\"name\":\"Created\"}"
    }

main : Task Error Unit
main =
  Api.serve
    { port = 8080
    , errorHandler = errorToResponse
    , context = { appName = "MyApp" }
    }
    routes
```

---

## 7. JSON Body Parsing

Parse JSON request bodies with type-safe decoders:

```
import Lune.Http.Api as Api
import Lune.Json.Decode as D

type AppError = ParseError String

userDecoder : D.Decoder { name : String, age : Int }
userDecoder =
  D.map2 (\n a -> { name = n, age = a })
    (D.field "name" D.string)
    (D.field "age" D.int)

createUserHandler : Request -> Context -> Task AppError Response
createUserHandler request ctx =
  case Api.jsonBody ParseError userDecoder request of
    Err e -> Api.fail e
    Ok user ->
      Api.pure { status = 201, headers = [], body = user.name }
```

---

## 8. Future Work (Not in v0.1)

- **Typed path parameters** - Automatic extraction of `:param` into typed records
- **Middleware** - Composable request/response transformers
- **Route groups** - Shared prefixes and authentication requirements
- **WebSockets** - Real-time communication
- **OpenAPI generation** - Auto-generate docs from route types
