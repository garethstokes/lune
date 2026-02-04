# Lune Web Framework Specification v0.1

## Overview

The Lune web framework provides a type-safe, JSON-focused foundation for building RESTful APIs. It is built on explicit effects, Result-based error handling, and user-defined context.

### Design Principles

1. **Type-safe routing** - Route patterns with method matching
2. **Explicit context** - No hidden global state; dependencies flow through a user-defined context
3. **Composable errors** - User defines error types and maps them to HTTP responses
4. **Focused scope** - JSON APIs only; HTML rendering is future work

---

## 1. Module Structure

```
Lune.Http           -- HTTP types (Method, Request, Response)
Lune.Http.Server    -- Low-level HTTP server
Lune.Api            -- Api monad and server entry point
Lune.Api.Route      -- Route definition helpers
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

### Response Helpers

```
response : Int -> String -> Response
jsonResponse : Int -> Json -> Response
```

---

## 3. The Api Monad

```
Api e a
```

A computation that:
- Has read access to user-defined context
- Can fail early with error type `e`
- Runs in IO
- Returns a value of type `a`

### Core Operations

```
-- Run an Api computation with context
run : ctx -> Api e a -> IO (Result e a)

-- Access the context
context : Api e ctx

-- Fail with an error
fail : e -> Api e a

-- Fail if Maybe is Nothing
orFail : e -> Maybe a -> Api e a

-- Transform error type
mapError : (e1 -> e2) -> Api e1 a -> Api e2 a

-- Monadic operations
pure : a -> Api e a
andThen : (a -> Api e b) -> Api e a -> Api e b
```

### Typeclass Instances

`Api e` has `Functor`, `Applicative`, and `Monad` instances, enabling do-notation:

```
getUser : { id : Int } -> Api AppError User
getUser params =
  do
    ctx <- Api.context
    -- use ctx.db, etc.
    ...
```

---

## 4. Route Definition

### Lune.Api.Route

```
type alias Route e ctx =
  { method : Method
  , pattern : String
  , handler : Request -> ctx -> Api e Response
  }

type alias Routes e ctx = List (Route e ctx)
```

### Route Constructors

```
get : String -> (Request -> ctx -> Api e Response) -> Route e ctx
post : String -> (Request -> ctx -> Api e Response) -> Route e ctx
put : String -> (Request -> ctx -> Api e Response) -> Route e ctx
patch : String -> (Request -> ctx -> Api e Response) -> Route e ctx
delete : String -> (Request -> ctx -> Api e Response) -> Route e ctx

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

serve : ServerConfig e ctx -> Routes e ctx -> IO Unit
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
      , headers = Cons { key = "Content-Type", value = "application/json" } Nil
      , body = "{\"error\":\"" ++ msg ++ "\"}"
      }
    BadRequest msg ->
      { status = 400
      , headers = Cons { key = "Content-Type", value = "application/json" } Nil
      , body = "{\"error\":\"" ++ msg ++ "\"}"
      }
```

---

## 6. Complete Example

```
module ApiServer exposing (main)

import Lune.IO as IO
import Lune.Api as Api
import Lune.Api.Route as Route
import Lune.Http exposing (Request, Response, Method(..))
import Lune.String as Str
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Maybe(..))

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
      , headers = Cons { key = "Content-Type", value = "application/json" } Nil
      , body = Str.append "{\"error\":\"" (Str.append msg "\"}")
      }
    BadRequest msg ->
      { status = 400
      , headers = Cons { key = "Content-Type", value = "application/json" } Nil
      , body = Str.append "{\"error\":\"" (Str.append msg "\"}")
      }

-- Routes
routes : Route.Routes AppError Context
routes =
  Route.define
    (Cons (Route.get "/health" healthHandler)
      (Cons (Route.get "/users/:id" getUserHandler)
        (Cons (Route.post "/users" createUserHandler) Nil)))

-- Handlers
healthHandler : Request -> Context -> Api.Api AppError Response
healthHandler request ctx =
  Api.pure
    { status = 200
    , headers = Cons { key = "Content-Type", value = "application/json" } Nil
    , body = "{\"status\":\"ok\"}"
    }

getUserHandler : Request -> Context -> Api.Api AppError Response
getUserHandler request ctx =
  Api.pure
    { status = 200
    , headers = Cons { key = "Content-Type", value = "application/json" } Nil
    , body = "{\"id\":123,\"name\":\"Alice\"}"
    }

createUserHandler : Request -> Context -> Api.Api AppError Response
createUserHandler request ctx =
  Api.pure
    { status = 201
    , headers = Cons { key = "Content-Type", value = "application/json" } Nil
    , body = "{\"id\":456,\"name\":\"Created\"}"
    }

main : IO Unit
main =
  let config =
        { port = 8080
        , errorHandler = errorToResponse
        , context = { appName = "MyApp" }
        }
  in Api.serve config routes
```

---

## 7. Future Work (Not in v0.1)

- **Typed path parameters** - Automatic extraction of `:param` into typed records
- **Query parameters** - `Api.query` for typed query string access
- **Request body parsing** - Automatic JSON decoding into typed records
- **Middleware** - Composable request/response transformers
- **Route groups** - Shared prefixes and authentication requirements
- **Database layer** - Type-safe query builder
- **WebSockets** - Real-time communication
- **OpenAPI generation** - Auto-generate docs from route types
