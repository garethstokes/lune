# Lune Web Framework Design

> **Status:** DRAFT
> **Date:** 2026-02-04

## Overview

A type-safe, JSON-focused web framework for building RESTful APIs in Lune. Designed around explicit effects, Result-based error handling, and Drizzle-inspired database access.

### Design Principles

1. **Type-safe routing** - Route parameters and request bodies are type-checked at compile time
2. **Explicit context** - No hidden global state; dependencies flow through a user-defined context
3. **Backend-aware database** - SQL stays SQL; backend-specific features are available, not abstracted away
4. **Focused scope** - JSON APIs only; HTML rendering and other response types are future work

---

## 1. The Api Monad

```
Api e a
```

A computation that:
- Has read access to user-defined context
- Can fail early with error type `e`
- Runs in IO
- Returns a value of type `a`

### Context Access

```
Api.context : Api e ctx
```

Returns the full context. User destructures as needed:

```
getUser : { id : Int } -> Api AppError User
getUser params =
  do
    ctx <- Api.context
    result <- Database.select users
      |> Database.where_ (users.id |> Database.eq params.id)
      |> Database.one
      |> Database.run ctx.db
      |> Api.mapError DbError
    result |> Api.orFail (NotFound "User not found")
```

### Error Handling

```
Api.fail : e -> Api e a
Api.orFail : e -> Maybe a -> Api e a
Api.mapError : (e1 -> e2) -> Api e1 a -> Api e2 a
```

---

## 2. Route Definition

### Basic Routes

```
import Lune.Api as Api

api : Api.Routes AppError Context
api =
  Api.define
    [ Api.get "/users" listUsers
    , Api.get "/users/:id" getUser
    , Api.post "/users" createUser
    , Api.put "/users/:id" updateUser
    , Api.delete "/users/:id" deleteUser
    ]
```

### Path Parameters

Path parameters (`:name`) become fields in a record passed to the handler:

```
-- Route: "/users/:id/posts/:postId"
-- Handler receives: { id : Int, postId : Int }

getUserPost : { id : Int, postId : Int } -> Api AppError Post
getUserPost params =
  do
    ctx <- Api.context
    ...
```

### Request Body

POST/PUT handlers receive body as a separate second argument:

```
createUser : {} -> CreateUserInput -> Api AppError User
createUser params body =
  do
    ctx <- Api.context
    Database.insert users
      |> Database.values { name = body.name, email = body.email }
      |> Database.returning All
      |> Database.run ctx.db
      |> Api.mapError DbError
```

### Type Constraints (Hidden)

Route functions enforce JSON constraints internally:

```
-- Internal signatures (user doesn't write these constraints)
Api.get : ToJson response => String -> (params -> Api e response) -> Route e
Api.post : (FromJson body, ToJson response) => String -> (params -> body -> Api e response) -> Route e
Api.put : (FromJson body, ToJson response) => String -> (params -> body -> Api e response) -> Route e
Api.delete : ToJson response => String -> (params -> Api e response) -> Route e
```

If a handler's body type lacks `FromJson`, compilation fails.

---

## 3. Handler Signatures

### GET (no body)

```
listUsers : {} -> Api AppError (List User)
getUser : { id : Int } -> Api AppError User
```

### POST/PUT (with body)

```
createUser : {} -> CreateUserInput -> Api AppError User
updateUser : { id : Int } -> UpdateUserInput -> Api AppError User
```

### DELETE (no body)

```
deleteUser : { id : Int } -> Api AppError Unit
```

---

## 4. Error Handling

### User-Defined Error Type

```
type AppError
  = NotFound String
  | BadRequest String
  | Unauthorized
  | Forbidden
  | Conflict String
  | DbError Database.Error
```

### Error-to-Response Mapping

User provides a function to convert errors to HTTP responses:

```
errorToResponse : AppError -> Response
errorToResponse err =
  case err of
    NotFound msg -> Response.json 404 { error = msg }
    BadRequest msg -> Response.json 400 { error = msg }
    Unauthorized -> Response.json 401 { error = "Unauthorized" }
    Forbidden -> Response.json 403 { error = "Forbidden" }
    Conflict msg -> Response.json 409 { error = msg }
    DbError e -> Response.json 500 { error = Database.errorToString e }
```

---

## 5. Server Configuration

```
type alias ServerConfig e ctx =
  { port : Int
  , errorHandler : e -> Response
  , context : ctx
  }

Api.serve : ServerConfig e ctx -> Api.Routes e ctx -> IO Unit
```

### Example

```
main : IO Unit
main =
  do
    db <- Postgres.connect "postgres://localhost/myapp"
    logger <- Logger.console
    let context = { db = db, logger = logger }
    let config =
      { port = 8080
      , errorHandler = errorToResponse
      , context = context
      }
    Api.serve config api
```

---

## 6. Database Layer

### Module Structure

```
import Lune.Database as Database
import Lune.Database.Postgres as Postgres
import Lune.Database.SQLite as SQLite
```

### Schema Definition (Backend-Specific)

```
users : Table User Postgres
users =
  Table.define "users"
    { id = Postgres.serial |> Column.primaryKey
    , name = Postgres.text |> Column.notNull
    , email = Postgres.text |> Column.unique
    , metadata = Postgres.jsonb
    , createdAt = Postgres.timestamp |> Column.default Now
    }

type alias User =
  { id : Int
  , name : String
  , email : String
  , metadata : Json.Value
  , createdAt : Timestamp
  }
```

### Queries (SQL-Like DSL)

```
-- SELECT
Database.select users
  |> Database.where_ (users.active |> Database.eq True)
  |> Database.orderBy users.createdAt Desc
  |> Database.limit 10
  |> Database.run db

-- SELECT ONE
Database.select users
  |> Database.where_ (users.id |> Database.eq id)
  |> Database.one
  |> Database.run db   -- Returns: IO (Result DbError (Maybe User))

-- INSERT
Database.insert users
  |> Database.values { name = "Alice", email = "alice@example.com" }
  |> Database.returning All
  |> Database.run db

-- UPDATE
Database.update users
  |> Database.set { name = "Bob" }
  |> Database.where_ (users.id |> Database.eq id)
  |> Database.run db

-- DELETE
Database.delete users
  |> Database.where_ (users.id |> Database.eq id)
  |> Database.run db
```

### Backend-Specific Features

```
-- Postgres JSONB operations
Database.select users
  |> Database.where_ (users.metadata |> Postgres.jsonbContains { role = "admin" })
  |> Database.run db
```

### Migrations

```bash
# Generate migration from schema changes
$ lune db generate-migration add-users-table

# Creates: migrations/001_add_users_table.sql
# User can edit before applying

$ lune db migrate
```

---

## 7. Complete Example

```
module MyApp exposing (main)

import Lune.Api as Api
import Lune.Database as Database
import Lune.Database.Postgres as Postgres
import Lune.Json as Json

-- ============================================================================
-- Validated Types
-- ============================================================================

type Email = Email String
type NonEmpty = NonEmpty String

-- ============================================================================
-- Schema
-- ============================================================================

users : Table User Postgres
users =
  Table.define "users"
    { id = Postgres.serial |> Column.primaryKey
    , name = Postgres.text |> Column.notNull
    , email = Postgres.text |> Column.unique
    }

type alias User =
  { id : Int
  , name : String
  , email : String
  }

type alias CreateUserInput =
  { name : NonEmpty
  , email : Email
  }

type alias UpdateUserInput =
  { name : Field NonEmpty
  , email : Field Email
  }

-- ============================================================================
-- Errors
-- ============================================================================

type AppError
  = NotFound String
  | BadRequest String
  | Unauthorized
  | Forbidden
  | Conflict String
  | DbError Database.Error

errorToResponse : AppError -> Response
errorToResponse err =
  case err of
    NotFound msg -> Response.json 404 { error = msg }
    BadRequest msg -> Response.json 400 { error = msg }
    Unauthorized -> Response.json 401 { error = "Unauthorized" }
    Forbidden -> Response.json 403 { error = "Forbidden" }
    Conflict msg -> Response.json 409 { error = msg }
    DbError e -> Response.json 500 { error = Database.errorToString e }

-- ============================================================================
-- Context
-- ============================================================================

type alias Context =
  { db : Database.Connection Postgres
  }

-- ============================================================================
-- Routes
-- ============================================================================

api : Api.Routes AppError Context
api =
  Api.define
    [ -- Public
      Api.get "/health" healthCheck
    , Api.post "/login" login

    -- Authenticated user routes
    , Api.group
        |> Api.requireAuth
        |> Api.prefix "/users"
        |> Api.routes
            [ Api.get ""
                |> Api.query "page" Api.int
                |> Api.query "limit" Api.int
                |> Api.handler listUsers
            , Api.get "/:id" getUser
            , Api.post "" createUser
            , Api.patch "/:id" updateUser
            , Api.delete "/:id" deleteUser
            ]
    ]

-- ============================================================================
-- Handlers
-- ============================================================================

healthCheck : {} -> Api AppError Unit
healthCheck params =
  pure unit

listUsers : { user : AuthUser, page : Maybe Int, limit : Maybe Int } -> Api AppError (List User)
listUsers params =
  do
    ctx <- Api.context
    let page = params.page |> Maybe.withDefault 1
    let limit = params.limit |> Maybe.withDefault 20
    let offset = Int.mul (Int.sub page 1) limit
    Database.select users
      |> Database.orderBy users.id Asc
      |> Database.limit limit
      |> Database.offset offset
      |> Database.run ctx.db
      |> Api.mapError DbError

getUser : { user : AuthUser, id : Int } -> Api AppError User
getUser params =
  do
    ctx <- Api.context
    result <- Database.select users
      |> Database.where_ (users.id |> Database.eq params.id)
      |> Database.one
      |> Database.run ctx.db
      |> Api.mapError DbError
    result |> Api.orFail (NotFound "User not found")

createUser : { user : AuthUser } -> CreateUserInput -> Api AppError User
createUser params body =
  do
    ctx <- Api.context
    Database.insert users
      |> Database.values { name = NonEmpty.toString body.name, email = Email.toString body.email }
      |> Database.returning All
      |> Database.run ctx.db
      |> Api.mapError DbError

updateUser : { user : AuthUser, id : Int } -> UpdateUserInput -> Api AppError User
updateUser params body =
  do
    ctx <- Api.context
    _ <- getUser { user = params.user, id = params.id }  -- ensure exists
    Database.update users
      |> Database.set body
      |> Database.where_ (users.id |> Database.eq params.id)
      |> Database.returning All
      |> Database.run ctx.db
      |> Api.mapError DbError

deleteUser : { user : AuthUser, id : Int } -> Api AppError Unit
deleteUser params =
  do
    ctx <- Api.context
    _ <- getUser { user = params.user, id = params.id }  -- ensure exists
    Database.delete users
      |> Database.where_ (users.id |> Database.eq params.id)
      |> Database.run ctx.db
      |> Api.mapError DbError

-- ============================================================================
-- Main
-- ============================================================================

main : IO Unit
main =
  do
    db <- Postgres.connect "postgres://localhost/myapp"
    let context = { db = db }
    let config =
      { port = 8080
      , errorHandler = errorToResponse
      , context = context
      }
    Api.serve config api
```

---

## 8. Future Work (Out of Scope for MVP)

- **Middleware** - Logging, auth, rate limiting as composable layers
- **HTML/Template responses** - `Web` monad for full web apps
- **WebSockets** - Real-time communication
- **File uploads** - Multipart form handling
- **OpenAPI generation** - Auto-generate docs from route types
- **Query builder extensions** - Joins, subqueries, aggregations
- **Connection pooling** - Production database configuration
- **Other backends** - SQLite, MongoDB adapters

---

## 9. Implementation Order

### Phase 1: HTTP Foundation
1. HTTP request/response types
2. Basic HTTP server (build on existing TCP sockets)
3. Request parsing (method, path, headers, body)
4. Response encoding
5. JSON body parsing integration

### Phase 2: Api Monad
1. `Api e a` monad definition
2. Context threading (ReaderT pattern)
3. Error short-circuit (ExceptT pattern)
4. `Api.context`, `Api.fail`, `Api.orFail`, `Api.mapError`

### Phase 3: Routing
1. Route data type
2. Path parameter extraction
3. `Api.define`, `Api.get`, `Api.post`, etc.
4. Route matching and dispatch
5. `Api.serve` entry point

### Phase 4: Database (Postgres MVP)
1. Postgres connection primitive
2. `Table` and `Column` types
3. Basic query builder (select, insert, update, delete)
4. Query execution and result parsing
5. Error types

### Phase 5: Integration & Polish
1. End-to-end example working
2. Error message quality
3. Golden tests for web handlers

---

## 10. Query Parameters

Query parameters are declared at the route level and flow into the params record:

```
Api.get "/users"
  |> Api.query "page" Api.int
  |> Api.query "limit" Api.int
  |> Api.query "sort" Api.string
  |> Api.handler listUsers

listUsers : { page : Maybe Int, limit : Maybe Int, sort : Maybe String } -> Api AppError (List User)
listUsers params =
  do
    let page = params.page |> Maybe.withDefault 1
    let limit = params.limit |> Maybe.withDefault 20
    ...
```

---

## 11. Header Access

Headers are accessed via `Api.header`, which is shorthand for reading from the request in context:

```
Api.header : String -> Api e (Maybe String)

-- Usage
getUser : { id : Int } -> Api AppError User
getUser params =
  do
    authHeader <- Api.header "Authorization"
    ...

-- Equivalent to:
getUser params =
  do
    ctx <- Api.context
    let authHeader = ctx.request.headers |> Headers.get "Authorization"
    ...
```

---

## 12. Authentication & Route Groups

Routes can be grouped with shared requirements. Auth requirements inject the authenticated user into the params record:

```
api : Api.Routes AppError Context
api =
  Api.define
    [ -- Public routes
      Api.get "/health" healthCheck
    , Api.post "/login" login

    -- Authenticated routes (grouped)
    , Api.group
        |> Api.requireAuth
        |> Api.prefix "/users"
        |> Api.routes
            [ Api.get "" listUsers           -- GET /users
            , Api.get "/:id" getUser         -- GET /users/:id
            , Api.post "" createUser         -- POST /users
            ]

    -- Admin routes (composed requirements)
    , Api.group
        |> Api.requireAuth
        |> Api.requireRole Admin
        |> Api.prefix "/admin"
        |> Api.routes
            [ Api.get "/stats" getStats
            , Api.delete "/users/:id" deleteUser
            ]
    ]

-- Public handler
healthCheck : {} -> Api AppError Unit

-- Authenticated handler (user injected into params)
listUsers : { user : AuthUser } -> Api AppError (List User)

-- Authenticated with path param
getUser : { user : AuthUser, id : Int } -> Api AppError User
```

---

## 13. Validated Types

Validation happens at the JSON parsing boundary using validated newtypes:

```
-- Validated types (can only be constructed via parse)
type Email = Email String
type NonEmpty = NonEmpty String

-- Smart constructors
Email.parse : String -> Result String Email
NonEmpty.parse : String -> Result String NonEmpty

-- FromJson instance validates during parsing
instance FromJson Email where
  decode = Json.string |> Json.andThen (\s ->
    case Email.parse s of
      Ok e -> Json.succeed e
      Err msg -> Json.fail msg
  )

-- Input types use validated types
type alias CreateUserInput =
  { email : Email
  , name : NonEmpty
  }

-- Handler receives already-validated data
createUser : {} -> CreateUserInput -> Api AppError User
createUser params body =
  do
    -- body.email is guaranteed valid Email
    -- body.name is guaranteed non-empty
    ...
```

---

## 14. Partial Updates with Field Type

For PATCH endpoints, use the `Field` type to distinguish between "unchanged", "set value", and "clear":

```
type Field a
  = Unchanged    -- field not present in JSON
  | Set a        -- field present with value
  | Clear        -- field explicitly set to null

type alias UpdateUserInput =
  { name : Field String
  , email : Field Email
  , bio : Field String
  }

-- JSON parsing:
-- {}                        => { name = Unchanged, email = Unchanged, bio = Unchanged }
-- { "name": "Alice" }       => { name = Set "Alice", email = Unchanged, bio = Unchanged }
-- { "bio": null }           => { name = Unchanged, email = Unchanged, bio = Clear }

-- Query builder understands Field
Database.update users
  |> Database.set body
  |> Database.where_ (users.id |> Database.eq params.id)
  |> Database.run ctx.db

-- Generates: UPDATE users SET name = 'Alice' WHERE id = 1
-- (unchanged fields not included, Clear sets NULL)
```

Use `Api.patch` for partial update endpoints:

```
Api.patch "/users/:id" updateUser

updateUser : { id : Int } -> UpdateUserInput -> Api AppError User
```

---

## 15. Open Questions (Resolved)

All initial open questions have been addressed:

| Question | Resolution |
|----------|------------|
| Query parameters | Declared at route level, flow into params record |
| Header access | `Api.header` accessor, shorthand for context access |
| Authentication | Route groups with `requireAuth`, user injected into params |
| Request validation | Validated newtypes with smart constructors |
| Partial updates | `Field` type with `Unchanged`, `Set`, `Clear` variants |
