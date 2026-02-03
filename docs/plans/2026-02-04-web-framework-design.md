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
  { name : String
  , email : String
  }

type alias UpdateUserInput =
  { name : Maybe String
  , email : Maybe String
  }

-- ============================================================================
-- Errors
-- ============================================================================

type AppError
  = NotFound String
  | BadRequest String
  | Conflict String
  | DbError Database.Error

errorToResponse : AppError -> Response
errorToResponse err =
  case err of
    NotFound msg -> Response.json 404 { error = msg }
    BadRequest msg -> Response.json 400 { error = msg }
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
    [ Api.get "/users" listUsers
    , Api.get "/users/:id" getUser
    , Api.post "/users" createUser
    , Api.put "/users/:id" updateUser
    , Api.delete "/users/:id" deleteUser
    ]

-- ============================================================================
-- Handlers
-- ============================================================================

listUsers : {} -> Api AppError (List User)
listUsers params =
  do
    ctx <- Api.context
    Database.select users
      |> Database.orderBy users.id Asc
      |> Database.run ctx.db
      |> Api.mapError DbError

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

createUser : {} -> CreateUserInput -> Api AppError User
createUser params body =
  do
    ctx <- Api.context
    Database.insert users
      |> Database.values { name = body.name, email = body.email }
      |> Database.returning All
      |> Database.run ctx.db
      |> Api.mapError DbError

updateUser : { id : Int } -> UpdateUserInput -> Api AppError User
updateUser params body =
  do
    ctx <- Api.context
    _ <- getUser params  -- ensure exists
    Database.update users
      |> Database.set body
      |> Database.where_ (users.id |> Database.eq params.id)
      |> Database.returning All
      |> Database.run ctx.db
      |> Api.mapError DbError

deleteUser : { id : Int } -> Api AppError Unit
deleteUser params =
  do
    ctx <- Api.context
    _ <- getUser params  -- ensure exists
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

## 10. Open Questions

1. **Query parameter handling** - How should `?page=2&limit=10` flow into handlers?
2. **Header access** - Should headers be part of params record or separate accessor?
3. **Authentication pattern** - How to express "this route requires auth" at the type level?
4. **Request validation** - Beyond JSON parsing, how to express field-level validation?
5. **Partial updates** - How should `UpdateUserInput` with `Maybe` fields translate to SQL?
