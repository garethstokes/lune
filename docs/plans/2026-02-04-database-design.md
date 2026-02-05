# Lune Database Module Design

> **Status:** DRAFT
> **Date:** 2026-02-04

## Overview

A type-safe database layer for Lune, starting with PostgreSQL. Built using vertical slices - each milestone delivers working end-to-end functionality that can be tested and used.

### Design Principles

1. **Vertical slices** - Each milestone is a complete, testable feature
2. **PostgreSQL first** - Get one backend working well before abstracting
3. **Type-safe where it matters** - Parameterized queries prevent injection; typed results prevent runtime errors
4. **SQL stays SQL** - Query builder generates SQL, doesn't hide it

---

## Module Structure (Final)

```
Lune.Database              -- Core types (Connection, Error, Query)
Lune.Database.Postgres.Connection -- Postgres connection and types
Lune.Database.Query        -- Query builder DSL
Lune.Database.Schema       -- Table and Column definitions
```

---

## Milestone 1: Hello Postgres

**Goal:** Connect to PostgreSQL and execute raw SQL, proving the pure Lune wire protocol works.

**Delivers:**
```lune
import Lune.Database.Postgres.Connection as Conn
import Lune.Database.Postgres.Query as Query
import Lune.Database exposing (Connection, Error)

main : IO Unit
main =
  do
    result <- Conn.connect "localhost" 5432 "testdb" "user" Nothing
    case result of
      Err e -> IO.println (show e) -- Assuming a Show instance for the error
      Ok conn ->
        do
          queryResult <- Query.query conn "SELECT 1 as num"
          case queryResult of
            Err e -> IO.println (show e)
            Ok rows -> IO.println "Query succeeded!"
```

**Implementation:**
- Pure Lune implementation of the PostgreSQL wire protocol.
- `Connection` is an opaque type wrapping the socket and state.
- `Error` ADT: `ConnectionFailed String | QueryFailed String | ...`

**Test:** Example that connects and runs `SELECT 1`.

---

## Milestone 2: Parameterized Queries

**Goal:** Safe parameter binding to prevent SQL injection.

**Delivers:**
```lune
-- Parameters as a list
Postgres.query conn "SELECT * FROM users WHERE id = $1" [Database.int 42]

-- Returns: IO (Result Error (List (List Value)))
-- Where Value = VInt Int | VString String | VBool Bool | VNull | ...
```

**Implementation:**
- `prim_pgQuery : Connection -> String -> List Value -> IO (Result Error RawRows)`
- `Value` type for dynamic SQL values
- Parameter encoding in Haskell layer

**Test:** Insert and select with parameters.

---

## Milestone 3: Typed Result Parsing

**Goal:** Parse raw query results into typed Lune records.

**Delivers:**
```lune
type alias User =
  { id : Int
  , name : String
  , email : String
  }

-- Manual decoder (like JSON)
userDecoder : Database.Decoder User
userDecoder =
  Database.map3 (\id name email -> { id = id, name = name, email = email })
    (Database.field "id" Database.int)
    (Database.field "name" Database.string)
    (Database.field "email" Database.string)

-- Usage
result <- Postgres.queryAs conn userDecoder "SELECT id, name, email FROM users WHERE id = $1" [Database.int 42]
-- Returns: IO (Result Error (List User))
```

**Implementation:**
- `Decoder a` type (similar to JSON decoder)
- `Database.field : String -> Decoder a -> Decoder a`
- `Database.int`, `Database.string`, `Database.bool`, etc.
- `Database.map2`, `Database.map3`, etc.
- `queryAs : Connection -> Decoder a -> String -> List Value -> IO (Result Error (List a))`

**Test:** Select users with typed decoder.

---

## Milestone 4: Query Builder Foundation

**Goal:** Type-safe query construction for SELECT.

**Delivers:**
```lune
-- Table definition (just metadata, no schema enforcement yet)
users : Table
users = Database.table "users"

-- Column references
users_id : Column Int
users_id = Database.column users "id"

users_name : Column String
users_name = Database.column users "name"

-- Query builder
query : Query User
query =
  Database.select users userDecoder
    |> Database.where_ (Database.eq users_id (Database.int 42))
    |> Database.orderBy users_name Database.Asc
    |> Database.limit 10

-- Execute
result <- Database.run conn query
-- Returns: IO (Result Error (List User))
```

**Implementation:**
- `Table` type (just wraps table name for now)
- `Column a` type (table + column name + phantom type)
- `Query a` type (accumulates clauses)
- `select : Table -> Decoder a -> Query a`
- `where_ : Condition -> Query a -> Query a`
- `orderBy : Column b -> Order -> Query a -> Query a`
- `limit : Int -> Query a -> Query a`
- `run : Connection -> Query a -> IO (Result Error (List a))`
- SQL generation from Query AST

**Test:** Build and run SELECT with WHERE, ORDER BY, LIMIT.

---

## Milestone 5: INSERT, UPDATE, DELETE

**Goal:** Complete CRUD operations in query builder.

**Delivers:**
```lune
-- INSERT
insertQuery : Query User
insertQuery =
  Database.insert users userDecoder
    |> Database.values
        [ Database.set users_name (Database.string "Alice")
        , Database.set users_email (Database.string "alice@example.com")
        ]
    |> Database.returning

-- UPDATE
updateQuery : Query User
updateQuery =
  Database.update users userDecoder
    |> Database.set users_name (Database.string "Bob")
    |> Database.where_ (Database.eq users_id (Database.int 42))
    |> Database.returning

-- DELETE
deleteQuery : Query Unit
deleteQuery =
  Database.delete users
    |> Database.where_ (Database.eq users_id (Database.int 42))
```

**Implementation:**
- Extend `Query` type to handle INSERT/UPDATE/DELETE
- `insert : Table -> Decoder a -> Query a`
- `update : Table -> Decoder a -> Query a`
- `delete : Table -> Query Unit`
- `values : List Assignment -> Query a -> Query a`
- `set : Column a -> Value -> Assignment`
- `returning : Query a -> Query a`
- SQL generation for all query types

**Test:** Full CRUD cycle on users table.

---

## Milestone 6: Query Helpers

**Goal:** Convenience functions for common patterns.

**Delivers:**
```lune
-- Get one result (Maybe)
Database.one : Query a -> Query (Maybe a)

user <- Database.run conn (Database.one query)
-- Returns: IO (Result Error (Maybe User))

-- More WHERE conditions
Database.and_ : Condition -> Condition -> Condition
Database.or_ : Condition -> Condition -> Condition
Database.gt : Column a -> Value -> Condition
Database.lt : Column a -> Value -> Condition
Database.like : Column String -> String -> Condition
Database.isNull : Column a -> Condition
Database.in_ : Column a -> List Value -> Condition

-- OFFSET for pagination
Database.offset : Int -> Query a -> Query a
```

**Implementation:**
- `one` modifier that wraps result in Maybe
- Additional condition combinators
- Offset clause

**Test:** Pagination example, complex WHERE clauses.

---

## Milestone 7: Transactions

**Goal:** ACID transaction support.

**Delivers:**
```lune
Database.transaction : Connection -> (Connection -> IO (Result e a)) -> IO (Result e a)

-- Usage
result <- Database.transaction conn (\tx ->
  do
    _ <- Database.run tx insertQuery
    _ <- Database.run tx updateQuery
    Database.run tx selectQuery
)
-- Commits on Ok, rolls back on Err
```

**Implementation:**
- `prim_pgBegin : Connection -> IO (Result Error Unit)`
- `prim_pgCommit : Connection -> IO (Result Error Unit)`
- `prim_pgRollback : Connection -> IO (Result Error Unit)`
- `transaction` wrapper that handles commit/rollback

**Test:** Transaction that commits, transaction that rolls back.

---

## Milestone 8: Connection Pooling

**Goal:** Production-ready connection management.

**Delivers:**
```lune
type alias PoolConfig =
  { connectionString : String
  , maxConnections : Int
  , idleTimeout : Int
  }

pool <- Postgres.createPool config
-- Returns: IO (Result Error Pool)

-- Use connection from pool
Database.withConnection : Pool -> (Connection -> IO (Result e a)) -> IO (Result e a)

-- In Api context
type alias Context =
  { db : Pool
  }

getUser params =
  do
    ctx <- Api.context
    Database.withConnection ctx.db (\conn ->
      Database.run conn query
    )
```

**Implementation:**
- Use `resource-pool` or `pool` Haskell library
- `Pool` type wrapping Haskell pool
- `createPool : PoolConfig -> IO (Result Error Pool)`
- `withConnection : Pool -> (Connection -> IO a) -> IO a`

**Test:** Concurrent requests using pool.

---

## Milestone 9: Schema Definitions (Optional Enhancement)

**Goal:** Richer schema metadata for documentation and potential future type generation.

**Delivers:**
```lune
users : TableDef User
users =
  Database.defineTable "users"
    { id = Postgres.serial |> Column.primaryKey
    , name = Postgres.text |> Column.notNull
    , email = Postgres.text |> Column.unique
    }
```

**Note:** This is polish - the query builder works without it. Schema definitions primarily help with:
- Documentation
- Future: migration generation
- Future: compile-time query validation

---

## Milestone 10: Postgres-Specific Features

**Goal:** Expose PostgreSQL-specific functionality.

**Delivers:**
```lune
-- JSONB
Postgres.jsonb : Column Json
Postgres.jsonbContains : Column Json -> Json -> Condition

-- Arrays
Postgres.array : Column (List a)
Postgres.arrayContains : Column (List a) -> a -> Condition

-- RETURNING with specific columns
Database.returningColumns : List (Column a) -> Query a -> Query a

-- UPSERT
Database.onConflict : Column a -> ConflictAction -> Query a -> Query a
```

---

## Future Work (Not in Initial Implementation)

- **SQLite backend** - Share Query type, different connection primitives
- **Migration CLI** - `lune db migrate`, `lune db generate`
- **Compile-time query validation** - Parse SQL at compile time
- **Query logging/tracing** - Debug output of generated SQL
- **Prepared statements** - Cache compiled queries

---

## Implementation Order Summary

| Milestone | Deliverable | Dependency |
|-----------|-------------|------------|
| 1 | Connect + raw SQL | None |
| 2 | Parameterized queries | M1 |
| 3 | Typed result parsing | M2 |
| 4 | SELECT query builder | M3 |
| 5 | INSERT/UPDATE/DELETE | M4 |
| 6 | Query helpers (one, conditions) | M5 |
| 7 | Transactions | M5 |
| 8 | Connection pooling | M7 |
| 9 | Schema definitions | M5 (optional) |
| 10 | Postgres-specific | M5 (optional) |

**MVP = Milestones 1-6** (full CRUD with query builder)
**Production-ready = Milestones 1-8** (adds transactions + pooling)

---

## Example: Complete CRUD (After Milestone 6)

```lune
module UserDb exposing (getUser, listUsers, createUser, updateUser, deleteUser)

import Lune.Database as Database
import Lune.Database.Postgres as Postgres

-- Table and columns
users : Table
users = Database.table "users"

users_id : Column Int
users_id = Database.column users "id"

users_name : Column String
users_name = Database.column users "name"

users_email : Column String
users_email = Database.column users "email"

-- Decoder
type alias User =
  { id : Int
  , name : String
  , email : String
  }

userDecoder : Database.Decoder User
userDecoder =
  Database.map3 (\id name email -> { id = id, name = name, email = email })
    (Database.field "id" Database.int)
    (Database.field "name" Database.string)
    (Database.field "email" Database.string)

-- CRUD operations
getUser : Connection -> Int -> IO (Result Error (Maybe User))
getUser conn id =
  Database.select users userDecoder
    |> Database.where_ (Database.eq users_id (Database.int id))
    |> Database.one
    |> Database.run conn

listUsers : Connection -> Int -> Int -> IO (Result Error (List User))
listUsers conn page limit =
  let offset = Int.mul (Int.sub page 1) limit
  in
  Database.select users userDecoder
    |> Database.orderBy users_id Database.Asc
    |> Database.limit limit
    |> Database.offset offset
    |> Database.run conn

createUser : Connection -> String -> String -> IO (Result Error User)
createUser conn name email =
  Database.insert users userDecoder
    |> Database.values
        [ Database.set users_name (Database.string name)
        , Database.set users_email (Database.string email)
        ]
    |> Database.returning
    |> Database.runOne conn

updateUser : Connection -> Int -> String -> IO (Result Error User)
updateUser conn id name =
  Database.update users userDecoder
    |> Database.set users_name (Database.string name)
    |> Database.where_ (Database.eq users_id (Database.int id))
    |> Database.returning
    |> Database.runOne conn

deleteUser : Connection -> Int -> IO (Result Error Unit)
deleteUser conn id =
  Database.delete users
    |> Database.where_ (Database.eq users_id (Database.int id))
    |> Database.run conn
    |> IO.map (\r -> Result.map (\_ -> unit) r)
```
