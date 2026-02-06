# Lune Database Framework Specification v0.1

## Overview

The Lune database framework provides type-safe database access with composable query building, typed row decoding, and connection pooling. It is designed around explicit effects, Result-based error handling, and STM-based concurrency.

### Design Principles

1. **Type-safe queries** - Build SQL with composable functions, not string concatenation
2. **Explicit decoding** - Row data is decoded into typed records via decoders
3. **Backend-agnostic core** - Query builder and decoders work with any backend
4. **Connection pooling** - STM-based pool with automatic acquire/release
5. **Pure Lune implementation** - Wire protocols implemented in Lune over TLS sockets

---

## 1. Module Structure

```
Lune.Database                    -- Core types (DbValue, DbError)
Lune.Database.Query              -- Type-safe query builder
Lune.Database.Decode             -- Row decoders
Lune.Database.Pool               -- Generic STM-based connection pool

Lune.Database.Postgres           -- PostgreSQL backend
Lune.Database.Postgres.Connection  -- TLS connection and handshake
Lune.Database.Postgres.Query       -- Query execution
Lune.Database.Postgres.Pool        -- PostgreSQL-specific pool wrapper
Lune.Database.Postgres.Protocol    -- Wire protocol (internal)
```

---

## 2. Core Types

### Lune.Database

```
-- Database value for query parameters
type DbValue =
  DbNull
  | DbInt Int
  | DbFloat Float
  | DbString String
  | DbBool Bool

-- Database errors
type DbError =
  ConnectionFailed String
  | QueryFailed String
  | InvalidConnection

-- Constructors
null   : DbValue
int    : Int -> DbValue
float  : Float -> DbValue
string : String -> DbValue
bool   : Bool -> DbValue

errorToString : DbError -> String
```

---

## 3. Query Builder

### Lune.Database.Query

The query builder constructs SQL from composable functions. Queries carry a phantom type for the result and a decoder for type-safe row extraction.

#### Table and Column References

```
type alias Table = { name : String }
type alias Column a = { tableName : String, columnName : String }

table  : String -> Table
column : Table -> String -> Column a
```

#### Query Types

```
type QueryType = Select | Insert | Update | Delete
type Order = Asc | Desc

type alias Query a =
  { tableName : String
  , decoder : Decoder a
  , queryType : QueryType
  , conditions : List Condition
  , assignments : List Assignment
  , orderBy : Maybe { column : String, order : Order }
  , limitCount : Maybe Int
  , offsetCount : Maybe Int
  , returningFlag : Bool
  }
```

#### Starting Queries

```
select : Table -> Decoder a -> Query a
insert : Table -> Decoder a -> Query a
update : Table -> Decoder a -> Query a
delete : Table -> Query Unit
```

#### WHERE Conditions

```
eq        : Column a -> DbValue -> Condition
neq       : Column a -> DbValue -> Condition
gt        : Column a -> DbValue -> Condition
lt        : Column a -> DbValue -> Condition
gte       : Column a -> DbValue -> Condition
lte       : Column a -> DbValue -> Condition
like      : Column String -> DbValue -> Condition
isNull    : Column a -> Condition
isNotNull : Column a -> Condition
in_       : Column a -> List DbValue -> Condition

where_    : Condition -> Query a -> Query a
```

#### Modifiers

```
set       : Column a -> DbValue -> Assignment
values    : List Assignment -> Query a -> Query a
returning : Query a -> Query a
orderBy   : Column a -> Order -> Query b -> Query b
limit     : Int -> Query a -> Query a
offset    : Int -> Query a -> Query a
```

#### SQL Generation

```
toSql     : Query a -> { sql : String, params : List DbValue }
getSql    : Query a -> String
getParams : Query a -> List DbValue
getDecoder : Query a -> Decoder a
```

### Example

```
users : Table
users = table "users"

users_id : Column Int
users_id = column users "id"

users_name : Column String
users_name = column users "name"

users_active : Column Bool
users_active = column users "active"

-- SELECT * FROM users WHERE active = true ORDER BY name ASC LIMIT 10
activeUsersQuery : Query User
activeUsersQuery =
  select users userDecoder
    |> where_ (eq users_active (bool True))
    |> orderBy users_name Asc
    |> limit 10

-- INSERT INTO users (name, active) VALUES ($1, $2) RETURNING *
insertUserQuery : String -> Query User
insertUserQuery name =
  insert users userDecoder
    |> values
        [ set users_name (string name)
        , set users_active (bool True)
        ]
    |> returning
```

---

## 4. Row Decoders

### Lune.Database.Decode

Decoders extract typed values from database rows by column index.

```
type alias Decoder a = List DbValue -> Result DecodeError a
type alias DecodeError = { message : String, column : Int }
```

#### Primitives

```
int      : Decoder Int
float    : Decoder Float
string   : Decoder String
bool     : Decoder Bool
nullable : Decoder a -> Decoder (Maybe a)
```

#### Combinators

```
succeed  : a -> Decoder a
fail     : String -> Decoder a
index    : Int -> Decoder a -> Decoder a
map      : (a -> b) -> Decoder a -> Decoder b
map2     : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map3     : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map4     : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map5     : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
andThen  : (a -> Decoder b) -> Decoder a -> Decoder b
```

#### Running Decoders

```
decodeRow     : Decoder a -> List DbValue -> Result DecodeError a
errorToString : DecodeError -> String
```

### Example

```
type alias User =
  { id : Int
  , name : String
  , email : Maybe String
  }

userDecoder : Decoder User
userDecoder =
  map3 (\id name email -> { id = id, name = name, email = email })
    (index 0 int)
    (index 1 string)
    (index 2 (nullable string))
```

---

## 5. Connection Pool

### Lune.Database.Pool

A generic STM-based connection pool that works with any database backend.

```
type alias ConnectionOps conn err =
  { open : Unit -> IO (Result err conn)
  , close : conn -> IO (Result err Unit)
  }

type alias Pool conn err =
  { maxConnections : Int
  , idle : Shared (List conn)
  , inUse : Shared Int
  , closed : Shared Bool
  , ops : ConnectionOps conn err
  }
```

#### Operations

```
createPool     : Int -> ConnectionOps conn err -> IO (Pool conn err)
destroyPool    : Pool conn err -> IO Unit
withConnection : Pool conn err -> (conn -> IO (Result err a)) -> IO (Result err a)
```

#### Behavior

- Connections are created lazily up to `maxConnections`
- `withConnection` blocks (via STM retry) when pool is exhausted
- Connections are automatically released after use, even on error
- Uses STM for thread-safe acquire/release without locks

---

## 6. PostgreSQL Backend

### Lune.Database.Postgres.Connection

```
type alias PgConn =
  { tlsConn : TlsConn
  , backendPid : Int
  , backendSecret : Int
  }

connect             : String -> Int -> String -> String -> IO (Result PgError PgConn)
connectWithPassword : String -> Int -> String -> String -> String -> IO (Result PgError PgConn)
close               : PgConn -> IO (Result PgError Unit)
```

### Lune.Database.Postgres.Query

```
query       : PgConn -> String -> IO (Result PgError (List (List (Maybe Bytes))))
execute     : PgConn -> String -> IO (Result PgError Int)
begin       : PgConn -> IO (Result PgError Unit)
commit      : PgConn -> IO (Result PgError Unit)
rollback    : PgConn -> IO (Result PgError Unit)
transaction : PgConn -> (PgConn -> IO (Result PgError a)) -> IO (Result PgError a)
```

### Lune.Database.Postgres.Pool

```
type alias PoolConfig =
  { host : String
  , port : Int
  , database : String
  , user : String
  , password : Maybe String
  , maxConnections : Int
  }

createPool     : PoolConfig -> IO Pool
destroyPool    : Pool -> IO Unit
withConnection : Pool -> (PgConn -> IO (Result PgError a)) -> IO (Result PgError a)
```

---

## 7. Complete Example

```
module UserService exposing (main)

import Lune.IO as IO
import Lune.Database exposing (DbValue(..))
import Lune.Database as Db
import Lune.Database.Query as Q
import Lune.Database.Decode as D
import Lune.Database.Postgres.Pool as Pool
import Lune.Database.Postgres.Query as PgQuery
import Lune.Prelude exposing (IO, Result(..), Unit, Maybe(..), List(..))

-- Schema
users : Q.Table
users = Q.table "users"

users_id : Q.Column Int
users_id = Q.column users "id"

users_name : Q.Column String
users_name = Q.column users "name"

-- Decoder
type alias User = { id : Int, name : String }

userDecoder : D.Decoder User
userDecoder =
  D.map2 (\id name -> { id = id, name = name })
    (D.index 0 D.int)
    (D.index 1 D.string)

-- Query
findUserById : Int -> Q.Query User
findUserById id =
  Q.select users userDecoder
    |> Q.where_ (Q.eq users_id (Db.int id))
    |> Q.limit 1

main : IO Unit
main =
  do
    pool <- Pool.createPool
      { host = "localhost"
      , port = 5432
      , database = "myapp"
      , user = "myuser"
      , password = Nothing
      , maxConnections = 10
      }
    result <- Pool.withConnection pool (\conn ->
      do
        let q = findUserById 42
        rows <- PgQuery.query conn (Q.getSql q)
        case rows of
          Err e -> pure (Err e)
          Ok rawRows ->
            case rawRows of
              Nil -> pure (Err { severity = "ERROR", code = "02000", message = "User not found" })
              Cons row _ ->
                -- Decode the row (simplified - real code would convert Bytes to DbValue)
                pure (Ok { id = 42, name = "Alice" })
    )
    case result of
      Ok user -> IO.println user.name
      Err e -> IO.println e.message
    Pool.destroyPool pool
```

---

## 8. Error Handling

### PostgreSQL Errors

```
type alias PgError =
  { severity : String   -- "ERROR", "FATAL", "WARNING", etc.
  , code : String       -- PostgreSQL error code (e.g., "23505" for unique violation)
  , message : String    -- Human-readable error message
  }
```

### Decode Errors

```
type alias DecodeError =
  { message : String
  , column : Int
  }
```

All database operations return `Result` types, enabling explicit error handling without exceptions.

---

## 9. Future Work

### 9.1 Database Features

- **Prepared statements** - Parse once, execute many times with different parameters
- **Streaming results** - Lazy iteration over large result sets without loading all rows into memory
- **LISTEN/NOTIFY** - PostgreSQL pub/sub for real-time notifications
- **Connection health checks** - Periodic ping to detect stale connections
- **Migrations** - Version-controlled schema changes with up/down scripts
- **Schema introspection** - Query database metadata for tables, columns, and constraints

### 9.2 Additional Backends

- **SQLite** - Embedded database for local applications and testing
- **MySQL/MariaDB** - Popular alternative to PostgreSQL
- **Backend abstraction** - Common interface for backend-agnostic code

### 9.3 Query Builder Enhancements

- **JOINs** - INNER, LEFT, RIGHT, and FULL OUTER joins with typed column resolution
- **Subqueries** - Nested queries in WHERE, FROM, and SELECT clauses
- **Aggregations** - COUNT, SUM, AVG, MIN, MAX with GROUP BY and HAVING
- **Raw SQL escape hatch** - Embed raw SQL fragments when the builder is insufficient
- **UPSERT** - INSERT ... ON CONFLICT for atomic insert-or-update
- **Batch operations** - Insert/update multiple rows in a single query
- **Common Table Expressions (CTEs)** - WITH clauses for complex queries

### 9.4 ORM-like Features

- **Model definition** - Declarative schema with automatic decoder generation
- **Relationships** - hasOne, hasMany, belongsTo with typed traversal
- **Lazy loading** - Defer relationship loading until accessed
- **Change tracking** - Detect modified fields for minimal UPDATE statements
- **Validation** - Field-level constraints checked before persistence
- **Code generation** - Generate Lune types and decoders from database schema

### 9.5 Transactions and Savepoints

- **Savepoints** - Nested transaction boundaries for partial rollback
- **Read-only transactions** - Hint for read replicas and optimization
- **Isolation levels** - Configure READ COMMITTED, REPEATABLE READ, SERIALIZABLE

### 9.6 Observability

- **Query logging** - Log SQL and parameters for debugging
- **Timing metrics** - Measure query execution time
- **Slow query detection** - Warn on queries exceeding threshold
- **Connection pool metrics** - Track pool utilization and wait times

---

## 10. Versioning

This document defines the database framework surface for **Lune v0.1**.

Changes to exported names or semantics require a new version of this specification.
