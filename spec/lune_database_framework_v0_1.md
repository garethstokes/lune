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

#### Table and Field References

```
type alias Table = { name : String }
type alias Field a = { tableName : String, fieldName : String }

table : String -> Table
field : Table -> String -> Field a
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
eq        : Field a -> DbValue -> Condition
neq       : Field a -> DbValue -> Condition
gt        : Field a -> DbValue -> Condition
lt        : Field a -> DbValue -> Condition
gte       : Field a -> DbValue -> Condition
lte       : Field a -> DbValue -> Condition
like      : Field String -> DbValue -> Condition
isNull    : Field a -> Condition
isNotNull : Field a -> Condition
in_       : Field a -> List DbValue -> Condition

where_    : Condition -> Query a -> Query a
```

#### Modifiers

```
set       : Field a -> DbValue -> Assignment
values    : List Assignment -> Query a -> Query a
returning : Query a -> Query a
orderBy   : Field a -> Order -> Query b -> Query b
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

users_id : Field Int
users_id = field users "id"

users_name : Field String
users_name = field users "name"

users_active : Field Bool
users_active = field users "active"

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

users_id : Q.Field Int
users_id = Q.field users "id"

users_name : Q.Field String
users_name = Q.field users "name"

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

## 8. Schema Derive (`@derive(Table)`)

The `@derive(Table)` annotation provides ORM-like automatic code generation for database tables. Instead of manually writing table references, field references, decoders, and CRUD helpers, you declare a type alias with annotations and the compiler generates everything.

### 8.1 Basic Usage

```lune
@derive(Table "users")
type alias User =
  { id : Int @primaryKey @serial
  , name : String
  , email : Maybe String
  , isActive : Bool
  }
```

This single declaration generates:

| Generated Name | Type | Description |
|----------------|------|-------------|
| `users` | `Table` | Table reference |
| `users_id` | `Field Int` | Field reference for id |
| `users_name` | `Field String` | Field reference for name |
| `users_email` | `Field (Maybe String)` | Field reference for email |
| `users_isActive` | `Field Bool` | Field reference for isActive |
| `userDecoder` | `Decoder User` | Row decoder |
| `findUserById` | `Int -> Query User` | Find by primary key |
| `findAllUsers` | `Query User` | Select all rows |
| `insertUser` | `{ name : String, email : Maybe String, isActive : Bool } -> Query User` | Insert (serial fields omitted) |
| `updateUser` | `Int -> List Assignment -> Query User` | Update by primary key |
| `deleteUser` | `Int -> Query Unit` | Delete by primary key |

### 8.2 Field Annotations

#### `@primaryKey`

Marks the primary key field. Required for derive to work.

```lune
{ id : Int @primaryKey }
```

- Exactly one field must have `@primaryKey`
- The primary key type determines the type of `findById` and `deleteById` parameters
- Primary key is used in WHERE clauses for update/delete operations

#### `@serial`

Marks auto-generated fields (e.g., PostgreSQL `SERIAL` or `IDENTITY` columns).

```lune
{ id : Int @primaryKey @serial }
```

- Serial fields are **excluded** from the insert input type
- The database generates the value, so users don't provide it
- Multiple fields can be serial (e.g., `id` and `createdAt`)

### 8.3 Supported Field Types

| Lune Type | DbValue | Decoder | Notes |
|-----------|---------|---------|-------|
| `Int` | `DbInt` | `int` | 32/64-bit integers |
| `Float` | `DbFloat` | `float` | Floating point |
| `String` | `DbString` | `string` | Text/varchar |
| `Bool` | `DbBool` | `bool` | Boolean |
| `Maybe a` | `DbNull` or inner | `nullable` | NULL handling |

### 8.4 Generated SQL Examples

Given the User type above:

```lune
-- findAllUsers generates:
getSql findAllUsers
-- "SELECT * FROM users"

-- findUserById 42 generates:
getSql (findUserById 42)
-- "SELECT * FROM users WHERE id = $1 LIMIT 1"

-- insertUser with serial id omitted:
getSql (insertUser { name = "Alice", email = Just "alice@example.com", isActive = True })
-- "INSERT INTO users (name, email, isActive) VALUES ($1, $2, $3) RETURNING *"

-- updateUser 42 [set users_name (DbString "Bob")]:
getSql (updateUser 42 (Cons (set users_name (DbString "Bob")) Nil))
-- "UPDATE users SET name = $1 WHERE id = $2 RETURNING *"

-- deleteUser 42:
getSql (deleteUser 42)
-- "DELETE FROM users WHERE id = $1"
```

### 8.5 Using Field References

The generated field references enable type-safe query building:

```lune
-- Filter active users
activeUsers : Query User
activeUsers =
  where_ (eq users_isActive (DbBool True)) findAllUsers

-- Filter by email
findByEmail : String -> Query User
findByEmail email =
  where_ (eq users_email (DbString email)) findAllUsers

-- Order by name
sortedUsers : Query User
sortedUsers =
  orderBy users_name Asc findAllUsers

-- Combine conditions
activeUsersByName : String -> Query User
activeUsersByName name =
  findAllUsers
    |> where_ (eq users_isActive (DbBool True))
    |> where_ (eq users_name (DbString name))
```

### 8.6 Foreign Keys and Relationships

Derive generates field references for foreign key columns, enabling type-safe joins:

```lune
@derive(Table "posts")
type alias Post =
  { id : Int @primaryKey @serial
  , title : String
  , body : String
  , authorId : Int
  , published : Bool
  }

-- Find posts by author
findPostsByAuthor : Int -> Query Post
findPostsByAuthor userId =
  where_ (eq posts_authorId (DbInt userId)) findAllPosts

-- Find published posts
publishedPosts : Query Post
publishedPosts =
  where_ (eq posts_published (DbBool True)) findAllPosts
```

### 8.7 Self-Referential Tables

Tables with nullable foreign keys to themselves work naturally:

```lune
@derive(Table "categories")
type alias Category =
  { id : Int @primaryKey @serial
  , name : String
  , parentId : Maybe Int  -- NULL for root categories
  }

-- Find root categories
rootCategories : Query Category
rootCategories =
  where_ (isNull categories_parentId) findAllCategories

-- Find children of a category
childCategories : Int -> Query Category
childCategories parentId =
  where_ (eq categories_parentId (DbInt parentId)) findAllCategories
```

### 8.8 Import Handling

When `@derive(Table)` is used, the compiler automatically injects required imports:

```lune
import Lune.Database.Query exposing (Table, Field, Query, Condition, Assignment, table, field, select, insert, update, delete, where_, eq, set, values, returning, limit)
import Lune.Database.Decode exposing (Decoder, int, string, bool, float, nullable, index, map2, map3, map4, map5)
import Lune.Database exposing (DbValue(..))
```

If you already import these modules, the derive system merges the exposing lists to avoid conflicts.

### 8.9 Naming Conventions

| Source | Generated Name Pattern |
|--------|------------------------|
| Type `User` | `userDecoder`, `findUserById`, `findAllUsers`, `insertUser`, `updateUser`, `deleteUser` |
| Table `"users"` | `users` (table ref), `users_id`, `users_name`, etc. (field refs) |
| Field `name` | `users_name : Field String` |
| Field `isActive` | `users_isActive : Field Bool` |

The type name is used for decoder and CRUD helper names (with first letter lowercased for decoder).
The table name is used for table and field reference names.

### 8.10 Limitations

1. **Maximum 5 fields** - The decoder uses `mapN` combinators, currently up to `map5`. Tables with more than 5 fields require adding `map6`, `map7`, etc. to `Lune.Database.Decode`.

2. **Simple types only** - Nested records and custom types are not supported as field types.

3. **Single primary key** - Composite primary keys are not supported.

4. **No default values** - Fields cannot have default values in the type definition.

5. **No computed fields** - All fields map directly to database columns.

### 8.11 Complete Example

```lune
module Blog exposing (main)

import Lune.Prelude exposing (Int, String, Bool, Maybe(..), List(..), IO, Unit)
import Lune.Database.Query exposing (getSql, where_, eq, orderBy, Order(..))
import Lune.Database exposing (DbValue(..))

-- User table with authentication fields
@derive(Table "users")
type alias User =
  { id : Int @primaryKey @serial
  , username : String
  , email : String
  , isAdmin : Bool
  }

-- Blog post with foreign key to user
@derive(Table "posts")
type alias Post =
  { id : Int @primaryKey @serial
  , title : String
  , body : String
  , authorId : Int
  , published : Bool
  }

-- Comment with nullable parent for threading
@derive(Table "comments")
type alias Comment =
  { id : Int @primaryKey @serial
  , postId : Int
  , authorId : Int
  , body : String
  , parentId : Maybe Int
  }

-- Example queries using generated helpers
adminUsers : Query User
adminUsers = where_ (eq users_isAdmin (DbBool True)) findAllUsers

publishedPostsByAuthor : Int -> Query Post
publishedPostsByAuthor userId =
  findAllPosts
    |> where_ (eq posts_authorId (DbInt userId))
    |> where_ (eq posts_published (DbBool True))
    |> orderBy posts_id Desc

topLevelComments : Int -> Query Comment
topLevelComments postId =
  findAllComments
    |> where_ (eq comments_postId (DbInt postId))
    |> where_ (isNull comments_parentId)

-- Demo
demo : String
demo = getSql adminUsers  -- "SELECT * FROM users WHERE isAdmin = $1"
```

---

## 9. Error Handling

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
