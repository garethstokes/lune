# Database Examples

## Pure Lune PostgreSQL Implementation

The PostgreSQL FFI-based primitives (`prim_pg*`) have been removed in favor of a pure Lune implementation of the PostgreSQL wire protocol.

### Current Examples (Pure Lune)

- **30_Generic_Pool_Test.lune** - Demonstrates the generic connection pool with PostgreSQL
- **31_SQLite_Pool_Example.lune** - Shows how to use the generic pool with SQLite (hypothetical)

### New Pure Lune API

The new API uses modules for connection, querying, and pooling.

```lune
-- New pure Lune approach:
import Lune.Database.Postgres.Connection as Conn
import Lune.Database.Postgres.Query as Query
import Lune.Database.Postgres.Pool as Pool

-- Direct connection
result <- Conn.connect "localhost" 5432 "testdb" "user"

-- With pooling
pool <- Pool.createPool
  { host = "localhost"
  , port = 5432
  , database = "testdb"
  , user = "user"
  , password = Nothing
  , maxConnections = 10
  }
result <- Pool.withConnection pool (\conn -> Query.query conn "SELECT 1")
```

## Pure Lune Modules

- `Lune.Database.Pool` - Generic STM-based connection pool
- `Lune.Database.Postgres.Connection` - PostgreSQL connection management
- `Lune.Database.Postgres.Protocol` - PostgreSQL wire protocol implementation
- `Lune.Database.Postgres.Pool` - PostgreSQL-specific pool wrapper
- `Lune.Database.Postgres.Query` - Query execution

## Benefits of Pure Lune Implementation

1. **No Runtime Dependencies** - No need for postgresql-simple or libpq
2. **Full Control** - Complete control over connection lifecycle
3. **Generic Pool** - Same pool logic works for any database
4. **Type Safety** - Database-specific error types (PgError, SqliteError, etc.)
5. **Portable** - Wire protocol implementation in pure Lune