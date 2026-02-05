# Database Examples

## Pure Lune PostgreSQL Implementation

The PostgreSQL FFI-based primitives (`prim_pg*`) have been removed in favor of a pure Lune implementation of the PostgreSQL wire protocol.

### Current Examples (Pure Lune)

- **30_Generic_Pool_Test.lune** - Demonstrates the generic connection pool with PostgreSQL
- **31_SQLite_Pool_Example.lune** - Shows how to use the generic pool with SQLite (hypothetical)

### Deprecated Examples (FFI-based - No Longer Work)

The following examples used the old FFI-based `Lune.Database.Postgres` module which has been removed:

- 20_Hello_Postgres.lune
- 21_Database_Decode.lune
- 22_Database_Query.lune
- 23_Database_CRUD.lune
- 24_Database_Helpers.lune
- 25_Database_Transaction.lune
- 26_Database_Pool.lune

These examples demonstrate concepts that are still valid, but need to be rewritten to use the pure Lune modules:

```lune
-- Old FFI approach (REMOVED):
import Lune.Database.Postgres as Postgres
result <- Postgres.connect "postgresql://localhost/testdb"

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
