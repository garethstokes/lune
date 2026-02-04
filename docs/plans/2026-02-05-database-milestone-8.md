# Database Milestone 8: Connection Pooling

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add production-ready connection pooling using Haskell's `resource-pool` library, enabling efficient connection reuse across multiple requests.

**Architecture:** Add `resource-pool` dependency and create a `DbPool` type that wraps a `Pool Connection`. Pools are stored in world state similar to connections. The `withConnection` function borrows a connection from the pool, executes an action, and returns it. Connections are automatically returned even on error.

**Tech Stack:** Haskell `resource-pool` library; Pure Lune for the wrapper API.

---

## Task 1: Add resource-pool dependency

**Files:**
- Modify: `lune.cabal`

**Step 1: Add resource-pool to build-depends**

In the `library` section's `build-depends`, add after `postgresql-simple`:

```cabal
      resource-pool >= 0.2,
```

**Step 2: Verify it builds**

Run: `cabal build`

**Step 3: Commit**

```bash
git add lune.cabal
git commit -m "build: add resource-pool dependency"
```

---

## Task 2: Add Pool types to Eval/Types.hs

**Files:**
- Modify: `src/Lune/Eval/Types.hs`

**Step 1: Add import for Pool**

Add to imports:

```haskell
import Data.Pool (Pool)
```

**Step 2: Add DbPoolId type alias**

After `DbConnId`:

```haskell
type DbPoolId = Int
```

**Step 3: Add DbPool type**

After `DbConnection`:

```haskell
-- | Database connection pool
data DbPool = PgPool (Pool PG.Connection)
```

**Step 4: Export the new types**

Add to module exports: `DbPoolId`, `DbPool(..)`

**Step 5: Add pool fields to World**

In the `World` record, after `worldNextDbConnId`:

```haskell
  , worldDbPools :: IntMap DbPool        -- Database connection pools
  , worldNextDbPoolId :: DbPoolId        -- Next DB pool ID
```

**Step 6: Update World Show instance**

Add to the Show instance:

```haskell
         <> ", dbpools = " <> show (IntMap.size (worldDbPools w)) <> " entries"
```

**Step 7: Verify it compiles**

Run: `cabal build`

**Step 8: Commit**

```bash
git add src/Lune/Eval/Types.hs
git commit -m "feat(types): add DbPool and DbPoolId types"
```

---

## Task 3: Update World initialization

**Files:**
- Modify: `src/Lune/Eval/Runtime.hs`

**Step 1: Find emptyWorld or initial World creation**

Search for where World is initialized and add the new fields:

```haskell
  , worldDbPools = IntMap.empty
  , worldNextDbPoolId = 0
```

**Step 2: Verify it compiles**

Run: `cabal build`

**Step 3: Commit**

```bash
git add src/Lune/Eval/Runtime.hs
git commit -m "feat(runtime): initialize pool fields in World"
```

---

## Task 4: Add VDbPool value constructor

**Files:**
- Modify: `src/Lune/Eval/Types.hs`

**Step 1: Add VDbPool to Value type**

In the `Value` data type, after `VDbConn`:

```haskell
  | VDbPool DbPoolId
```

**Step 2: Verify it compiles**

Run: `cabal build`

**Step 3: Commit**

```bash
git add src/Lune/Eval/Types.hs
git commit -m "feat(types): add VDbPool value constructor"
```

---

## Task 5: Add createPool primitive

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add import for Pool**

Add to imports:

```haskell
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Time.Clock (NominalDiffTime)
```

**Step 2: Add prim_pgCreatePool type signature**

In the `builtinTypes` list, add after `prim_pgRollback`:

```haskell
    , ("prim_pgCreatePool", Forall [] []
        (TArrow (TCon "String")       -- connection string
          (TArrow (TCon "Int")        -- max connections
            (TArrow (TCon "Int")      -- idle timeout seconds
              (TApp (TCon "IO")
                (TApp (TApp (TCon "Result") (TCon "DbError")) (TCon "DbPool")))))))
```

**Step 3: Add prim_pgCreatePool to builtinEvalEnv**

In the `builtinEvalEnv` list, add after `prim_pgRollback`:

```haskell
    , ("prim_pgCreatePool", BuiltinPrim 3 primPgCreatePool)
```

**Step 4: Add primPgCreatePool implementation**

Add after `primPgRollback`:

```haskell
-- | prim_pgCreatePool : String -> Int -> Int -> IO (Result DbError DbPool)
primPgCreatePool :: [Value] -> Either EvalError Value
primPgCreatePool args =
  case args of
    [VString connStr, VInt maxConns, VInt idleTimeout] ->
      Right $ VIO $ \world -> do
        let createConn = PG.connectPostgreSQL (TE.encodeUtf8 connStr)
            destroyConn = PG.close
            stripes = 1  -- Single stripe is fine for most use cases
            idleTime = fromIntegral idleTimeout :: NominalDiffTime
            maxPerStripe = fromIntegral maxConns
        result <- try (Pool.createPool createConn destroyConn stripes idleTime maxPerStripe)
        case result of
          Left (e :: SomeException) ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.ConnectionFailed" [VString (T.pack (show e))]])
          Right pool ->
            let poolId = worldNextDbPoolId world
                world' = world
                  { worldDbPools = IntMap.insert poolId (PgPool pool) (worldDbPools world)
                  , worldNextDbPoolId = poolId + 1
                  }
            in pure $ Right (world', VCon "Lune.Prelude.Ok" [VDbPool poolId])
    _ ->
      Left (NotAFunction (VPrim 3 primPgCreatePool args))
```

**Step 5: Verify it compiles**

Run: `cabal build`

**Step 6: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(db): add prim_pgCreatePool primitive"
```

---

## Task 6: Add withConnection primitive

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add prim_pgWithConnection type signature**

In the `builtinTypes` list, add after `prim_pgCreatePool`:

```haskell
    , ("prim_pgWithConnection", Forall [("a", KStar)] []
        (TArrow (TCon "DbPool")
          (TArrow (TArrow (TCon "DbConn") (TApp (TCon "IO") (TVar "a")))
            (TApp (TCon "IO") (TVar "a")))))
```

**Step 2: Add prim_pgWithConnection to builtinEvalEnv**

In the `builtinEvalEnv` list, add after `prim_pgCreatePool`:

```haskell
    , ("prim_pgWithConnection", BuiltinPrim 2 primPgWithConnection)
```

**Step 3: Add primPgWithConnection implementation**

This is complex because we need to:
1. Get a connection from the pool
2. Create a temporary DbConn in world state
3. Run the action
4. Return the connection to the pool
5. Clean up the temporary DbConn

Add after `primPgCreatePool`:

```haskell
-- | prim_pgWithConnection : DbPool -> (DbConn -> IO a) -> IO a
primPgWithConnection :: [Value] -> Either EvalError Value
primPgWithConnection args =
  case args of
    [VDbPool poolId, actionFn] ->
      Right $ VIO $ \world ->
        case IntMap.lookup poolId (worldDbPools world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgPool pool) -> do
            Pool.withResource pool $ \conn -> do
              -- Create temporary connection in world state
              let tempConnId = worldNextDbConnId world
                  world1 = world
                    { worldDbConns = IntMap.insert tempConnId (PgConn conn) (worldDbConns world)
                    , worldNextDbConnId = tempConnId + 1
                    }
              -- Apply the action function to the connection
              case actionFn of
                VThunk env expr -> do
                  -- Need to evaluate: actionFn tempConnId
                  -- This requires access to the evaluator
                  pure $ Left (NotAFunction actionFn)
                _ ->
                  pure $ Left (NotAFunction actionFn)
    _ ->
      Left (NotAFunction (VPrim 2 primPgWithConnection args))
```

Wait - this approach won't work cleanly because we need to call back into the Lune evaluator from within the IO action. Let me revise the approach.

**Alternative approach:** Instead of a callback-style `withConnection`, we provide `acquireConnection` and `releaseConnection` primitives, and the Lune-level `withConnection` handles the bracket pattern.

Actually, looking at this more carefully, the cleanest approach is to make `withConnection` work at the Lune level using `begin`/`commit`/`rollback` style - acquire a connection, use it, release it. Let's do:

- `poolAcquire : DbPool -> IO (Result DbError DbConn)` - get connection from pool
- `poolRelease : DbPool -> DbConn -> IO (Result DbError Unit)` - return connection to pool

Then the Lune `withConnection` wrapper handles the bracket.

**Step 3 (revised): Remove the complex primitive and add simpler ones**

Delete the complex `primPgWithConnection` and instead add acquire/release:

```haskell
-- | prim_pgPoolAcquire : DbPool -> IO (Result DbError DbConn)
primPgPoolAcquire :: [Value] -> Either EvalError Value
primPgPoolAcquire args =
  case args of
    [VDbPool poolId] ->
      Right $ VIO $ \world ->
        case IntMap.lookup poolId (worldDbPools world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgPool pool) -> do
            result <- try (Pool.takeResource pool)
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.ConnectionFailed" [VString (T.pack (show e))]])
              Right (conn, localPool) ->
                let connId = worldNextDbConnId world
                    world' = world
                      { worldDbConns = IntMap.insert connId (PgConn conn) (worldDbConns world)
                      , worldNextDbConnId = connId + 1
                      }
                in pure $ Right (world', VCon "Lune.Prelude.Ok" [VDbConn connId])
    _ ->
      Left (NotAFunction (VPrim 1 primPgPoolAcquire args))
```

Hmm, but `takeResource` returns `(a, LocalPool a)` and we need the LocalPool to put it back. This makes the simple acquire/release pattern complicated.

**Final approach:** Use `withResource` inside the primitive but structure it differently. We'll create a simpler API:

- `createPool` - creates the pool
- `destroyPool` - destroys the pool
- For using connections, users will use the existing `connect`/`close` pattern, OR we provide a `withPoolConnection` that takes a callback

Actually, the cleanest solution for Lune is to make the pool manage connections internally and provide a way to "borrow" connections that are automatically tracked. Let me simplify:

**Simplest viable approach:**
1. `createPool : String -> Int -> Int -> IO (Result DbError DbPool)` - create pool
2. `destroyPool : DbPool -> IO (Result DbError Unit)` - destroy pool
3. `withConnection : DbPool -> (DbConn -> IO (Result DbError a)) -> IO (Result DbError a)` - implemented in pure Lune using a different primitive

For `withConnection` to work in pure Lune, we need a primitive that gives us a connection from the pool that we can use with existing primitives. The trick is the connection needs to be returned to the pool, not closed.

Let me use a different strategy: add a flag to connections indicating they're pooled, and modify `close` to return to pool instead of closing.

**Even simpler:** Just provide `createPool` for now, and document that connections from `connect` should be reused manually. Connection pooling at the Lune level can use `transaction` to wrap multiple operations on a single connection.

Let me go with the simplest possible M8 that still adds value:

1. `createPool` - create a pool
2. `withPooledConnection` - borrow connection, run action, return connection (using callback pattern via a different mechanism)

Actually, let me check how resource-pool's `withResource` works - it manages the acquire/release automatically. The challenge is calling back into Lune.

**Final decision:** For M8, let's implement:
1. `createPool` primitive
2. `destroyPool` primitive
3. A pure Lune `withConnection` that creates a connection, runs the action, and closes it (not true pooling, but the API is ready)
4. Document that true pooling with `withResource` would require runtime support for callbacks

This gets the API in place and the pool creation working. True pooled connection reuse would need evaluator changes to support callbacks.

Let me revise the plan with this simpler approach.

---

**Step 1 (revised): Simplify to createPool only for now**

Update the type signature:

```haskell
    , ("prim_pgCreatePool", Forall [] []
        (TArrow (TCon "String")       -- connection string
          (TArrow (TCon "Int")        -- max connections
            (TArrow (TCon "Int")      -- idle timeout seconds
              (TApp (TCon "IO")
                (TApp (TApp (TCon "Result") (TCon "DbError")) (TCon "DbPool")))))))
```

**Step 2: Keep the primPgCreatePool implementation as shown above**

**Step 3: Verify it compiles**

Run: `cabal build`

**Step 4: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(db): add prim_pgCreatePool primitive"
```

---

## Task 6: Add destroyPool primitive

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add prim_pgDestroyPool type signature**

In the `builtinTypes` list, add after `prim_pgCreatePool`:

```haskell
    , ("prim_pgDestroyPool", Forall [] []
        (TArrow (TCon "DbPool")
          (TApp (TCon "IO")
            (TApp (TApp (TCon "Result") (TCon "DbError")) (TCon "Unit")))))
```

**Step 2: Add prim_pgDestroyPool to builtinEvalEnv**

In the `builtinEvalEnv` list, add after `prim_pgCreatePool`:

```haskell
    , ("prim_pgDestroyPool", BuiltinPrim 1 primPgDestroyPool)
```

**Step 3: Add primPgDestroyPool implementation**

Add after `primPgCreatePool`:

```haskell
-- | prim_pgDestroyPool : DbPool -> IO (Result DbError Unit)
primPgDestroyPool :: [Value] -> Either EvalError Value
primPgDestroyPool args =
  case args of
    [VDbPool poolId] ->
      Right $ VIO $ \world ->
        case IntMap.lookup poolId (worldDbPools world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgPool pool) -> do
            Pool.destroyAllResources pool
            let world' = world { worldDbPools = IntMap.delete poolId (worldDbPools world) }
            pure $ Right (world', VCon "Lune.Prelude.Ok" [VCon "Lune.Prelude.Unit" []])
    _ ->
      Left (NotAFunction (VPrim 1 primPgDestroyPool args))
```

**Step 4: Verify it compiles**

Run: `cabal build`

**Step 5: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(db): add prim_pgDestroyPool primitive"
```

---

## Task 7: Add DbPool type to Lune Database module

**Files:**
- Modify: `prelude/Lune/Database.lune`

**Step 1: Add DbPool to exports**

Update the module header to include `DbPool`:

```lune
module Lune.Database exposing (
  DbConn,
  DbPool,
  DbError(..),
  ...
)
```

**Step 2: Add DbPool type**

After `DbConn`:

```lune
-- | Opaque database connection pool handle
type DbPool = DbPool#
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database.lune
git commit -m "feat(db): add DbPool type"
```

---

## Task 8: Export pool functions from Postgres module

**Files:**
- Modify: `prelude/Lune/Database/Postgres.lune`

**Step 1: Add pool functions to exports**

Update the module header:

```lune
module Lune.Database.Postgres exposing (
  connect,
  execute,
  query,
  queryAs,
  close,
  run,
  runOne,
  begin,
  commit,
  rollback,
  transaction,
  createPool,
  destroyPool,
  withConnection
)
```

**Step 2: Add import for DbPool**

Update the import:

```lune
import Lune.Database exposing (DbConn, DbPool, DbError(..), DbValue)
```

**Step 3: Add createPool function**

Add after `transaction`:

```lune
-- | Create a connection pool
-- Parameters: connection string, max connections, idle timeout (seconds)
createPool : String -> Int -> Int -> IO (Result DbError DbPool)
createPool = prim_pgCreatePool
```

**Step 4: Add destroyPool function**

Add after `createPool`:

```lune
-- | Destroy a connection pool, closing all connections
destroyPool : DbPool -> IO (Result DbError Unit)
destroyPool = prim_pgDestroyPool
```

**Step 5: Add withConnection function**

This is implemented in pure Lune - it creates a connection, runs the action, and closes it.
Note: This doesn't use true pooling yet (would require runtime callback support).

Add after `destroyPool`:

```lune
-- | Execute an action with a connection from the pool
-- Creates a new connection, runs the action, and closes it
-- Note: For production use, consider using transactions to batch operations
withConnection : DbPool -> (DbConn -> IO (Result DbError a)) -> IO (Result DbError a)
withConnection pool action =
  do
    -- For now, create a fresh connection (true pooling requires runtime support)
    -- The pool's connection string is not accessible, so we can't implement this yet
    -- This is a placeholder that will be properly implemented when runtime supports callbacks
    pure (Err (ConnectionFailed "withConnection not yet implemented - use connect/close directly"))
```

Actually, we can't implement `withConnection` properly without storing the connection string in the pool or having callback support. Let me remove it from exports and document this limitation.

**Step 5 (revised): Remove withConnection from exports**

Update exports to not include `withConnection`:

```lune
module Lune.Database.Postgres exposing (
  connect,
  execute,
  query,
  queryAs,
  close,
  run,
  runOne,
  begin,
  commit,
  rollback,
  transaction,
  createPool,
  destroyPool
)
```

**Step 6: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Postgres.lune`

**Step 7: Commit**

```bash
git add prelude/Lune/Database/Postgres.lune
git commit -m "feat(postgres): export createPool and destroyPool"
```

---

## Task 9: Create Connection Pool example

**Files:**
- Create: `examples/26_Database_Pool.lune`

**Step 1: Create the example file**

```lune
module DatabasePool exposing (main)

{-| Database Connection Pool example.

This demonstrates Milestone 8 of the database module:
- Creating a connection pool
- Destroying a connection pool
- Pool configuration options

Note: This example requires a PostgreSQL database to run.
The pool manages connection lifecycle automatically.

Future: withConnection will be added when runtime supports callbacks.
-}

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Maybe(..), Int, String, Bool(..))
import Lune.Database as Database exposing (DbConn, DbPool, DbError(..), DbValue(..))
import Lune.Database.Postgres as Postgres
import Lune.String as Str

main : IO Unit
main =
  do
    IO.println "=== Connection Pool Examples ==="
    IO.println ""
    IO.println "Connection pooling provides efficient connection reuse."
    IO.println ""
    IO.println "1. Create a pool:"
    IO.println "   pool <- Postgres.createPool connString maxConns idleTimeout"
    IO.println ""
    IO.println "   Parameters:"
    IO.println "   - connString: PostgreSQL connection string"
    IO.println "   - maxConns: Maximum number of connections"
    IO.println "   - idleTimeout: Seconds before idle connections are closed"
    IO.println ""
    IO.println "2. Example:"
    IO.println "   poolResult <- Postgres.createPool"
    IO.println "     \"postgresql://localhost/mydb\""
    IO.println "     10    -- max 10 connections"
    IO.println "     300   -- 5 minute idle timeout"
    IO.println ""
    IO.println "3. Destroy pool when done:"
    IO.println "   Postgres.destroyPool pool"
    IO.println ""
    IO.println "4. Type signatures:"
    IO.println "   createPool : String -> Int -> Int -> IO (Result DbError DbPool)"
    IO.println "   destroyPool : DbPool -> IO (Result DbError Unit)"
    IO.println ""
    IO.println "5. Usage pattern (until withConnection is available):"
    IO.println "   -- Create pool at application startup"
    IO.println "   poolResult <- Postgres.createPool connStr 10 300"
    IO.println "   case poolResult of"
    IO.println "     Err e -> handleError e"
    IO.println "     Ok pool ->"
    IO.println "       do"
    IO.println "         -- Use individual connections for operations"
    IO.println "         connResult <- Postgres.connect connStr"
    IO.println "         -- ... run queries ..."
    IO.println "         Postgres.close conn"
    IO.println "         -- Destroy pool at shutdown"
    IO.println "         Postgres.destroyPool pool"
    IO.println ""
    IO.println "Done!"
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/26_Database_Pool.lune`

**Step 3: Commit**

```bash
git add examples/26_Database_Pool.lune
git commit -m "feat(example): add Database Connection Pool example"
```

---

## Task 10: Update golden tests

**Files:**
- Modify: `tests/golden/` (various)

**Step 1: Run golden tests**

Run: `cabal test golden`

**Step 2: Accept new golden output**

Run: `cabal test golden --test-options="--accept"`

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: update golden tests for connection pooling"
```

---

## Task 11: Run example and verify

**Step 1: Build**

Run: `cabal build`

**Step 2: Run the example**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/26_Database_Pool.lune`

**Expected output:**

```
=== Connection Pool Examples ===

Connection pooling provides efficient connection reuse.

1. Create a pool:
   pool <- Postgres.createPool connString maxConns idleTimeout

   Parameters:
   - connString: PostgreSQL connection string
   - maxConns: Maximum number of connections
   - idleTimeout: Seconds before idle connections are closed

2. Example:
   poolResult <- Postgres.createPool
     "postgresql://localhost/mydb"
     10    -- max 10 connections
     300   -- 5 minute idle timeout

3. Destroy pool when done:
   Postgres.destroyPool pool

4. Type signatures:
   createPool : String -> Int -> Int -> IO (Result DbError DbPool)
   destroyPool : DbPool -> IO (Result DbError Unit)

5. Usage pattern (until withConnection is available):
   -- Create pool at application startup
   poolResult <- Postgres.createPool connStr 10 300
   case poolResult of
     Err e -> handleError e
     Ok pool ->
       do
         -- Use individual connections for operations
         connResult <- Postgres.connect connStr
         -- ... run queries ...
         Postgres.close conn
         -- Destroy pool at shutdown
         Postgres.destroyPool pool

Done!
```

---

## Summary

This milestone adds:
1. `resource-pool` dependency for connection pooling
2. `DbPool` type in Eval/Types.hs
3. `VDbPool` value constructor
4. `prim_pgCreatePool : String -> Int -> Int -> IO (Result DbError DbPool)` - create pool
5. `prim_pgDestroyPool : DbPool -> IO (Result DbError Unit)` - destroy pool
6. `DbPool` type in Lune Database module
7. `Postgres.createPool` and `Postgres.destroyPool` exports

**Limitations:**
- `withConnection` (callback-style pooled connection usage) is not implemented because the Lune runtime doesn't support calling back into Lune code from within Haskell IO actions
- Users should use `connect`/`close` directly for now, with `transaction` to batch operations

**Future work (M9+):**
- Add runtime support for callbacks to enable true `withConnection`
- Or implement a continuation-passing style that works with the current runtime

Next milestone (M9) could add Schema Definitions or Postgres-specific features (JSONB, arrays).
