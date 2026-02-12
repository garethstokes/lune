# ORM-like Schema Derive

## Overview

Add compile-time schema derivation to reduce database boilerplate. A `@derive(Table ...)` annotation on a type alias generates table refs, field refs, decoders, and CRUD helpers automatically.

Inspired by Drizzle ORM's schema-as-code approach, but integrated into the Lune compiler.

## Goals

- Eliminate manual decoder writing
- Eliminate duplicate table/field definitions
- Generate type-safe CRUD helpers
- Keep queries explicit (no magic relationship hopping)

## Non-Goals (Future Work)

- Migration generation
- Schema diffing
- Relationship traversal (users handle with explicit view queries)

---

## Schema Definition Syntax

```lune
@derive(Table "users")
type alias User =
  { id : Int @primaryKey @serial
  , name : String
  , email : Maybe String
  , role : String @default("user")
  , bio : String @varchar(500)
  , createdAt : Time @default(now)
  , updatedAt : Time @default(now) @onUpdate(now)
  }
```

### Type Inference Rules

| Lune Type | Default SQL Type | Overrides |
|-----------|------------------|-----------|
| `Int` | `INTEGER` | `@serial`, `@bigint`, `@smallint` |
| `String` | `TEXT` | `@varchar(n)`, `@char(n)` |
| `Float` | `DOUBLE PRECISION` | `@real` |
| `Bool` | `BOOLEAN` | - |
| `Time` | `TIMESTAMP` | `@date`, `@timestamptz`, `@time` |
| `Maybe a` | nullable version of inner type | - |

### Field Annotations

| Annotation | Description |
|------------|-------------|
| `@primaryKey` | Marks primary key (required for CRUD generation) |
| `@serial` | Auto-incrementing integer |
| `@default(value)` | Default value (`now`, literals) |
| `@onUpdate(value)` | Update trigger value |
| `@unique` | Unique constraint |
| `@varchar(n)` | String with max length |
| `@char(n)` | Fixed-length string |

---

## Generated Code

From the schema above, the compiler generates:

### Table Reference

```lune
users : Table
users = table "users"
```

### Field References

```lune
users_id : Field Int
users_id = field users "id"

users_name : Field String
users_name = field users "name"

users_email : Field (Maybe String)
users_email = field users "email"

users_role : Field String
users_role = field users "role"

users_bio : Field String
users_bio = field users "bio"

users_createdAt : Field Time
users_createdAt = field users "createdAt"

users_updatedAt : Field Time
users_updatedAt = field users "updatedAt"
```

### Decoder

```lune
userDecoder : Decoder User
userDecoder =
  map7 (\id name email role bio createdAt updatedAt ->
        { id = id, name = name, email = email, role = role
        , bio = bio, createdAt = createdAt, updatedAt = updatedAt })
    (index 0 int)
    (index 1 string)
    (index 2 (nullable string))
    (index 3 string)
    (index 4 string)
    (index 5 time)
    (index 6 time)
```

### CRUD Helpers

```lune
-- Find by primary key
findUserById : Int -> Query (Maybe User)

-- Find all records
findAllUsers : Query (List User)

-- Insert (without serial primary key field)
insertUser : { name : String, email : Maybe String, role : String, bio : String, createdAt : Time, updatedAt : Time } -> Query User

-- Update by primary key with explicit assignments
updateUser : Int -> List Assignment -> Query User

-- Delete by primary key
deleteUser : Int -> Query Unit
```

### Naming Convention

- Table name `"users"` → `users : Table`
- Fields → `users_<fieldName> : Field a`
- Decoder → `userDecoder` (singular from type alias)
- CRUD → `findUserById`, `findAllUsers`, `insertUser`, `updateUser`, `deleteUser`

---

## Usage Example

```lune
module UserService exposing (getUser, createUser)

import Lune.Database.Postgres.Pool as Pool
import Lune.Database.Postgres.Query as PgQuery
import Lune.Database.Query as Q
import Lune.Database exposing (string)

@derive(Table "users")
type alias User =
  { id : Int @primaryKey @serial
  , name : String
  , email : Maybe String
  }

getUser : Pool -> Int -> IO (Result PgError (Maybe User))
getUser pool id =
  Pool.withConnection pool (\conn ->
    PgQuery.queryOne conn (findUserById id)
  )

createUser : Pool -> String -> Maybe String -> IO (Result PgError User)
createUser pool name email =
  Pool.withConnection pool (\conn ->
    PgQuery.queryOne conn (insertUser { name = name, email = email })
  )

-- Custom query using generated fields
findUsersByRole : String -> Query (List User)
findUsersByRole role =
  Q.select users userDecoder
    |> Q.where_ (Q.eq users_role (string role))
    |> Q.orderBy users_name Q.Asc
```

---

## Compiler Implementation

### Phase: After Parsing, Before Type Checking

1. **Parser** recognizes `@derive(Table "tablename")` on type alias declarations
2. **Parser** recognizes field-level annotations (`@primaryKey`, `@serial`, etc.)
3. **Derive expansion pass** runs before type checking:
   - Finds all `@derive(Table ...)` type aliases
   - Extracts field names, Lune types, and annotations
   - Generates AST nodes for table, fields, decoder, CRUD helpers
   - Injects generated declarations into the module AST

### New Compiler Components

```
src/Lune/Derive.hs           -- Derive expansion driver
src/Lune/Derive/Table.hs     -- Table-specific code generation
src/Lune/Syntax.hs           -- Add annotation AST nodes
src/Lune/Parser.hs           -- Parse @annotation syntax
```

### Error Handling

| Condition | Error |
|-----------|-------|
| Unknown annotation | `Unknown annotation @foo. Did you mean @default?` |
| Missing `@primaryKey` | `@derive(Table) requires exactly one @primaryKey field` |
| Unsupported type | `Type 'CustomType' has no SQL mapping. Supported: Int, String, Float, Bool, Time, Maybe` |
| Invalid annotation combo | `@serial can only be used with Int fields` |

### Generated Code is Normal AST

The expansion produces regular Lune AST nodes. No special runtime behavior - type checking and evaluation proceed normally after expansion.

---

## Query Builder Updates

Rename `Column` to `Field` throughout `Lune.Database.Query`:

```lune
type alias Field a = { tableName : String, fieldName : String }

field : Table -> String -> Field a

eq      : Field a -> DbValue -> Condition
neq     : Field a -> DbValue -> Condition
gt      : Field a -> DbValue -> Condition
lt      : Field a -> DbValue -> Condition
gte     : Field a -> DbValue -> Condition
lte     : Field a -> DbValue -> Condition
like    : Field String -> DbValue -> Condition
isNull  : Field a -> Condition
isNotNull : Field a -> Condition
in_     : Field a -> List DbValue -> Condition

set     : Field a -> DbValue -> Assignment
orderBy : Field a -> Order -> Query b -> Query b
```

---

## Implementation Tasks

1. **Parser: Add annotation syntax**
   - `@name` and `@name(args)` on declarations and record fields
   - Store in AST as `Annotation { name : String, args : Maybe Expr }`

2. **AST: Add annotation nodes**
   - Extend `Decl` with annotations list
   - Extend record field representation with annotations

3. **Derive pass: Expansion driver**
   - Run after parsing, before desugaring
   - Find `@derive(...)` declarations, dispatch to handlers

4. **Derive/Table: Table code generation**
   - Extract schema from type alias + annotations
   - Generate table, fields, decoder, CRUD helpers
   - Inject into module AST

5. **Query module: Rename Column to Field**
   - Update `Lune.Database.Query` exports and implementation
   - Update existing code using Column

6. **Tests: Golden tests for derive expansion**
   - Schema in → generated code out
   - Error cases for invalid annotations

---

## Future Enhancements

- **Composite primary keys** - `@primaryKey` on multiple fields
- **Indexes** - `@index`, `@uniqueIndex` annotations
- **Foreign keys** - `@references("other_table", "field")` for documentation/migrations
- **Custom SQL types** - `@sqlType("JSONB")` escape hatch
- **Migration generation** - Diff schema against database, generate SQL
