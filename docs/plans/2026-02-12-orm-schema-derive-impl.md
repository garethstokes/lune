# ORM Schema Derive Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add compile-time `@derive(Table "name")` annotation that generates table refs, field refs, decoders, and CRUD helpers from type alias definitions.

**Architecture:** Extend the parser to recognize `@annotation` syntax on declarations and record fields. Add a derive expansion pass between parsing and resolution that finds `@derive(Table ...)` type aliases, extracts schema info, and injects generated AST declarations. The generated code uses existing `Lune.Database.Query` types (renamed from Column to Field).

**Tech Stack:** Haskell (Megaparsec parser), existing Lune AST in `Lune.Syntax`

---

## Task 1: Rename Column to Field in Query Module

Prerequisite refactor to align naming before derive generates Field references.

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Update exports and type alias**

Replace all occurrences of `Column` with `Field` and `column` with `field`:

```lune
module Lune.Database.Query exposing (
  Table,
  Field,          -- was Column
  Query,
  ...
  field,          -- was column
  ...
)

-- | A field reference with phantom type for the field's Lune type
type alias Field a = { tableName : String, fieldName : String }  -- was columnName

-- | Create a field reference
field : Table -> String -> Field a
field tbl fldName = { tableName = tbl.name, fieldName = fldName }
```

**Step 2: Update all internal usages**

Search and replace throughout the file:
- `Column` → `Field`
- `column` → `field`
- `columnName` → `fieldName`
- `col` → `fld` (in variable names)

**Step 3: Run tests to verify refactor**

Run: `cabal test`
Expected: Some golden tests fail (core output changed)

**Step 4: Accept golden test changes**

Run: `cabal test --test-options="--accept"`
Expected: PASS (all 105 tests)

**Step 5: Commit**

```bash
git add -A
git commit -m "refactor: rename Column to Field in Lune.Database.Query"
```

---

## Task 2: Add Annotation AST Nodes

Extend `Lune.Syntax` to represent annotations on declarations and record fields.

**Files:**
- Modify: `src/Lune/Syntax.hs`

**Step 1: Add Annotation data type**

Add after line 43 (after `ForeignConvention`):

```haskell
data Annotation = Annotation
  { annName :: Text
  , annArgs :: Maybe Expr  -- Nothing for @foo, Just expr for @foo(expr)
  }
  deriving (Show)
```

**Step 2: Extend Decl to carry annotations**

Change `DeclTypeAlias` to include annotations:

```haskell
data Decl
  = DeclTypeSig Text QualType
  | DeclValue Text [Pattern] Expr
  | DeclType Text [Text] [TypeCtor]
  | DeclTypeAlias [Annotation] Text [Text] Type  -- Added [Annotation]
  | DeclNewtype Text [Text] Text Type
  | DeclClass Text [ClassParam] [Constraint] [ClassMethodSig]
  | DeclInstance Text Type [InstanceMethodDef]
  | DeclForeignImport ForeignConvention Text Text QualType
  deriving (Show)
```

**Step 3: Extend Type to support annotated record fields**

Change `TypeRecord` to carry field annotations:

```haskell
data Type
  = TypeCon Text
  | TypeVar Text
  | TypeApp Type Type
  | TypeArrow Type Type
  | TypeRecord [(Text, Type, [Annotation])]  -- Added [Annotation] per field
  deriving (Show)
```

**Step 4: Build to verify syntax changes compile**

Run: `cabal build 2>&1 | head -50`
Expected: Compile errors in Parser.hs, Resolve.hs, etc. (expected - we'll fix next)

**Step 5: Commit**

```bash
git add src/Lune/Syntax.hs
git commit -m "feat(syntax): add Annotation type to AST for @derive support"
```

---

## Task 3: Update Compiler to Handle New AST Shape

Fix all compiler modules that pattern match on the changed AST nodes.

**Files:**
- Modify: `src/Lune/Parser.hs`
- Modify: `src/Lune/Resolve.hs`
- Modify: `src/Lune/Desugar.hs`
- Modify: `src/Lune/Validate.hs`
- Modify: `src/Lune/Elaborate.hs`
- Modify: `src/Lune/Infer.hs`
- Modify: `src/Lune/Check.hs`

**Step 1: Fix Parser.hs - typeAliasDecl**

Update `typeAliasDecl` (around line 140) to produce empty annotation list:

```haskell
typeAliasDecl :: Parser Decl
typeAliasDecl = do
  keyword "type"
  keyword "alias"
  name <- typeConstructor
  vars <- many typeVar
  symbol "="
  scnOptional
  body <- parseType
  scn
  pure (DeclTypeAlias [] name vars body)  -- Empty annotations for now
```

**Step 2: Fix Parser.hs - recordFields**

Update `fieldType` (around line 353) to produce empty field annotations:

```haskell
    fieldType = do
      name <- ident
      sym ":"
      ty <- parseTypeWith spaceConsumer
      pure (name, ty, [])  -- Empty annotations for now
```

**Step 3: Fix Resolve.hs**

Find all pattern matches on `DeclTypeAlias` and `TypeRecord`, add the new fields:

```haskell
-- DeclTypeAlias pattern: add anns
DeclTypeAlias anns name vars body -> ...

-- TypeRecord pattern: add field annotations
TypeRecord fields -> TypeRecord [(n, resolveType t, anns) | (n, t, anns) <- fields]
```

**Step 4: Fix remaining modules similarly**

Apply same pattern to Desugar.hs, Validate.hs, Elaborate.hs, Infer.hs, Check.hs:
- `DeclTypeAlias anns name vars body`
- `TypeRecord [(name, ty, anns)]`

**Step 5: Build and verify**

Run: `cabal build`
Expected: Compiles successfully

**Step 6: Run tests**

Run: `cabal test`
Expected: All tests pass (no semantic change yet)

**Step 7: Commit**

```bash
git add -A
git commit -m "fix: update compiler modules for new annotation AST shape"
```

---

## Task 4: Parse Declaration Annotations

Add parser support for `@name` and `@name(args)` before declarations.

**Files:**
- Modify: `src/Lune/Parser.hs`

**Step 1: Add annotation parser**

Add after imports (around line 50):

```haskell
-- | Parse a single annotation: @name or @name(arg)
annotation :: Parser Annotation
annotation = do
  _ <- char '@'
  name <- identifier
  args <- optional (between (symbol "(") (symbol ")") parseExpr)
  pure (Annotation name args)

-- | Parse zero or more annotations
annotations :: Parser [Annotation]
annotations = many (try (annotation <* scnOptional))
```

**Step 2: Update typeAliasDecl to parse annotations**

```haskell
typeAliasDecl :: Parser Decl
typeAliasDecl = do
  anns <- annotations  -- NEW: parse leading annotations
  keyword "type"
  keyword "alias"
  name <- typeConstructor
  vars <- many typeVar
  symbol "="
  scnOptional
  body <- parseType
  scn
  pure (DeclTypeAlias anns name vars body)
```

**Step 3: Build and test basic parsing**

Run: `cabal build`
Expected: Compiles

**Step 4: Create test file for annotation parsing**

Create `examples/39_Derive_Schema.lune`:

```lune
module DeriveSchema exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Unit)

@derive(Table "users")
type alias User =
  { id : Int
  , name : String
  }

main : IO Unit
main = IO.println "schema test"
```

**Step 5: Test parsing**

Run: `cabal run lune -- examples/39_Derive_Schema.lune`
Expected: Parses and prints AST showing `DeclTypeAlias [Annotation "derive" (Just ...)] "User" ...`

**Step 6: Commit**

```bash
git add -A
git commit -m "feat(parser): parse @annotation syntax on type alias declarations"
```

---

## Task 5: Parse Field Annotations

Add parser support for `@name` after field type in record definitions.

**Files:**
- Modify: `src/Lune/Parser.hs`

**Step 1: Add field annotation parser**

Modify `fieldType` in `typeAtomWith` (around line 353):

```haskell
    fieldType = do
      name <- ident
      sym ":"
      ty <- parseTypeWith spaceConsumer
      fieldAnns <- many fieldAnnotation  -- NEW
      pure (name, ty, fieldAnns)

    fieldAnnotation = do
      _ <- char '@'
      annName <- identifierWith spaceConsumer
      annArgs <- optional (between (sym "(") (sym ")") (parseExprWith spaceConsumer))
      pure (Annotation annName annArgs)
```

Note: We need `parseExprWith` that takes a space consumer. If it doesn't exist, use `parseExpr` and handle spacing.

**Step 2: Simpler approach - parse field annotations inline**

Actually, let's keep it simpler. Update `fieldType`:

```haskell
    fieldType = do
      name <- ident
      sym ":"
      ty <- parseTypeWith spaceConsumer
      anns <- many (try fieldAnnotation)
      pure (name, ty, anns)

    fieldAnnotation = do
      _ <- L.symbol spaceConsumer "@"
      annName <- identifierWith spaceConsumer
      annArgs <- optional $ between (sym "(") (sym ")") $ do
        -- For now, just parse a simple expression (identifier, string, or int)
        choice
          [ StringLit . T.pack <$> stringLiteral
          , IntLit <$> intLiteral
          , Var <$> identifierWith spaceConsumer
          ]
      pure (Annotation annName annArgs)
```

**Step 3: Build and test**

Run: `cabal build`
Expected: Compiles

**Step 4: Update test file with field annotations**

Update `examples/39_Derive_Schema.lune`:

```lune
module DeriveSchema exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Unit)

@derive(Table "users")
type alias User =
  { id : Int @primaryKey @serial
  , name : String
  , email : Maybe String
  , role : String @default("user")
  }

main : IO Unit
main = IO.println "schema test"
```

**Step 5: Test parsing**

Run: `cabal run lune -- examples/39_Derive_Schema.lune`
Expected: Parses successfully showing field annotations in AST

**Step 6: Commit**

```bash
git add -A
git commit -m "feat(parser): parse @annotation syntax on record fields"
```

---

## Task 6: Create Derive Expansion Module

Create the derive expansion pass that transforms `@derive(Table ...)` into generated code.

**Files:**
- Create: `src/Lune/Derive.hs`
- Modify: `lune.cabal` (add to exposed-modules)

**Step 1: Create src/Lune/Derive.hs**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Lune.Derive
  ( expandDerives
  , DeriveError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Lune.Syntax

data DeriveError
  = MissingPrimaryKey Text        -- Type name
  | UnsupportedType Text Text     -- Type name, field name
  | UnknownAnnotation Text Text   -- Type name, annotation name
  deriving (Show)

-- | Expand all @derive annotations in a module
expandDerives :: Module -> Either DeriveError Module
expandDerives mod' = do
  newDecls <- expandDecls (modDecls mod')
  pure mod' { modDecls = newDecls }

-- | Process declarations, expanding derives and collecting generated decls
expandDecls :: [Decl] -> Either DeriveError [Decl]
expandDecls decls = do
  results <- traverse expandDecl decls
  pure (concat results)

-- | Expand a single declaration (may produce multiple)
expandDecl :: Decl -> Either DeriveError [Decl]
expandDecl decl = case decl of
  DeclTypeAlias anns name vars body ->
    case findTableDerive anns of
      Nothing -> pure [decl]  -- No @derive(Table), keep as-is
      Just tableName -> do
        generated <- generateTableDecls name tableName body
        pure (decl : generated)  -- Keep original + add generated
  _ -> pure [decl]

-- | Find @derive(Table "name") annotation
findTableDerive :: [Annotation] -> Maybe Text
findTableDerive [] = Nothing
findTableDerive (Annotation "derive" (Just (App (Var "Table") (StringLit tableName))) : _) = Just tableName
findTableDerive (_ : rest) = findTableDerive rest

-- | Generate table, field refs, decoder, and CRUD helpers
generateTableDecls :: Text -> Text -> Type -> Either DeriveError [Decl]
generateTableDecls typeName tableName body = case body of
  TypeRecord fields -> do
    let tableDecl = generateTableRef tableName
        fieldDecls = generateFieldRefs tableName fields
        -- TODO: decoder and CRUD helpers
    pure (tableDecl : fieldDecls)
  _ -> Left (UnsupportedType typeName "not a record type")

-- | Generate: tableName : Table; tableName = table "tableName"
generateTableRef :: Text -> Decl
generateTableRef tableName =
  DeclValue tableName [] (App (Var "table") (StringLit tableName))

-- | Generate field references for each record field
generateFieldRefs :: Text -> [(Text, Type, [Annotation])] -> [Decl]
generateFieldRefs tableName fields =
  [ DeclValue
      (tableName <> "_" <> fieldName)
      []
      (App (App (Var "field") (Var tableName)) (StringLit fieldName))
  | (fieldName, _, _) <- fields
  ]
```

**Step 2: Add to lune.cabal**

In `exposed-modules` section, add:
```
    Lune.Derive
```

**Step 3: Build**

Run: `cabal build`
Expected: Compiles

**Step 4: Commit**

```bash
git add -A
git commit -m "feat: add Lune.Derive module for @derive expansion"
```

---

## Task 7: Integrate Derive Pass into Pipeline

Hook the derive expansion into the compiler pipeline between parsing and resolution.

**Files:**
- Modify: `app/Main.hs`
- Modify: `src/Lune/ModuleGraph.hs`

**Step 1: Add derive import to Main.hs**

Add to imports:
```haskell
import qualified Lune.Derive as Derive
```

**Step 2: Apply derive expansion after loading**

In `runCompiler`, after `program <- ...` (around line 121), add:

```haskell
  -- Apply derive expansion to all modules
  expandedProgram <- case Derive.expandProgram program of
    Left err -> do
      putStrLn ("Derive error: " ++ show err)
      exitFailure
    Right p -> pure p
```

Then use `expandedProgram` instead of `program` for the rest.

**Step 3: Add expandProgram to Derive.hs**

Add to `src/Lune/Derive.hs`:

```haskell
import qualified Lune.ModuleGraph as MG

-- | Expand derives in all modules of a program
expandProgram :: MG.Program -> Either DeriveError MG.Program
expandProgram prog = do
  expandedMods <- traverse expandDerives (MG.progModules prog)
  pure prog { MG.progModules = expandedMods }
```

**Step 4: Build and test**

Run: `cabal build`
Run: `cabal run lune -- examples/39_Derive_Schema.lune`
Expected: Parses and processes (may fail at typecheck due to missing imports)

**Step 5: Commit**

```bash
git add -A
git commit -m "feat: integrate derive expansion into compiler pipeline"
```

---

## Task 8: Generate Decoder

Extend derive expansion to generate the row decoder.

**Files:**
- Modify: `src/Lune/Derive.hs`

**Step 1: Add decoder generation**

Add to `generateTableDecls`:

```haskell
generateTableDecls :: Text -> Text -> Type -> Either DeriveError [Decl]
generateTableDecls typeName tableName body = case body of
  TypeRecord fields -> do
    pkField <- findPrimaryKey typeName fields
    let tableDecl = generateTableRef tableName
        fieldDecls = generateFieldRefs tableName fields
        decoderDecl = generateDecoder typeName fields
    pure (tableDecl : fieldDecls ++ [decoderDecl])
  _ -> Left (UnsupportedType typeName "not a record type")

-- | Find the field marked @primaryKey
findPrimaryKey :: Text -> [(Text, Type, [Annotation])] -> Either DeriveError Text
findPrimaryKey typeName fields =
  case [name | (name, _, anns) <- fields, hasAnnotation "primaryKey" anns] of
    [pk] -> Right pk
    []   -> Left (MissingPrimaryKey typeName)
    _    -> Left (MissingPrimaryKey typeName)  -- TODO: better error for multiple

hasAnnotation :: Text -> [Annotation] -> Bool
hasAnnotation name anns = any (\(Annotation n _) -> n == name) anns

-- | Generate decoder: typeNameDecoder = mapN (\a b c -> {a=a,b=b,c=c}) (index 0 int) ...
generateDecoder :: Text -> [(Text, Type, [Annotation])] -> Decl
generateDecoder typeName fields =
  let decoderName = lowerFirst typeName <> "Decoder"
      n = length fields
      mapFn = "map" <> T.pack (show n)
      -- Build: \f1 f2 ... -> { field1 = f1, field2 = f2, ... }
      fieldNames = [name | (name, _, _) <- fields]
      lamVars = [PVar ("_f" <> T.pack (show i)) | i <- [0..n-1]]
      recordFields = [(name, Var ("_f" <> T.pack (show i))) | (name, i) <- zip fieldNames [0..]]
      lamExpr = Lam lamVars (RecordLiteral recordFields)
      -- Build: (index 0 decoder0) (index 1 decoder1) ...
      indexExprs = [App (App (Var "index") (IntLit (fromIntegral i))) (typeToDecoder ty)
                   | (i, (_, ty, _)) <- zip [0..] fields]
      -- Combine: mapN lamExpr index0 index1 ...
      fullExpr = foldl App (App (Var mapFn) lamExpr) indexExprs
  in DeclValue decoderName [] fullExpr

-- | Convert Lune type to decoder expression
typeToDecoder :: Type -> Expr
typeToDecoder ty = case ty of
  TypeCon "Int" -> Var "int"
  TypeCon "String" -> Var "string"
  TypeCon "Float" -> Var "float"
  TypeCon "Bool" -> Var "bool"
  TypeApp (TypeCon "Maybe") inner -> App (Var "nullable") (typeToDecoder inner)
  _ -> Var "string"  -- Fallback

lowerFirst :: Text -> Text
lowerFirst t = case T.uncons t of
  Nothing -> t
  Just (c, rest) -> T.cons (toLower c) rest
  where toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
```

**Step 2: Build and test**

Run: `cabal build`
Run: `cabal run lune -- --core examples/39_Derive_Schema.lune`
Expected: Shows generated decoder in Core output

**Step 3: Commit**

```bash
git add -A
git commit -m "feat(derive): generate row decoder from schema"
```

---

## Task 9: Generate CRUD Helpers

Add CRUD helper generation to derive expansion.

**Files:**
- Modify: `src/Lune/Derive.hs`

**Step 1: Add CRUD generation**

Update `generateTableDecls`:

```haskell
generateTableDecls :: Text -> Text -> Type -> Either DeriveError [Decl]
generateTableDecls typeName tableName body = case body of
  TypeRecord fields -> do
    pkField <- findPrimaryKey typeName fields
    let pkType = getPkType pkField fields
        tableDecl = generateTableRef tableName
        fieldDecls = generateFieldRefs tableName fields
        decoderDecl = generateDecoder typeName fields
        crudDecls = generateCrudHelpers typeName tableName pkField pkType fields
    pure (tableDecl : fieldDecls ++ [decoderDecl] ++ crudDecls)
  _ -> Left (UnsupportedType typeName "not a record type")

getPkType :: Text -> [(Text, Type, [Annotation])] -> Type
getPkType pkName fields =
  head [ty | (name, ty, _) <- fields, name == pkName]

generateCrudHelpers :: Text -> Text -> Text -> Type -> [(Text, Type, [Annotation])] -> [Decl]
generateCrudHelpers typeName tableName pkField pkType fields =
  [ generateFindById typeName tableName pkField
  , generateFindAll typeName tableName
  , generateInsert typeName tableName fields
  , generateUpdate typeName tableName pkField
  , generateDelete typeName tableName pkField
  ]

-- findUserById : Int -> Query (Maybe User)
-- findUserById id = select users userDecoder |> where_ (eq users_id (int id)) |> limit 1
generateFindById :: Text -> Text -> Text -> Decl
generateFindById typeName tableName pkField =
  let fnName = "find" <> typeName <> "ById"
      decoderName = lowerFirst typeName <> "Decoder"
      fieldRef = tableName <> "_" <> pkField
      body =
        App (App (Var "|>")
          (App (App (Var "|>")
            (App (App (Var "select") (Var tableName)) (Var decoderName)))
            (App (Var "where_") (App (App (Var "eq") (Var fieldRef)) (App (Var "int") (Var "id"))))))
          (App (Var "limit") (IntLit 1))
  in DeclValue fnName [PVar "id"] body

-- Similar for findAll, insert, update, delete...
generateFindAll :: Text -> Text -> Decl
generateFindAll typeName tableName =
  let fnName = "findAll" <> typeName <> "s"
      decoderName = lowerFirst typeName <> "Decoder"
  in DeclValue fnName [] (App (App (Var "select") (Var tableName)) (Var decoderName))

generateInsert :: Text -> Text -> [(Text, Type, [Annotation])] -> Decl
generateInsert typeName tableName fields =
  let fnName = "insert" <> typeName
      decoderName = lowerFirst typeName <> "Decoder"
      -- Filter out @serial fields (auto-generated)
      insertFields = [(n, t) | (n, t, anns) <- fields, not (hasAnnotation "serial" anns)]
      -- TODO: Build values expression
  in DeclValue fnName [PVar "record"]
      (App (App (Var "insert") (Var tableName)) (Var decoderName))

generateUpdate :: Text -> Text -> Text -> Decl
generateUpdate typeName tableName pkField =
  let fnName = "update" <> typeName
      decoderName = lowerFirst typeName <> "Decoder"
      fieldRef = tableName <> "_" <> pkField
  in DeclValue fnName [PVar "id", PVar "assignments"]
      (App (App (Var "|>")
        (App (App (Var "update") (Var tableName)) (Var decoderName)))
        (App (Var "where_") (App (App (Var "eq") (Var fieldRef)) (App (Var "int") (Var "id")))))

generateDelete :: Text -> Text -> Text -> Decl
generateDelete typeName tableName pkField =
  let fnName = "delete" <> typeName
      fieldRef = tableName <> "_" <> pkField
  in DeclValue fnName [PVar "id"]
      (App (App (Var "|>")
        (App (Var "delete") (Var tableName)))
        (App (Var "where_") (App (App (Var "eq") (Var fieldRef)) (App (Var "int") (Var "id")))))
```

**Step 2: Build and test**

Run: `cabal build`
Run: `cabal run lune -- --core examples/39_Derive_Schema.lune`
Expected: Shows generated CRUD helpers

**Step 3: Commit**

```bash
git add -A
git commit -m "feat(derive): generate CRUD helpers from schema"
```

---

## Task 10: Add Required Imports

The generated code needs Query module imports. Auto-inject them.

**Files:**
- Modify: `src/Lune/Derive.hs`

**Step 1: Add import injection**

Update `expandDerives`:

```haskell
expandDerives :: Module -> Either DeriveError Module
expandDerives mod' = do
  newDecls <- expandDecls (modDecls mod')
  let hasTableDerive = any isTableDeriveDecl (modDecls mod')
      newImports = if hasTableDerive
                   then modImports mod' ++ requiredImports
                   else modImports mod'
  pure mod' { modDecls = newDecls, modImports = newImports }

isTableDeriveDecl :: Decl -> Bool
isTableDeriveDecl (DeclTypeAlias anns _ _ _) = any isTableAnnotation anns
isTableDeriveDecl _ = False

isTableAnnotation :: Annotation -> Bool
isTableAnnotation (Annotation "derive" (Just (App (Var "Table") _))) = True
isTableAnnotation _ = False

requiredImports :: [Import]
requiredImports =
  [ Import "Lune.Database.Query" (Just "Q") Nothing
  , Import "Lune.Database.Decode" (Just "D") Nothing
  ]
```

**Step 2: Update generated code to use qualified names**

Update generators to use `Q.` and `D.` prefixes:
- `Q.table`, `Q.field`, `Q.select`, `Q.where_`, `Q.eq`, etc.
- `D.index`, `D.int`, `D.string`, `D.map2`, etc.

**Step 3: Build and test**

Run: `cabal build`
Run: `cabal run lune -- --typecheck examples/39_Derive_Schema.lune`
Expected: Type checks successfully

**Step 4: Commit**

```bash
git add -A
git commit -m "feat(derive): auto-inject required imports for generated code"
```

---

## Task 11: Add Golden Tests

Create golden tests for derive expansion.

**Files:**
- Create: `examples/39_Derive_Schema.lune` (already exists)
- Create: `examples/invalid/09_Derive_NoPrimaryKey.lune`

**Step 1: Finalize example file**

Ensure `examples/39_Derive_Schema.lune` is complete and demonstrates the feature.

**Step 2: Create negative test**

Create `examples/invalid/09_Derive_NoPrimaryKey.lune`:

```lune
module DeriveMissingPK exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Unit)

@derive(Table "items")
type alias Item =
  { name : String
  , price : Int
  }

main : IO Unit
main = IO.println "test"
```

**Step 3: Run tests to generate golden files**

Run: `cabal test --test-options="--accept"`
Expected: Creates new golden files

**Step 4: Verify negative test fails correctly**

Run: `cabal run lune -- --typecheck examples/invalid/09_Derive_NoPrimaryKey.lune`
Expected: Error about missing @primaryKey

**Step 5: Commit**

```bash
git add -A
git commit -m "test: add golden tests for @derive(Table) feature"
```

---

## Task 12: Update Documentation

Document the new feature in specs.

**Files:**
- Modify: `spec/lune_database_framework_v0_1.md`

**Step 1: Add Schema Derive section**

Add new section after Query Builder:

```markdown
## X. Schema Derive

Define database schemas as annotated type aliases. The compiler generates table refs, field refs, decoders, and CRUD helpers.

### Syntax

\`\`\`lune
@derive(Table "users")
type alias User =
  { id : Int @primaryKey @serial
  , name : String
  , email : Maybe String
  , role : String @default("user")
  }
\`\`\`

### Generated Code

- `users : Table`
- `users_id : Field Int`, `users_name : Field String`, etc.
- `userDecoder : Decoder User`
- `findUserById : Int -> Query (Maybe User)`
- `findAllUsers : Query (List User)`
- `insertUser : {...} -> Query User`
- `updateUser : Int -> List Assignment -> Query User`
- `deleteUser : Int -> Query Unit`
```

**Step 2: Commit**

```bash
git add -A
git commit -m "docs: document @derive(Table) schema feature"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Rename Column to Field | `prelude/Lune/Database/Query.lune` |
| 2 | Add Annotation AST nodes | `src/Lune/Syntax.hs` |
| 3 | Fix compiler for new AST | Multiple `src/Lune/*.hs` |
| 4 | Parse declaration annotations | `src/Lune/Parser.hs` |
| 5 | Parse field annotations | `src/Lune/Parser.hs` |
| 6 | Create Derive module | `src/Lune/Derive.hs` |
| 7 | Integrate into pipeline | `app/Main.hs` |
| 8 | Generate decoder | `src/Lune/Derive.hs` |
| 9 | Generate CRUD helpers | `src/Lune/Derive.hs` |
| 10 | Auto-inject imports | `src/Lune/Derive.hs` |
| 11 | Add golden tests | `examples/`, `tests/` |
| 12 | Update documentation | `spec/` |
