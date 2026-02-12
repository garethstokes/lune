-- | Derive expansion driver
--
-- Processes @derive annotations on type aliases and generates code.
-- Runs after parsing, before type checking.
module Lune.Derive
  ( expandDerives
  , DeriveError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Lune.Syntax
import Lune.Derive.Table (expandTableDerive, TableError)

-- | Errors that can occur during derive expansion
data DeriveError
  = UnknownDerive Text  -- ^ Unknown @derive(...) annotation
  | TableDeriveError TableError  -- ^ Error from Table derive
  deriving (Show)

-- | Expand all @derive annotations in a module
-- Returns the module with generated declarations and required imports added
expandDerives :: Module -> Either DeriveError Module
expandDerives m = do
  (newDecls, hasTableDerive) <- processDecls (modDecls m)
  let newImports = if hasTableDerive
        then addDeriveImports (modImports m)
        else modImports m
  pure m { modDecls = newDecls, modImports = newImports }

-- | Add imports required by Table derive
-- If a module is already imported, merge the exposing lists
addDeriveImports :: [Import] -> [Import]
addDeriveImports existing =
  let requiredImports =
        [ Import "Lune.Database.Query" Nothing (Just queryExposes)
        , Import "Lune.Database.Decode" Nothing (Just decodeExposes)
        , Import "Lune.Database" Nothing (Just dbExposes)
        ]
  in mergeImports existing requiredImports
  where
    -- Merge required imports into existing, combining exposing lists
    mergeImports :: [Import] -> [Import] -> [Import]
    mergeImports existingImps [] = existingImps
    mergeImports existingImps (req:reqs) =
      let (merged, found) = mergeOne existingImps req
      in if found
         then mergeImports merged reqs
         else mergeImports (merged ++ [req]) reqs

    -- Try to merge a required import into existing imports
    mergeOne :: [Import] -> Import -> ([Import], Bool)
    mergeOne [] _ = ([], False)
    mergeOne (imp:imps) req
      | impName imp == impName req =
          (mergeImport imp req : imps, True)
      | otherwise =
          let (rest, found) = mergeOne imps req
          in (imp : rest, found)

    -- Merge two imports of the same module
    mergeImport :: Import -> Import -> Import
    mergeImport existing req =
      case (impExposing existing, impExposing req) of
        (Nothing, _) -> existing  -- Already imports everything
        (_, Nothing) -> existing { impExposing = Nothing }  -- Upgrade to import all
        (Just existingExp, Just reqExp) ->
          existing { impExposing = Just (mergeExposes existingExp reqExp) }

    -- Merge two exposing lists, avoiding duplicates
    mergeExposes :: [Expose] -> [Expose] -> [Expose]
    mergeExposes existing required =
      existing ++ filter (\e -> not (any (sameExpose e) existing)) required

    sameExpose :: Expose -> Expose -> Bool
    sameExpose (ExposeValue n1) (ExposeValue n2) = n1 == n2
    sameExpose (ExposeType n1 _) (ExposeType n2 _) = n1 == n2
    sameExpose _ _ = False

    -- Expose all query builder functions and types
    queryExposes =
      [ ExposeType "Table" ExposeOpaque
      , ExposeType "Field" ExposeOpaque
      , ExposeType "Query" ExposeOpaque
      , ExposeType "Condition" ExposeOpaque
      , ExposeType "Assignment" ExposeOpaque
      , ExposeValue "table"
      , ExposeValue "field"
      , ExposeValue "select"
      , ExposeValue "insert"
      , ExposeValue "update"
      , ExposeValue "delete"
      , ExposeValue "where_"
      , ExposeValue "eq"
      , ExposeValue "set"
      , ExposeValue "values"
      , ExposeValue "returning"
      , ExposeValue "limit"
      ]

    -- Expose decoder functions (no 'map' to avoid conflict with Prelude)
    decodeExposes =
      [ ExposeType "Decoder" ExposeOpaque
      , ExposeValue "int"
      , ExposeValue "string"
      , ExposeValue "bool"
      , ExposeValue "float"
      , ExposeValue "nullable"
      , ExposeValue "index"
      , ExposeValue "map2"
      , ExposeValue "map3"
      , ExposeValue "map4"
      , ExposeValue "map5"
      ]

    -- Expose DbValue constructors only (helpers have name conflicts with Decode)
    dbExposes =
      [ ExposeType "DbValue" ExposeAll
      ]

-- | Process all declarations, expanding derives
-- Returns (declarations, hasTableDerive)
processDecls :: [Decl] -> Either DeriveError ([Decl], Bool)
processDecls = go [] False
  where
    go acc hasTable [] = pure (reverse acc, hasTable)
    go acc hasTable (d:ds) = do
      (expanded, isTable) <- expandDecl d
      go (expanded ++ acc) (hasTable || isTable) ds

-- | Expand a single declaration, returning (declarations, hasTableDerive)
expandDecl :: Decl -> Either DeriveError ([Decl], Bool)
expandDecl decl =
  case decl of
    DeclTypeAlias anns name vars body -> do
      (generated, hasTable) <- processAnnotations anns name vars body
      pure (decl : generated, hasTable)
    _ ->
      pure ([decl], False)

-- | Process annotations on a type alias
-- Returns (declarations, hasTableDerive)
processAnnotations :: [Annotation] -> Text -> [Text] -> Type -> Either DeriveError ([Decl], Bool)
processAnnotations anns name _vars body = do
  results <- mapM (processAnnotation name body) anns
  let (declLists, flags) = unzip results
  pure (concat declLists, or flags)

-- | Process a single annotation
-- Returns (declarations, isTableDerive)
processAnnotation :: Text -> Type -> Annotation -> Either DeriveError ([Decl], Bool)
processAnnotation typeName typeBody ann =
  case annName ann of
    "derive" ->
      processDeriveAnnotation typeName typeBody (annArgs ann)
    _ ->
      -- Ignore unknown annotations (they might be documentation or future features)
      pure ([], False)

-- | Process @derive(...) annotation
-- Returns (declarations, isTableDerive)
processDeriveAnnotation :: Text -> Type -> Maybe Expr -> Either DeriveError ([Decl], Bool)
processDeriveAnnotation typeName typeBody maybeArgs =
  case maybeArgs of
    Nothing ->
      Left (UnknownDerive "derive annotation requires arguments")
    Just args ->
      case extractDeriveKind args of
        Just ("Table", tableName) ->
          case expandTableDerive typeName tableName typeBody of
            Left err -> Left (TableDeriveError err)
            Right decls -> pure (decls, True)  -- Mark as Table derive
        Just (kind, _) ->
          Left (UnknownDerive kind)
        Nothing ->
          Left (UnknownDerive "invalid derive arguments")

-- | Extract the derive kind and first argument from an expression
-- @derive(Table "users") -> Just ("Table", "users")
extractDeriveKind :: Expr -> Maybe (Text, Text)
extractDeriveKind expr =
  case expr of
    App (Var kind) (StringLit arg) ->
      Just (kind, arg)
    Var kind ->
      Just (kind, kind)  -- For derives without string arg
    _ ->
      Nothing
