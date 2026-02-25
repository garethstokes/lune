-- | Derive expansion driver
--
-- Processes @derive annotations on type aliases and generates code.
-- Runs after parsing, before type checking.
module Lune.Derive
  ( expandDerives
  , DeriveError(..)
  ) where

import Data.Text (Text)
import Lune.Syntax
import Lune.Derive.Table (expandTableDerive, TableError)
import Lune.Derive.Json (generateJsonForAliasRecord, generateJsonForAdt, JsonError)

-- | Errors that can occur during derive expansion
data DeriveError
  = UnknownDerive Text  -- ^ Unknown @derive(...) annotation
  | TableDeriveError TableError  -- ^ Error from Table derive
  | JsonDeriveError JsonError  -- ^ Error from Json derive
  deriving (Show)

data DeriveFlags = DeriveFlags
  { dfHasTableDerive :: Bool
  , dfHasJsonDerive :: Bool
  }
  deriving (Eq, Show)

emptyFlags :: DeriveFlags
emptyFlags =
  DeriveFlags
    { dfHasTableDerive = False
    , dfHasJsonDerive = False
    }

combineFlags :: DeriveFlags -> DeriveFlags -> DeriveFlags
combineFlags a b =
  DeriveFlags
    { dfHasTableDerive = dfHasTableDerive a || dfHasTableDerive b
    , dfHasJsonDerive = dfHasJsonDerive a || dfHasJsonDerive b
    }

-- | Expand all @derive annotations in a module
-- Returns the module with generated declarations and required imports added
expandDerives :: Module -> Either DeriveError Module
expandDerives m = do
  (newDecls, flags) <- processDecls (modDecls m)
  let imports1 =
        if dfHasTableDerive flags
          then addDeriveImports (modImports m)
          else modImports m
      newImports =
        if dfHasJsonDerive flags
          then addJsonDeriveImports imports1
          else imports1
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

-- | Add imports required by Json derive.
--
-- Inserts `import Lune.Json.Decode as D` and `import Lune.Json.Encode as E` if missing.
addJsonDeriveImports :: [Import] -> [Import]
addJsonDeriveImports existing =
  let requiredImports =
        [ Import "Lune.Json.Decode" (Just "D") Nothing
        , Import "Lune.Json.Encode" (Just "E") Nothing
        ]
   in foldl (\acc req -> ensureImport req acc) existing requiredImports
  where
    ensureImport req acc =
      if any (sameImport req) acc
        then acc
        else acc ++ [req]

    sameImport a b =
      impName a == impName b && impAs a == impAs b

-- | Process all declarations, expanding derives
-- Returns (declarations, derive flags)
processDecls :: [Decl] -> Either DeriveError ([Decl], DeriveFlags)
processDecls = go [] emptyFlags
  where
    go acc flags [] = pure (reverse acc, flags)
    go acc flags (d:ds) = do
      (expanded, flags') <- expandDecl d
      go (reverse expanded ++ acc) (combineFlags flags flags') ds

-- | Expand a single declaration, returning (declarations, derive flags)
expandDecl :: Decl -> Either DeriveError ([Decl], DeriveFlags)
expandDecl decl =
  case decl of
    DeclTypeAlias anns name vars body -> do
      (generated, flags) <- processAliasAnnotations anns name vars body
      pure (decl : generated, flags)
    DeclTypeAnn anns name vars ctors -> do
      (generated, flags) <- processAdtAnnotations anns name vars ctors
      pure (decl : generated, flags)
    _ ->
      pure ([decl], emptyFlags)

processAliasAnnotations :: [Annotation] -> Text -> [Text] -> Type -> Either DeriveError ([Decl], DeriveFlags)
processAliasAnnotations anns name vars body = do
  results <- mapM (processAliasAnnotation name vars body) anns
  let (declLists, flags) = unzip results
  pure (concat declLists, foldr combineFlags emptyFlags flags)

processAdtAnnotations :: [Annotation] -> Text -> [Text] -> [TypeCtor] -> Either DeriveError ([Decl], DeriveFlags)
processAdtAnnotations anns name vars ctors = do
  results <- mapM (processAdtAnnotation name vars ctors) anns
  let (declLists, flags) = unzip results
  pure (concat declLists, foldr combineFlags emptyFlags flags)

-- | Process a single annotation on a type alias.
processAliasAnnotation :: Text -> [Text] -> Type -> Annotation -> Either DeriveError ([Decl], DeriveFlags)
processAliasAnnotation typeName vars typeBody ann =
  case annName ann of
    "derive" ->
      processDeriveAnnotationAlias typeName vars typeBody (annArgs ann)
    _ ->
      pure ([], emptyFlags)

-- | Process a single annotation on an ADT.
processAdtAnnotation :: Text -> [Text] -> [TypeCtor] -> Annotation -> Either DeriveError ([Decl], DeriveFlags)
processAdtAnnotation typeName vars ctors ann =
  case annName ann of
    "derive" ->
      processDeriveAnnotationAdt typeName vars ctors (annArgs ann)
    _ ->
      pure ([], emptyFlags)

-- | Process @derive(...) annotation
processDeriveAnnotationAlias :: Text -> [Text] -> Type -> Maybe Expr -> Either DeriveError ([Decl], DeriveFlags)
processDeriveAnnotationAlias typeName vars typeBody maybeArgs =
  case maybeArgs of
    Nothing ->
      Left (UnknownDerive "derive annotation requires arguments")
    Just args ->
      case extractDeriveKind args of
        Just ("Table", tableName) ->
          case expandTableDerive typeName tableName typeBody of
            Left err -> Left (TableDeriveError err)
            Right decls ->
              pure
                ( decls
                , emptyFlags { dfHasTableDerive = True }
                )
        Just ("Json", _) ->
          case generateJsonForAliasRecord typeName vars typeBody of
            Left err -> Left (JsonDeriveError err)
            Right decls ->
              pure
                ( decls
                , emptyFlags { dfHasJsonDerive = True }
                )
        Just (kind, _) ->
          Left (UnknownDerive kind)
        Nothing ->
          Left (UnknownDerive "invalid derive arguments")

processDeriveAnnotationAdt :: Text -> [Text] -> [TypeCtor] -> Maybe Expr -> Either DeriveError ([Decl], DeriveFlags)
processDeriveAnnotationAdt typeName vars ctors maybeArgs =
  case maybeArgs of
    Nothing ->
      Left (UnknownDerive "derive annotation requires arguments")
    Just args ->
      case extractDeriveKind args of
        Just ("Json", _) ->
          case generateJsonForAdt typeName vars ctors of
            Left err -> Left (JsonDeriveError err)
            Right decls ->
              pure
                ( decls
                , emptyFlags { dfHasJsonDerive = True }
                )
        Just (kind, _) ->
          Left (UnknownDerive kind)
        Nothing ->
          Left (UnknownDerive "invalid derive arguments")

-- | Extract the derive kind and first argument from an expression
-- @derive(Table "users") -> Just ("Table", "users")
extractDeriveKind :: Expr -> Maybe (Text, Text)
extractDeriveKind expr =
  case expr of
    App lKind lArg
      | Var kind <- unLoc lKind
      , StringLit arg <- unLoc lArg ->
          Just (kind, arg)
    Var kind ->
      Just (kind, kind)  -- For derives without string arg
    _ ->
      Nothing
