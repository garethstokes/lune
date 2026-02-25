-- | Table derive implementation
--
-- Generates table refs, field refs, decoder, and CRUD helpers from
-- @derive(Table "tablename") annotations on type aliases.
module Lune.Derive.Table
  ( expandTableDerive
  , TableError(..)
  ) where

import Data.Char (toLower, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Lune.Syntax

-- | Errors specific to Table derive
data TableError
  = NotRecordType Text  -- ^ Type alias body is not a record
  | NoPrimaryKey Text   -- ^ No @primaryKey annotation found
  | MultiplePrimaryKeys Text  -- ^ Multiple @primaryKey annotations
  | UnsupportedFieldType Text Text  -- ^ Field name, type name
  deriving (Show)

-- | Expand a @derive(Table "tablename") annotation
-- Returns list of generated declarations
expandTableDerive :: Text -> Text -> Type -> Either TableError [Decl]
expandTableDerive typeName tableName typeBody =
  case typeBody of
    TypeRecord fields -> do
      -- Validate we have exactly one primary key
      let pkFields = filter (hasAnnotation "primaryKey") fields
      case pkFields of
        [] -> Left (NoPrimaryKey typeName)
        [_] -> generateTableCode typeName tableName fields
        _ -> Left (MultiplePrimaryKeys typeName)
    _ ->
      Left (NotRecordType typeName)

-- | Check if a field has a specific annotation
hasAnnotation :: Text -> (Text, Type, [Annotation]) -> Bool
hasAnnotation name (_, _, anns) =
  any (\a -> annName a == name) anns

-- | Find annotation by name on a field
findAnnotation :: Text -> [Annotation] -> Maybe Annotation
findAnnotation name anns =
  case filter (\a -> annName a == name) anns of
    (a:_) -> Just a
    [] -> Nothing

-- | Generate all table-related code
generateTableCode :: Text -> Text -> [(Text, Type, [Annotation])] -> Either TableError [Decl]
generateTableCode typeName tableName fields = do
  let tableDecls = generateTableRef tableName
      fieldDecls = concatMap (generateFieldRef tableName) fields
      decoderDecls = generateDecoder typeName fields
      crudDecls = generateCrudHelpers typeName tableName fields
  pure (tableDecls ++ fieldDecls ++ decoderDecls ++ crudDecls)

-- | Generate table reference
-- users : Table
-- users = table "users"
generateTableRef :: Text -> [Decl]
generateTableRef tableName =
  [ DeclTypeSig tableName (QualType [] (TypeCon "Table"))
  , DeclValue tableName []
      (noLoc (App (noLoc (Var "table")) (noLoc (StringLit tableName))))
  ]

-- | Generate field reference
-- users_id : Field Int
-- users_id = field users "id"
generateFieldRef :: Text -> (Text, Type, [Annotation]) -> [Decl]
generateFieldRef tableName (fieldName, fieldType, _anns) =
  let refName = tableName <> "_" <> fieldName
      fieldTypeWrapped = TypeApp (TypeCon "Field") fieldType
  in
  [ DeclTypeSig refName (QualType [] fieldTypeWrapped)
  , DeclValue refName []
      (noLoc (App (noLoc (App (noLoc (Var "field")) (noLoc (Var tableName)))) (noLoc (StringLit fieldName))))
  ]

-- | Generate decoder
-- userDecoder : Decoder User
-- userDecoder = map3 (\id name email -> { id = id, name = name, email = email })
--                 (index 0 int) (index 1 string) (index 2 (nullable string))
generateDecoder :: Text -> [(Text, Type, [Annotation])] -> [Decl]
generateDecoder typeName fields =
  let decoderName = lowerFirst typeName <> "Decoder"
      numFields = length fields
      mapN = "map" <> T.pack (show numFields)

      -- Build the constructor lambda
      fieldNames = map (\(n, _, _) -> n) fields
      lambdaParams = map (noLoc . PVar) fieldNames
      recordFields = map (\n -> (n, noLoc (Var n))) fieldNames
      recordExpr = RecordLiteral recordFields
      constructorLambda = Lam lambdaParams (noLoc recordExpr)

      -- Build the index decoders
      indexDecoders = zipWith buildIndexDecoder [0..] fields

      -- Apply map to constructor and all decoders
      fullExpr = foldl (\acc e -> App (noLoc acc) (noLoc e)) (App (noLoc (Var mapN)) (noLoc constructorLambda)) indexDecoders
  in
  [ DeclTypeSig decoderName (QualType [] (TypeApp (TypeCon "Decoder") (TypeCon typeName)))
  , DeclValue decoderName [] (noLoc fullExpr)
  ]

-- | Build an index decoder for a field
-- (index 0 int) or (index 1 string) or (index 2 (nullable string))
buildIndexDecoder :: Int -> (Text, Type, [Annotation]) -> Expr
buildIndexDecoder idx (_fieldName, fieldType, _anns) =
  let indexExpr = App (noLoc (App (noLoc (Var "index")) (noLoc (IntLit (fromIntegral idx))))) (noLoc (typeToDecoder fieldType))
  in indexExpr

-- | Convert a Lune type to its decoder
typeToDecoder :: Type -> Expr
typeToDecoder ty =
  case ty of
    TypeCon "Int" -> Var "int"
    TypeCon "Float" -> Var "float"
    TypeCon "String" -> Var "string"
    TypeCon "Bool" -> Var "bool"
    TypeCon "Timestamp" -> Var "timestamp"
    TypeApp (TypeCon "Maybe") inner ->
      App (noLoc (Var "nullable")) (noLoc (typeToDecoder inner))
    _ ->
      -- Fallback - generate a placeholder that will error at typecheck
      Var "unsupportedDecoder"

-- | Generate CRUD helpers
generateCrudHelpers :: Text -> Text -> [(Text, Type, [Annotation])] -> [Decl]
generateCrudHelpers typeName tableName fields =
  let singularName = lowerFirst typeName
      pkFields = filter (hasAnnotation "primaryKey") fields
  in case pkFields of
    [] -> []  -- Should not happen as we validated earlier
    (pkField:_) ->
      let (pkName, pkType, _pkAnns) = pkField
          isSerial = hasAnnotation "serial" pkField
      in
      generateFindById singularName typeName tableName pkType ++
      generateFindAll singularName typeName tableName ++
      generateInsert singularName typeName tableName fields pkName isSerial ++
      generateUpdate singularName typeName tableName pkType ++
      generateDelete singularName typeName tableName pkType

-- | Generate findUserById : Int -> Query User
-- Note: Returns Query User; limit 1 is applied, executor returns Maybe User
generateFindById :: Text -> Text -> Text -> Type -> [Decl]
generateFindById singularName typeName tableName pkType =
  let fnName = "find" <> capitalize typeName <> "ById"
      returnType = TypeApp (TypeCon "Query") (TypeCon typeName)
      decoderName = singularName <> "Decoder"
      -- findUserById id = limit 1 (where_ (eq users_id (DbInt id)) (select users userDecoder))
      selectExpr = App (noLoc (App (noLoc (Var "select")) (noLoc (Var tableName)))) (noLoc (Var decoderName))
      condExpr = App (noLoc (App (noLoc (Var "eq")) (noLoc (Var (tableName <> "_id")))))
                   (noLoc (App (noLoc (typeToDbValue pkType)) (noLoc (Var "id"))))
      whereExpr = App (noLoc (App (noLoc (Var "where_")) (noLoc condExpr))) (noLoc selectExpr)
      body = App (noLoc (App (noLoc (Var "limit")) (noLoc (IntLit 1)))) (noLoc whereExpr)
  in
  [ DeclTypeSig fnName (QualType [] (TypeArrow pkType returnType))
  , DeclValue fnName [noLoc (PVar "id")] (noLoc body)
  ]

-- | Generate findAllUsers : Query User
-- Note: Query a means "query where each row decodes to a"
-- The caller/executor maps over rows to get List a
generateFindAll :: Text -> Text -> Text -> [Decl]
generateFindAll singularName typeName tableName =
  let fnName = "findAll" <> capitalize typeName <> "s"
      returnType = TypeApp (TypeCon "Query") (TypeCon typeName)
      decoderName = singularName <> "Decoder"
      body = App (noLoc (App (noLoc (Var "select")) (noLoc (Var tableName)))) (noLoc (Var decoderName))
  in
  [ DeclTypeSig fnName (QualType [] returnType)
  , DeclValue fnName [] (noLoc body)
  ]

-- | Generate insertUser : { ... } -> Query User
-- Omits all serial fields (both primary key and timestamps like createdAt)
generateInsert :: Text -> Text -> Text -> [(Text, Type, [Annotation])] -> Text -> Bool -> [Decl]
generateInsert singularName typeName tableName fields _pkName _isSerial =
  let fnName = "insert" <> capitalize typeName
      -- Filter out all serial fields from input type
      inputFields = filter (not . hasAnnotation "serial") fields
      inputType = TypeRecord [(n, t, []) | (n, t, _) <- inputFields]
      returnType = TypeApp (TypeCon "Query") (TypeCon typeName)
      decoderName = singularName <> "Decoder"

      -- Build: returning (values [...] (insert users userDecoder))
      assignments = map (\(n, t, _) ->
        App (noLoc (App (noLoc (Var "set")) (noLoc (Var (tableName <> "_" <> n)))))
          (noLoc (App (noLoc (typeToDbValue t)) (noLoc (FieldAccess (noLoc (Var "input")) n))))) inputFields
      valuesExpr = foldr (\a acc -> App (noLoc (App (noLoc (Var "Cons")) (noLoc a))) (noLoc acc)) (Var "Nil") assignments
      insertExpr = App (noLoc (App (noLoc (Var "insert")) (noLoc (Var tableName)))) (noLoc (Var decoderName))
      withValues = App (noLoc (App (noLoc (Var "values")) (noLoc valuesExpr))) (noLoc insertExpr)
      body = App (noLoc (Var "returning")) (noLoc withValues)
  in
  [ DeclTypeSig fnName (QualType [] (TypeArrow inputType returnType))
  , DeclValue fnName [noLoc (PVar "input")] (noLoc body)
  ]

-- | Generate updateUser : Int -> List Assignment -> Query User
generateUpdate :: Text -> Text -> Text -> Type -> [Decl]
generateUpdate singularName typeName tableName pkType =
  let fnName = "update" <> capitalize typeName
      returnType = TypeApp (TypeCon "Query") (TypeCon typeName)
      decoderName = singularName <> "Decoder"
      -- updateUser id assigns = returning (where_ (eq users_id (int id)) (values assigns (update users userDecoder)))
      updateExpr = App (noLoc (App (noLoc (Var "update")) (noLoc (Var tableName)))) (noLoc (Var decoderName))
      withValues = App (noLoc (App (noLoc (Var "values")) (noLoc (Var "assigns")))) (noLoc updateExpr)
      condExpr = App (noLoc (App (noLoc (Var "eq")) (noLoc (Var (tableName <> "_id")))))
                   (noLoc (App (noLoc (typeToDbValue pkType)) (noLoc (Var "id"))))
      withWhere = App (noLoc (App (noLoc (Var "where_")) (noLoc condExpr))) (noLoc withValues)
      body = App (noLoc (Var "returning")) (noLoc withWhere)
  in
  [ DeclTypeSig fnName (QualType [] (TypeArrow pkType (TypeArrow (TypeApp (TypeCon "List") (TypeCon "Assignment")) returnType)))
  , DeclValue fnName [noLoc (PVar "id"), noLoc (PVar "assigns")] (noLoc body)
  ]

-- | Generate deleteUser : Int -> Query Unit
generateDelete :: Text -> Text -> Text -> Type -> [Decl]
generateDelete _singularName typeName tableName pkType =
  let fnName = "delete" <> capitalize typeName
      returnType = TypeApp (TypeCon "Query") (TypeCon "Unit")
      -- deleteUser id = where_ (eq users_id (int id)) (delete users)
      deleteExpr = App (noLoc (Var "delete")) (noLoc (Var tableName))
      condExpr = App (noLoc (App (noLoc (Var "eq")) (noLoc (Var (tableName <> "_id")))))
                   (noLoc (App (noLoc (typeToDbValue pkType)) (noLoc (Var "id"))))
      body = App (noLoc (App (noLoc (Var "where_")) (noLoc condExpr))) (noLoc deleteExpr)
  in
  [ DeclTypeSig fnName (QualType [] (TypeArrow pkType returnType))
  , DeclValue fnName [noLoc (PVar "id")] (noLoc body)
  ]

-- | Convert type to DbValue constructor
-- For Maybe types, we need special handling in the caller
typeToDbValue :: Type -> Expr
typeToDbValue ty =
  case ty of
    TypeCon "Int" -> Var "DbInt"
    TypeCon "Float" -> Var "DbFloat"
    TypeCon "String" -> Var "DbString"
    TypeCon "Bool" -> Var "DbBool"
    TypeCon "Timestamp" ->
      -- Timestamp wraps Int, need to extract micros: \ts -> case ts of Timestamp m -> DbTimestamp m
      Lam [noLoc (PVar "ts")]
        (noLoc (Case (noLoc (Var "ts"))
          [ noLoc (Alt (noLoc (PCon "Timestamp" [noLoc (PVar "m")])) (noLoc (App (noLoc (Var "DbTimestamp")) (noLoc (Var "m")))))
          ]))
    TypeApp (TypeCon "Maybe") inner ->
      -- For Maybe, we need to handle it specially in the caller
      -- Return a lambda that converts Maybe a to DbValue
      Lam [noLoc (PVar "maybeVal")]
        (noLoc (Case (noLoc (Var "maybeVal"))
          [ noLoc (Alt (noLoc (PCon "Nothing" [])) (noLoc (Var "DbNull")))
          , noLoc (Alt (noLoc (PCon "Just" [noLoc (PVar "v")])) (noLoc (App (noLoc (typeToDbValue inner)) (noLoc (Var "v")))))
          ]))
    _ -> Var "DbNull"  -- Fallback for unknown types

-- | Lower the first character of a text
lowerFirst :: Text -> Text
lowerFirst t = case T.uncons t of
  Just (c, rest) -> T.cons (toLower c) rest
  Nothing -> t

-- | Capitalize the first character of a text
capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> t
