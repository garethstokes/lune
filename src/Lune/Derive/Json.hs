-- | Json derive implementation
--
-- Generates JSON encoders and decoders from @derive(Json) annotations on:
--   - record type aliases (product types)
--   - ADTs / sum types
--
-- The JSON encoding format for ADTs is:
--   { "tag": "<CtorName>", "fields": [ ...ctor args... ] }
module Lune.Derive.Json
  ( JsonError(..)
  , generateJsonForAliasRecord
  , generateJsonForAdt
  ) where

import Control.Monad (zipWithM)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Lune.Syntax

data JsonError
  = JsonOnlyRecordAliasesAndAdts
  | JsonTypeHasParameters Text
  | JsonUnsupportedType Type

instance Show JsonError where
  show err =
    case err of
      JsonOnlyRecordAliasesAndAdts ->
        "@derive(Json) only supports record aliases and ADTs"
      JsonTypeHasParameters typeName ->
        "@derive(Json) does not support type parameters on " <> T.unpack typeName
      JsonUnsupportedType ty ->
        "@derive(Json) encountered an unsupported type: " <> show ty

decodeAlias :: Text
decodeAlias = "D"

encodeAlias :: Text
encodeAlias = "E"

-- | Generate decoder + encoder for a record type alias.
generateJsonForAliasRecord :: Text -> [Text] -> Type -> Either JsonError [Decl]
generateJsonForAliasRecord typeName vars typeBody = do
  if null vars then pure () else Left (JsonTypeHasParameters typeName)
  case typeBody of
    TypeRecord fields ->
      generateRecordAlias typeName fields
    _ ->
      Left JsonOnlyRecordAliasesAndAdts

-- | Generate decoder + encoder for an ADT.
generateJsonForAdt :: Text -> [Text] -> [TypeCtor] -> Either JsonError [Decl]
generateJsonForAdt typeName vars ctors = do
  if null vars then pure () else Left (JsonTypeHasParameters typeName)
  decoderExpr <- adtDecoderExpr ctors
  encoderExpr <- adtEncoderExpr (encoderParamName typeName) ctors

  let decoderName = lowerFirst typeName <> "Decoder"
      encoderName = lowerFirst typeName <> "Encoder"

      decoderSigTy =
        TypeApp (TypeCon (decodeAlias <> ".Decoder")) (TypeCon typeName)
      encoderSigTy =
        TypeArrow (TypeCon typeName) (TypeCon (encodeAlias <> ".Value"))

  pure
    [ DeclTypeSig decoderName (QualType [] decoderSigTy)
    , DeclValue decoderName [] (noLoc decoderExpr)
    , DeclTypeSig encoderName (QualType [] encoderSigTy)
    , DeclValue encoderName [noLoc (PVar (encoderParamName typeName))] (noLoc encoderExpr)
    ]

-- ===== Codegen: records =====

generateRecordAlias :: Text -> [(Text, Type, [Annotation])] -> Either JsonError [Decl]
generateRecordAlias typeName fields = do
  decoderExpr <- recordDecoderExpr fields
  encoderExpr <- recordEncodeValueExpr (Var (encoderParamName typeName)) fields

  let decoderName = lowerFirst typeName <> "Decoder"
      encoderName = lowerFirst typeName <> "Encoder"

      decoderSigTy =
        TypeApp (TypeCon (decodeAlias <> ".Decoder")) (TypeCon typeName)
      encoderSigTy =
        TypeArrow (TypeCon typeName) (TypeCon (encodeAlias <> ".Value"))

  pure
    [ DeclTypeSig decoderName (QualType [] decoderSigTy)
    , DeclValue decoderName [] (noLoc decoderExpr)
    , DeclTypeSig encoderName (QualType [] encoderSigTy)
    , DeclValue encoderName [noLoc (PVar (encoderParamName typeName))] (noLoc encoderExpr)
    ]

recordDecoderExpr :: [(Text, Type, [Annotation])] -> Either JsonError Expr
recordDecoderExpr fields = do
  let fieldNames = map (\(n, _, _) -> n) fields
      recordExpr = RecordLiteral [(n, noLoc (Var n)) | n <- fieldNames]
      succeedRecord = App (noLoc (d "succeed")) (noLoc recordExpr)

  fieldDecoders <- mapM recordFieldDecoder fields

  case fieldNames of
    [] ->
      pure succeedRecord
    _ ->
      case dMapN (length fieldNames) of
        Just mapFn ->
          let builder = Lam (map (noLoc . PVar) fieldNames) (noLoc recordExpr)
              applied = foldl (\acc dec -> App (noLoc acc) (noLoc dec)) (App (noLoc mapFn) (noLoc builder)) fieldDecoders
           in pure applied
        Nothing ->
          -- Fallback for >5 fields: chain decoders with andThen.
          pure (andThenChain (zip fieldNames fieldDecoders) succeedRecord)

recordFieldDecoder :: (Text, Type, [Annotation]) -> Either JsonError Expr
recordFieldDecoder (fieldName, fieldTy, _anns) = do
  dec <- decoderExprOfType fieldTy
  pure (App (noLoc (App (noLoc (d "field")) (noLoc (StringLit fieldName)))) (noLoc dec))

recordEncodeValueExpr :: Expr -> [(Text, Type, [Annotation])] -> Either JsonError Expr
recordEncodeValueExpr recordValue fields = do
  pairs <-
    mapM
      ( \(fieldName, fieldTy, _anns) -> do
          value <- encodeValueOfType fieldTy (FieldAccess (noLoc recordValue) fieldName)
          pure (RecordLiteral [("key", noLoc (StringLit fieldName)), ("value", noLoc value)])
      )
      fields

  pure (App (noLoc (e "object")) (noLoc (listExpr pairs)))

-- ===== Codegen: ADTs =====

adtEncoderExpr :: Text -> [TypeCtor] -> Either JsonError Expr
adtEncoderExpr valueName ctors = do
  alts <- mapM adtEncoderAlt ctors
  pure (Case (noLoc (Var valueName)) alts)

adtEncoderAlt :: TypeCtor -> Either JsonError (Located Alt)
adtEncoderAlt (TypeCtor ctorName argTys) = do
  let argNames = ctorArgNames (length argTys)
      pat = PCon ctorName (map (noLoc . PVar) argNames)

  encodedArgs <- zipWithM encodeValueOfType argTys (map Var argNames)
  let fieldsJson =
        App
          (noLoc (App (noLoc (e "list")) (noLoc identityLam)))
          (noLoc (listExpr encodedArgs))
      obj =
        App
          (noLoc (e "object"))
          ( noLoc (listExpr
              [ RecordLiteral [("key", noLoc (StringLit "tag")), ("value", noLoc (App (noLoc (e "string")) (noLoc (StringLit ctorName))))]
              , RecordLiteral [("key", noLoc (StringLit "fields")), ("value", noLoc fieldsJson)]
              ])
          )

  pure (noLoc (Alt (noLoc pat) (noLoc obj)))

adtDecoderExpr :: [TypeCtor] -> Either JsonError Expr
adtDecoderExpr ctors = do
  alts <- mapM adtDecoderAlt ctors
  let defaultAlt =
        noLoc (Alt
          (noLoc (PVar "other"))
          (noLoc (App (noLoc (d "fail")) (noLoc (App (noLoc (App (noLoc (Var "prim_appendString")) (noLoc (StringLit "Unknown tag: ")))) (noLoc (Var "other")))))))
      tagCase =
        Lam [noLoc (PVar "tag")] (noLoc (Case (noLoc (Var "tag")) (alts <> [defaultAlt])))
      tagDecoder =
        App (noLoc (App (noLoc (d "field")) (noLoc (StringLit "tag")))) (noLoc (d "string"))
  pure (App (noLoc (App (noLoc (d "andThen")) (noLoc tagCase))) (noLoc tagDecoder))

adtDecoderAlt :: TypeCtor -> Either JsonError (Located Alt)
adtDecoderAlt (TypeCtor ctorName argTys) = do
  decoder <- ctorDecoderExpr ctorName argTys
  pure (noLoc (Alt (noLoc (PString ctorName)) (noLoc decoder)))

ctorDecoderExpr :: Text -> [Type] -> Either JsonError Expr
ctorDecoderExpr ctorName argTys =
  case argTys of
    [] ->
      pure (App (noLoc (d "succeed")) (noLoc (Var ctorName)))
    _ -> do
      argDecoders <- zipWithM ctorArgDecoder [0 ..] argTys
      let ctorVar = Var ctorName
      case dMapN (length argTys) of
        Just mapFn ->
          pure (foldl (\acc dec -> App (noLoc acc) (noLoc dec)) (App (noLoc mapFn) (noLoc ctorVar)) argDecoders)
        Nothing -> do
          let argNames = ctorArgNames (length argTys)
              ctorApp = foldl (\acc name -> App (noLoc acc) (noLoc (Var name))) ctorVar argNames
              succeedCtor = App (noLoc (d "succeed")) (noLoc ctorApp)
          pure (andThenChain (zip argNames argDecoders) succeedCtor)

ctorArgDecoder :: Int -> Type -> Either JsonError Expr
ctorArgDecoder idx ty = do
  dec <- decoderExprOfType ty
  let idxExpr = IntLit (fromIntegral idx)
      fieldsDecoder = App (noLoc (App (noLoc (d "index")) (noLoc idxExpr))) (noLoc dec)
  pure (App (noLoc (App (noLoc (d "field")) (noLoc (StringLit "fields")))) (noLoc fieldsDecoder))

-- ===== Type mapping =====

decoderExprOfType :: Type -> Either JsonError Expr
decoderExprOfType ty =
  case ty of
    TypeCon "Int" -> pure (d "int")
    TypeCon "Float" -> pure (d "float")
    TypeCon "String" -> pure (d "string")
    TypeCon "Bool" -> pure (d "bool")
    TypeApp (TypeCon "List") inner ->
      App (noLoc (d "list")) . noLoc <$> decoderExprOfType inner
    TypeApp (TypeCon "Maybe") inner ->
      App (noLoc (d "maybe")) . noLoc <$> decoderExprOfType inner
    TypeRecord fields ->
      recordDecoderExpr fields
    TypeCon name ->
      pure (Var (lowerFirst name <> "Decoder"))
    _ ->
      Left (JsonUnsupportedType ty)

encoderFnExprOfType :: Type -> Either JsonError Expr
encoderFnExprOfType ty =
  case ty of
    TypeCon "Int" -> pure (e "int")
    TypeCon "Float" -> pure (e "float")
    TypeCon "String" -> pure (e "string")
    TypeCon "Bool" -> pure (e "bool")
    TypeApp (TypeCon "List") inner ->
      App (noLoc (e "list")) . noLoc <$> encoderFnExprOfType inner
    TypeApp (TypeCon "Maybe") inner ->
      maybeEncoderExpr inner
    TypeRecord fields ->
      Lam [noLoc (PVar "r")] . noLoc <$> recordEncodeValueExpr (Var "r") fields
    TypeCon name ->
      pure (Var (lowerFirst name <> "Encoder"))
    _ ->
      Left (JsonUnsupportedType ty)

encodeValueOfType :: Type -> Expr -> Either JsonError Expr
encodeValueOfType ty value =
  case ty of
    TypeCon "Int" -> pure (App (noLoc (e "int")) (noLoc value))
    TypeCon "Float" -> pure (App (noLoc (e "float")) (noLoc value))
    TypeCon "String" -> pure (App (noLoc (e "string")) (noLoc value))
    TypeCon "Bool" -> pure (App (noLoc (e "bool")) (noLoc value))
    TypeApp (TypeCon "List") inner -> do
      innerEnc <- encoderFnExprOfType inner
      pure (App (noLoc (App (noLoc (e "list")) (noLoc innerEnc))) (noLoc value))
    TypeApp (TypeCon "Maybe") inner -> do
      enc <- maybeEncoderExpr inner
      pure (App (noLoc enc) (noLoc value))
    TypeRecord fields ->
      recordEncodeValueExpr value fields
    TypeCon name ->
      pure (App (noLoc (Var (lowerFirst name <> "Encoder"))) (noLoc value))
    _ ->
      Left (JsonUnsupportedType ty)

maybeEncoderExpr :: Type -> Either JsonError Expr
maybeEncoderExpr innerTy =
  do
    innerEnc <- encoderFnExprOfType innerTy
    pure $
      Lam
        [noLoc (PVar "v")]
        ( noLoc (Case
            (noLoc (Var "v"))
            [ noLoc (Alt (noLoc (PCon "Nothing" [])) (noLoc (e "null")))
            , noLoc (Alt (noLoc (PCon "Just" [noLoc (PVar "x")])) (noLoc (App (noLoc innerEnc) (noLoc (Var "x")))))
            ])
        )

-- ===== Small expression helpers =====

d :: Text -> Expr
d name =
  FieldAccess (noLoc (Var decodeAlias)) name

e :: Text -> Expr
e name =
  FieldAccess (noLoc (Var encodeAlias)) name

identityLam :: Expr
identityLam =
  Lam [noLoc (PVar "x")] (noLoc (Var "x"))

listExpr :: [Expr] -> Expr
listExpr =
  foldr (\x xs -> App (noLoc (App (noLoc (Var "Cons")) (noLoc x))) (noLoc xs)) (Var "Nil")

andThenChain :: [(Text, Expr)] -> Expr -> Expr
andThenChain bindings terminal =
  foldr
    (\(name, dec) acc -> App (noLoc (App (noLoc (d "andThen")) (noLoc (Lam [noLoc (PVar name)] (noLoc acc))))) (noLoc dec))
    terminal
    bindings

dMapN :: Int -> Maybe Expr
dMapN n =
  case n of
    1 -> Just (d "map")
    2 -> Just (d "map2")
    3 -> Just (d "map3")
    4 -> Just (d "map4")
    5 -> Just (d "map5")
    _ -> Nothing

encoderParamName :: Text -> Text
encoderParamName typeName =
  let lowered = lowerFirst typeName
   in case T.uncons lowered of
        Just (c, _) -> T.singleton c
        Nothing -> "x"

ctorArgNames :: Int -> [Text]
ctorArgNames n =
  [ "a" <> T.pack (show i)
  | i <- [0 .. (n - 1)]
  ]

lowerFirst :: Text -> Text
lowerFirst t =
  case T.uncons t of
    Just (c, rest) ->
      T.cons (toLower c) rest
    Nothing ->
      t
