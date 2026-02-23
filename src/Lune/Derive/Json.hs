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
    , DeclValue decoderName [] decoderExpr
    , DeclTypeSig encoderName (QualType [] encoderSigTy)
    , DeclValue encoderName [PVar (encoderParamName typeName)] encoderExpr
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
    , DeclValue decoderName [] decoderExpr
    , DeclTypeSig encoderName (QualType [] encoderSigTy)
    , DeclValue encoderName [PVar (encoderParamName typeName)] encoderExpr
    ]

recordDecoderExpr :: [(Text, Type, [Annotation])] -> Either JsonError Expr
recordDecoderExpr fields = do
  let fieldNames = map (\(n, _, _) -> n) fields
      recordExpr = RecordLiteral [(n, Var n) | n <- fieldNames]
      succeedRecord = App (d "succeed") recordExpr

  fieldDecoders <- mapM recordFieldDecoder fields

  case fieldNames of
    [] ->
      pure succeedRecord
    _ ->
      case dMapN (length fieldNames) of
        Just mapFn ->
          let builder = Lam (map PVar fieldNames) recordExpr
              applied = foldl App (App mapFn builder) fieldDecoders
           in pure applied
        Nothing ->
          -- Fallback for >5 fields: chain decoders with andThen.
          pure (andThenChain (zip fieldNames fieldDecoders) succeedRecord)

recordFieldDecoder :: (Text, Type, [Annotation]) -> Either JsonError Expr
recordFieldDecoder (fieldName, fieldTy, _anns) = do
  dec <- decoderExprOfType fieldTy
  pure (App (App (d "field") (StringLit fieldName)) dec)

recordEncodeValueExpr :: Expr -> [(Text, Type, [Annotation])] -> Either JsonError Expr
recordEncodeValueExpr recordValue fields = do
  pairs <-
    mapM
      ( \(fieldName, fieldTy, _anns) -> do
          value <- encodeValueOfType fieldTy (FieldAccess recordValue fieldName)
          pure (RecordLiteral [("key", StringLit fieldName), ("value", value)])
      )
      fields

  pure (App (e "object") (listExpr pairs))

-- ===== Codegen: ADTs =====

adtEncoderExpr :: Text -> [TypeCtor] -> Either JsonError Expr
adtEncoderExpr valueName ctors = do
  alts <- mapM adtEncoderAlt ctors
  pure (Case (Var valueName) alts)

adtEncoderAlt :: TypeCtor -> Either JsonError Alt
adtEncoderAlt (TypeCtor ctorName argTys) = do
  let argNames = ctorArgNames (length argTys)
      pat = PCon ctorName (map PVar argNames)

  encodedArgs <- zipWithM encodeValueOfType argTys (map Var argNames)
  let fieldsJson =
        App
          (App (e "list") identityLam)
          (listExpr encodedArgs)
      obj =
        App
          (e "object")
          ( listExpr
              [ RecordLiteral [("key", StringLit "tag"), ("value", App (e "string") (StringLit ctorName))]
              , RecordLiteral [("key", StringLit "fields"), ("value", fieldsJson)]
              ]
          )

  pure (Alt pat obj)

adtDecoderExpr :: [TypeCtor] -> Either JsonError Expr
adtDecoderExpr ctors = do
  alts <- mapM adtDecoderAlt ctors
  let defaultAlt =
        Alt
          (PVar "other")
          (App (d "fail") (App (App (Var "prim_appendString") (StringLit "Unknown tag: ")) (Var "other")))
      tagCase =
        Lam [PVar "tag"] (Case (Var "tag") (alts <> [defaultAlt]))
      tagDecoder =
        App (App (d "field") (StringLit "tag")) (d "string")
  pure (App (App (d "andThen") tagCase) tagDecoder)

adtDecoderAlt :: TypeCtor -> Either JsonError Alt
adtDecoderAlt (TypeCtor ctorName argTys) = do
  decoder <- ctorDecoderExpr ctorName argTys
  pure (Alt (PString ctorName) decoder)

ctorDecoderExpr :: Text -> [Type] -> Either JsonError Expr
ctorDecoderExpr ctorName argTys =
  case argTys of
    [] ->
      pure (App (d "succeed") (Var ctorName))
    _ -> do
      argDecoders <- zipWithM ctorArgDecoder [0 ..] argTys
      let ctorVar = Var ctorName
      case dMapN (length argTys) of
        Just mapFn ->
          pure (foldl App (App mapFn ctorVar) argDecoders)
        Nothing -> do
          let argNames = ctorArgNames (length argTys)
              ctorApp = foldl App ctorVar (map Var argNames)
              succeedCtor = App (d "succeed") ctorApp
          pure (andThenChain (zip argNames argDecoders) succeedCtor)

ctorArgDecoder :: Int -> Type -> Either JsonError Expr
ctorArgDecoder idx ty = do
  dec <- decoderExprOfType ty
  let idxExpr = IntLit (fromIntegral idx)
      fieldsDecoder = App (App (d "index") idxExpr) dec
  pure (App (App (d "field") (StringLit "fields")) fieldsDecoder)

-- ===== Type mapping =====

decoderExprOfType :: Type -> Either JsonError Expr
decoderExprOfType ty =
  case ty of
    TypeCon "Int" -> pure (d "int")
    TypeCon "Float" -> pure (d "float")
    TypeCon "String" -> pure (d "string")
    TypeCon "Bool" -> pure (d "bool")
    TypeApp (TypeCon "List") inner ->
      App (d "list") <$> decoderExprOfType inner
    TypeApp (TypeCon "Maybe") inner ->
      App (d "maybe") <$> decoderExprOfType inner
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
      App (e "list") <$> encoderFnExprOfType inner
    TypeApp (TypeCon "Maybe") inner ->
      maybeEncoderExpr inner
    TypeRecord fields ->
      Lam [PVar "r"] <$> recordEncodeValueExpr (Var "r") fields
    TypeCon name ->
      pure (Var (lowerFirst name <> "Encoder"))
    _ ->
      Left (JsonUnsupportedType ty)

encodeValueOfType :: Type -> Expr -> Either JsonError Expr
encodeValueOfType ty value =
  case ty of
    TypeCon "Int" -> pure (App (e "int") value)
    TypeCon "Float" -> pure (App (e "float") value)
    TypeCon "String" -> pure (App (e "string") value)
    TypeCon "Bool" -> pure (App (e "bool") value)
    TypeApp (TypeCon "List") inner -> do
      innerEnc <- encoderFnExprOfType inner
      pure (App (App (e "list") innerEnc) value)
    TypeApp (TypeCon "Maybe") inner -> do
      enc <- maybeEncoderExpr inner
      pure (App enc value)
    TypeRecord fields ->
      recordEncodeValueExpr value fields
    TypeCon name ->
      pure (App (Var (lowerFirst name <> "Encoder")) value)
    _ ->
      Left (JsonUnsupportedType ty)

maybeEncoderExpr :: Type -> Either JsonError Expr
maybeEncoderExpr innerTy =
  do
    innerEnc <- encoderFnExprOfType innerTy
    pure $
      Lam
        [PVar "v"]
        ( Case
            (Var "v")
            [ Alt (PCon "Nothing" []) (e "null")
            , Alt (PCon "Just" [PVar "x"]) (App innerEnc (Var "x"))
            ]
        )

-- ===== Small expression helpers =====

d :: Text -> Expr
d name =
  FieldAccess (Var decodeAlias) name

e :: Text -> Expr
e name =
  FieldAccess (Var encodeAlias) name

identityLam :: Expr
identityLam =
  Lam [PVar "x"] (Var "x")

listExpr :: [Expr] -> Expr
listExpr =
  foldr (\x xs -> App (App (Var "Cons") x) xs) (Var "Nil")

andThenChain :: [(Text, Expr)] -> Expr -> Expr
andThenChain bindings terminal =
  foldr
    (\(name, dec) acc -> App (App (d "andThen") (Lam [PVar name] acc)) dec)
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
