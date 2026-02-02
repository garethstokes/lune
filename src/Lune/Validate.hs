module Lune.Validate
  ( ValidateError (..)
  , validateModule
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Lune.Syntax

data ValidateError
  = TypeConstructorArityMismatch Text Int Int
  | AppliedArrowType
  | AppliedRecordType
  | DuplicateRecordTypeFields [Text]
  deriving (Eq)

instance Show ValidateError where
  show err =
    case err of
      TypeConstructorArityMismatch name expected got ->
        T.unpack name
          <> " expects "
          <> show expected
          <> " type arguments"
          <> (if expected == got then "" else " (got " <> show got <> ")")
      AppliedArrowType ->
        "Cannot apply a function type"
      AppliedRecordType ->
        "Cannot apply a record type"
      DuplicateRecordTypeFields fields ->
        "Record type has duplicate field(s): " <> T.unpack (T.intercalate ", " fields)

validateModule :: Module -> Either ValidateError ()
validateModule m = do
  let env = builtinArities <> declaredArities (modDecls m)
  mapM_ (validateDecl env) (modDecls m)

type ArityEnv = [(Text, Int)]

builtinArities :: ArityEnv
builtinArities =
  [ ("Int", 0)
  , ("Bool", 0)
  , ("String", 0)
  , ("Unit", 0)
  , ("Char", 0)
  , ("List", 1)
  , ("Maybe", 1)
  , ("Result", 2)
  , ("IO", 1)
  , ("Validation", 2)
  , ("Atomic", 1)
  , ("Shared", 1)
  , ("Task", 1)
  ]

declaredArities :: [Decl] -> ArityEnv
declaredArities =
  foldr go []
  where
    go decl acc =
      case decl of
        DeclType name vars _ ->
          (name, length vars) : acc
        DeclTypeAlias name vars _ ->
          (name, length vars) : acc
        DeclNewtype name vars _ _ ->
          (name, length vars) : acc
        _ ->
          acc

validateDecl :: ArityEnv -> Decl -> Either ValidateError ()
validateDecl env decl =
  case decl of
    DeclTypeSig _ (QualType constraints ty) -> do
      mapM_ (validateConstraint env) constraints
      validateType env ty
    DeclType _ _ ctors ->
      mapM_ (validateTypeCtor env) ctors
    DeclTypeAlias _ _ body ->
      validateType env body
    DeclNewtype _ _ _ ctorTy ->
      validateType env ctorTy
    DeclClass _ _ supers methods -> do
      mapM_ (validateConstraint env) supers
      mapM_ (validateClassMethodSig env) methods
    DeclValue {} ->
      Right ()
    DeclInstance {} ->
      Right ()

validateConstraint :: ArityEnv -> Constraint -> Either ValidateError ()
validateConstraint env constraint =
  mapM_ (validateType env) (constraintArgs constraint)

validateClassMethodSig :: ArityEnv -> ClassMethodSig -> Either ValidateError ()
validateClassMethodSig env (ClassMethodSig _ (QualType constraints ty)) = do
  mapM_ (validateConstraint env) constraints
  validateType env ty

validateTypeCtor :: ArityEnv -> TypeCtor -> Either ValidateError ()
validateTypeCtor env (TypeCtor _ args) =
  mapM_ (validateType env) args

validateType :: ArityEnv -> Type -> Either ValidateError ()
validateType env ty =
  case ty of
    TypeArrow dom cod -> do
      validateType env dom
      validateType env cod

    TypeRecord fields -> do
      validateRecordFields env fields

    TypeApp {} ->
      validateAppliedType env ty

    TypeCon name ->
      validateTypeConstructorArity env name 0

    TypeVar _ ->
      Right ()

validateAppliedType :: ArityEnv -> Type -> Either ValidateError ()
validateAppliedType env ty = do
  let (headTy, args) = unapplyTypeApps ty
  case headTy of
    TypeArrow {} ->
      Left AppliedArrowType
    TypeRecord {} ->
      Left AppliedRecordType
    TypeCon name ->
      validateTypeConstructorArity env name (length args)
    _ ->
      Right ()
  mapM_ (validateType env) args

validateTypeConstructorArity :: ArityEnv -> Text -> Int -> Either ValidateError ()
validateTypeConstructorArity env name got =
  case lookup name env of
    Nothing ->
      Right ()
    Just expected ->
      if expected == got
        then Right ()
        else Left (TypeConstructorArityMismatch name expected got)

validateRecordFields :: ArityEnv -> [(Text, Type)] -> Either ValidateError ()
validateRecordFields env fields = do
  let names = map fst fields
  case duplicates names of
    [] -> Right ()
    dups -> Left (DuplicateRecordTypeFields dups)
  mapM_ (validateType env . snd) fields

duplicates :: Eq a => [a] -> [a]
duplicates =
  go [] []
  where
    go _ dupes [] =
      reverse dupes
    go seen dupes (x : xs)
      | x `elem` seen =
          if x `elem` dupes
            then go seen dupes xs
            else go seen (x : dupes) xs
      | otherwise =
          go (x : seen) dupes xs

unapplyTypeApps :: Type -> (Type, [Type])
unapplyTypeApps =
  go []
  where
    go args t =
      case t of
        TypeApp f x ->
          go (x : args) f
        _ ->
          (t, args)
