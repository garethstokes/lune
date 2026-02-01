module Lune.Check
  ( TypecheckError (..)
  , typecheckModule
  , renderScheme
  ) where

import Control.Monad (foldM_)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lune.Desugar (desugarModule)
import Lune.Kind (KindEnv, KindError, builtinClassEnv, kindCheckQualType, kindEnvFromDecls)
import qualified Lune.Syntax as S
import Lune.Infer
import Lune.Type

data TypecheckError
  = InValue Text InferError
  | InTypeSig Text KindError

instance Show TypecheckError where
  show err =
    case err of
      InValue name inferErr ->
        T.unpack name <> ": " <> show inferErr
      InTypeSig name kindErr ->
        T.unpack name <> ": " <> show kindErr

typecheckModule :: S.Module -> Either TypecheckError [(Text, Scheme)]
typecheckModule m = do
  checkSigKinds kindEnv decls
  go env0 [] (valueDecls decls)
  where
    m' = desugarModule m
    decls = S.modDecls m'
    kindEnv = kindEnvFromDecls decls
    aliasEnv = buildAliasEnv decls
    sigEnv = buildSigEnv aliasEnv decls
    ctorEnv = buildCtorEnv aliasEnv decls
    env0 = builtinEnv <> ctorEnv <> sigEnv

    go _ acc [] =
      Right (reverse acc)
    go env acc (decl : rest) =
      case runInferM (checkDecl env sigEnv decl) of
        Left err ->
          Left (InValue (nameOf decl) err)
        Right (name, scheme) ->
          let env' = Map.insert name scheme env
           in go env' ((name, scheme) : acc) rest

    nameOf (n, _, _) = n

checkSigKinds :: KindEnv -> [S.Decl] -> Either TypecheckError ()
checkSigKinds kindEnv =
  foldM_ step ()
  where
    step () decl =
      case decl of
        S.DeclTypeSig name qualTy ->
          case kindCheckQualType kindEnv builtinClassEnv qualTy of
            Left err ->
              Left (InTypeSig name err)
            Right () ->
              Right ()
        _ ->
          Right ()

checkDecl :: TypeEnv -> TypeEnv -> (Text, [S.Pattern], S.Expr) -> InferM (Text, Scheme)
checkDecl env sigEnv (name, args, body) =
  case Map.lookup name sigEnv of
    Just scheme -> do
      checkAgainstScheme env name args body scheme
      pure (name, scheme)
    Nothing -> do
      (s, constraints, ty) <- inferExpr env (S.Lam args body)
      let env' = applySubstEnv s env
          scheme = generalize env' constraints ty
      pure (name, scheme)

checkAgainstScheme :: TypeEnv -> Text -> [S.Pattern] -> S.Expr -> Scheme -> InferM ()
checkAgainstScheme env name args body scheme = do
  skTy <- skolemize scheme
  (argTys, resTy) <- peelFunctionType name (length args) skTy
  argEnv <- bindTopLevelArgs args argTys
  (sBody, _constraints, bodyTy) <- inferExpr (argEnv <> env) body
  _ <- unify (applySubstType sBody bodyTy) (applySubstType sBody resTy)
  pure ()

peelFunctionType :: Text -> Int -> Type -> InferM ([Type], Type)
peelFunctionType _ 0 ty =
  pure ([], ty)
peelFunctionType name n ty =
  case ty of
    TArrow a b -> do
      (args, res) <- peelFunctionType name (n - 1) b
      pure (a : args, res)
    _ ->
      inferFail (ExpectedFunctionType ty)

bindTopLevelArgs :: [S.Pattern] -> [Type] -> InferM TypeEnv
bindTopLevelArgs pats tys =
  foldlM step Map.empty (zip pats tys)
  where
    foldlM f z xs = go z xs
      where
        go acc [] = pure acc
        go acc (y : ys) = f acc y >>= \acc' -> go acc' ys

    step envAcc (pat, ty) =
      case pat of
        S.PVar name ->
          pure (Map.insert name (Forall [] [] ty) envAcc)
        S.PWildcard ->
          pure envAcc
        _ ->
          inferFail (UnsupportedPattern pat)

valueDecls :: [S.Decl] -> [(Text, [S.Pattern], S.Expr)]
valueDecls =
  foldr go []
  where
    go decl acc =
      case decl of
        S.DeclValue name args expr ->
          (name, args, expr) : acc
        _ ->
          acc

buildAliasEnv :: [S.Decl] -> AliasEnv
buildAliasEnv decls =
  Map.fromList
    [ (name, (vars, convertType Map.empty body))
    | S.DeclTypeAlias name vars body <- decls
    ]

type AliasEnv = Map Text ([Text], Type)

buildSigEnv :: AliasEnv -> [S.Decl] -> TypeEnv
buildSigEnv aliasEnv decls =
  Map.fromList
    [ (name, schemeFromQualType aliasEnv qualTy)
    | S.DeclTypeSig name qualTy <- decls
    ]

schemeFromQualType :: AliasEnv -> S.QualType -> Scheme
schemeFromQualType aliasEnv (S.QualType constraints ty) =
  Forall vars constraints' ty'
  where
    ty' = expandAliases aliasEnv (convertType aliasEnv ty)
    constraints' = map (convertConstraint aliasEnv) constraints
    vars = Set.toList (ftvConstraints constraints' <> ftvType ty')

convertConstraint :: AliasEnv -> S.Constraint -> Constraint
convertConstraint aliasEnv constraint =
  Constraint
    { constraintClass = S.constraintClass constraint
    , constraintArgs = map (expandAliases aliasEnv . convertType aliasEnv) (S.constraintArgs constraint)
    }

buildCtorEnv :: AliasEnv -> [S.Decl] -> TypeEnv
buildCtorEnv aliasEnv decls =
  Map.fromList (concatMap (ctorsFromDecl aliasEnv) decls)

ctorsFromDecl :: AliasEnv -> S.Decl -> [(Text, Scheme)]
ctorsFromDecl aliasEnv decl =
  case decl of
    S.DeclType typeName vars ctors ->
      let resultTy = foldl TApp (TCon typeName) (map TVar vars)
       in map (ctorScheme vars resultTy) ctors
    _ ->
      []
  where
    ctorScheme vars resultTy (S.TypeCtor name args) =
      let argTys = map (expandAliases aliasEnv . convertType aliasEnv) args
          ty = foldr TArrow resultTy argTys
       in (name, Forall vars [] ty)

builtinEnv :: TypeEnv
builtinEnv =
  Map.fromList
    [ ("addInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("geInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("unit", Forall [] [] (TCon "Unit"))
    , ("True", Forall [] [] (TCon "Bool"))
    , ("False", Forall [] [] (TCon "Bool"))
    , ("Nil", Forall ["a"] [] (TApp (TCon "List") (TVar "a")))
    , ("Cons", Forall ["a"] [] (TArrow (TVar "a") (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))))
    , ("Ok", Forall ["e", "a"] [] (TArrow (TVar "a") (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))))
    , ("Err", Forall ["e", "a"] [] (TArrow (TVar "e") (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))))
    , ("pureM", Forall ["m", "a"] [Constraint "Monad" [TVar "m"]] (TArrow (TVar "a") (TApp (TVar "m") (TVar "a"))))
    , ("thenM", Forall ["m", "a", "b"] [Constraint "Monad" [TVar "m"]] (TArrow (TApp (TVar "m") (TVar "a")) (TArrow (TApp (TVar "m") (TVar "b")) (TApp (TVar "m") (TVar "b")))))
    , ("bindM", Forall ["m", "a", "b"] [Constraint "Monad" [TVar "m"]] (TArrow (TApp (TVar "m") (TVar "a")) (TArrow (TArrow (TVar "a") (TApp (TVar "m") (TVar "b"))) (TApp (TVar "m") (TVar "b")))))
    ]

convertType :: AliasEnv -> S.Type -> Type
convertType aliasEnv ty =
  case ty of
    S.TypeCon name ->
      TCon name
    S.TypeVar name ->
      TVar name
    S.TypeApp f x ->
      TApp (convertType aliasEnv f) (convertType aliasEnv x)
    S.TypeArrow a b ->
      TArrow (convertType aliasEnv a) (convertType aliasEnv b)
    S.TypeRecord fields ->
      TRecord (sortOn fst [(name, convertType aliasEnv t) | (name, t) <- fields])

expandAliases :: AliasEnv -> Type -> Type
expandAliases aliasEnv =
  go Set.empty
  where
    go seen ty =
      case ty of
        TVar _ ->
          ty

        TCon name ->
          case Map.lookup name aliasEnv of
            Nothing ->
              ty
            Just (params, body) ->
              if null params && not (name `Set.member` seen)
                then go (Set.insert name seen) body
                else ty

        TArrow a b ->
          TArrow (go seen a) (go seen b)

        TRecord fields ->
          TRecord [(name, go seen fieldTy) | (name, fieldTy) <- fields]

        TApp {} ->
          case unapplyApps ty of
            (TCon name, args) ->
              case Map.lookup name aliasEnv of
                Nothing ->
                  rebuildApps (TCon name) (map (go seen) args)
                Just (params, body) ->
                  if name `Set.member` seen
                    then rebuildApps (TCon name) (map (go seen) args)
                    else
                      if length params /= length args
                        then rebuildApps (TCon name) (map (go seen) args)
                        else
                          let subst = Map.fromList (zip params (map (go seen) args))
                           in go (Set.insert name seen) (applySubstType subst body)
            (headTy, args) ->
              rebuildApps (go seen headTy) (map (go seen) args)

    rebuildApps =
      foldl TApp

    unapplyApps =
      goApps []
      where
        goApps args t =
          case t of
            TApp f x ->
              goApps (x : args) f
            _ ->
              (t, args)

renderScheme :: Scheme -> Text
renderScheme (Forall vars constraints ty) =
  forallPart <> constraintsPart <> renderType ty
  where
    forallPart =
      case vars of
        [] -> ""
        _ -> "forall " <> T.intercalate " " vars <> ". "

    constraintsPart =
      case constraints of
        [] -> ""
        _ -> renderConstraints constraints <> " => "

renderConstraints :: [Constraint] -> Text
renderConstraints constraints =
  case constraints of
    [c] -> renderConstraint c
    _ -> "(" <> T.intercalate ", " (map renderConstraint constraints) <> ")"

renderConstraint :: Constraint -> Text
renderConstraint constraint =
  case constraintArgs constraint of
    [] ->
      constraintClass constraint
    args ->
      constraintClass constraint <> " " <> T.intercalate " " (map renderConstraintArg args)
  where
    renderConstraintArg t =
      case t of
        TArrow {} -> "(" <> renderType t <> ")"
        TApp {} -> "(" <> renderType t <> ")"
        _ -> renderType t

renderType :: Type -> Text
renderType ty =
  case ty of
    TVar v ->
      v
    TCon n ->
      n
    TApp f x ->
      renderType f <> " " <> renderAtom x
    TArrow a b ->
      renderAtom a <> " -> " <> renderType b
    TRecord fields ->
      "{ " <> T.intercalate ", " [name <> " : " <> renderType t | (name, t) <- fields] <> " }"
  where
    renderAtom t =
      case t of
        TArrow {} -> "(" <> renderType t <> ")"
        TApp {} -> "(" <> renderType t <> ")"
        _ -> renderType t
