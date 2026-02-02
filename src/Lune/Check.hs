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
import qualified Lune.Builtins as Builtins
import Lune.Desugar (desugarModule)
import qualified Lune.ClassEnv as CE
import qualified Lune.InstanceEnv as IE
import qualified Lune.Kind as K
import Lune.Kind (KindEnv, KindError, kindCheckQualType, kindEnvFromDecls)
import qualified Lune.Syntax as S
import Lune.Infer
import Lune.Type
import Lune.TypeConvert (AliasEnv, buildAliasEnv, convertType, expandAliases, schemeFromQualType)

data TypecheckError
  = InValue Text InferError
  | InTypeSig Text KindError
  | InClass CE.ClassError
  | InInstance IE.InstanceError
  | InInstanceMethod Text InferError

instance Show TypecheckError where
  show err =
    case err of
      InValue name inferErr ->
        T.unpack name <> ": " <> show inferErr
      InTypeSig name kindErr ->
        T.unpack name <> ": " <> show kindErr
      InClass classErr ->
        show classErr
      InInstance instErr ->
        show instErr
      InInstanceMethod label inferErr ->
        T.unpack label <> ": " <> show inferErr

typecheckModule :: S.Module -> Either TypecheckError [(Text, Scheme)]
typecheckModule m = do
  classEnv <-
    case CE.buildClassEnv m' of
      Left err -> Left (InClass err)
      Right env -> Right env

  methodEnv <-
    case CE.classMethodEnv classEnv of
      Left err -> Left (InClass err)
      Right env -> Right env

  instanceEnv <-
    case IE.buildInstanceEnv m' classEnv of
      Left err -> Left (InInstance err)
      Right env -> Right env

  checkSigKinds kindEnv (CE.classKindEnv classEnv) decls

  let env0 = Builtins.builtinSchemes <> ctorEnv <> methodEnv <> sigEnv

  checkInstanceMethods env0 aliasEnv classEnv instanceEnv
  go env0 [] (valueDecls decls)
  where
    m' = desugarModule m
    decls = S.modDecls m'
    kindEnv = kindEnvFromDecls decls
    aliasEnv = buildAliasEnv decls
    sigEnv = buildSigEnv aliasEnv decls
    ctorEnv = buildCtorEnv aliasEnv decls

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

checkSigKinds :: KindEnv -> K.ClassEnv -> [S.Decl] -> Either TypecheckError ()
checkSigKinds kindEnv classKinds =
  foldM_ step ()
  where
    step () decl =
      case decl of
        S.DeclTypeSig name qualTy ->
          case kindCheckQualType kindEnv classKinds qualTy of
            Left err ->
              Left (InTypeSig name err)
            Right () ->
              Right ()
        _ ->
          Right ()

checkInstanceMethods :: TypeEnv -> AliasEnv -> CE.ClassEnv -> IE.InstanceEnv -> Either TypecheckError ()
checkInstanceMethods env0 aliasEnv classEnv instanceEnv =
  mapM_ checkInstance (Map.elems instanceEnv)
  where
    checkInstance inst = do
      classInfo <-
        case Map.lookup (IE.instanceInfoClass inst) classEnv of
          Nothing ->
            Left (InInstance (IE.UnknownClass (IE.instanceInfoClass inst)))
          Just info ->
            Right info

      let params = CE.classInfoParams classInfo
      paramName <-
        case params of
          (p : _) -> Right p
          _ -> Left (InInstance (IE.UnknownClass (IE.instanceInfoClass inst)))

      let instTy = expandAliases aliasEnv (convertType aliasEnv (IE.instanceInfoHead inst))
          instVars = Set.toList (ftvType instTy)

      let requiredMethods = CE.classInfoMethods classInfo
          methodSchemes =
            Map.map (specializeMethodScheme (IE.instanceInfoClass inst) paramName instTy instVars) requiredMethods

      let instEnv = methodSchemes <> env0

      mapM_ (checkMethod inst instEnv methodSchemes) (Map.toList (IE.instanceInfoMethods inst))
      pure ()

    checkMethod inst instEnv expectedSchemes (methodName, methodExpr) =
      case Map.lookup methodName expectedSchemes of
        Nothing ->
          pure ()
        Just scheme ->
          case runInferM (checkAgainstScheme instEnv methodName [] methodExpr scheme) of
            Left err ->
              Left (InInstanceMethod (methodLabel inst methodName) err)
            Right () ->
              Right ()

    methodLabel inst methodName =
      IE.instanceInfoClass inst <> " " <> IE.instanceInfoHeadCon inst <> "." <> methodName

specializeMethodScheme :: Text -> Text -> Type -> [Text] -> Scheme -> Scheme
specializeMethodScheme className classParam instTy instVars (Forall vars constraints ty) =
  Forall varsFinal constraintsFinal tyFinal
  where
    subst = Map.singleton classParam instTy
    vars' = filter (/= classParam) vars
    tyFinal = applySubstType subst ty
    constraintsSub = applySubstConstraints subst constraints
    constraintsFinal = filter (/= Constraint className [instTy]) constraintsSub
    varsFinal = Set.toList (Set.fromList (instVars <> vars'))

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

buildSigEnv :: AliasEnv -> [S.Decl] -> TypeEnv
buildSigEnv aliasEnv decls =
  Map.fromList
    [ (name, schemeFromQualType aliasEnv qualTy)
    | S.DeclTypeSig name qualTy <- decls
    ]

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
