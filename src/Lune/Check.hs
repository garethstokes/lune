module Lune.Check
  ( TypecheckError (..)
  , TypeScheme
  , typecheckModule
  , typecheckModuleEnv
  , renderScheme
  , renderTypeScheme
  , renderTypeSchemeHover
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
import qualified Lune.Pretty.Type as PrettyType
import Lune.Type
import Lune.TypeConvert (AliasEnv, buildAliasEnv, convertType, expandAliases, schemeFromQualType)

type TypeScheme = Scheme

data InstanceCandidate = InstanceCandidate
  { instanceCandidateClass :: Text
  , instanceCandidateHead :: Type
  , instanceCandidateLabel :: Text
  }
  deriving (Eq, Show)

data TypecheckError
  = InValue Text InferError
  | InTypeSig Text KindError
  | InClass CE.ClassError
  | InInstance IE.InstanceError
  | InInstanceMethod Text InferError
  | MissingInstance Text Constraint
  | AmbiguousConstraint Text Constraint
  | UnsupportedConstraint Text Constraint
  | OverlappingInstances Text Constraint [Text]

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
      MissingInstance label c ->
        T.unpack label <> ": missing instance for " <> T.unpack (renderConstraint c)
      AmbiguousConstraint label c ->
        T.unpack label <> ": cannot resolve constraint " <> T.unpack (renderConstraint c)
      UnsupportedConstraint label c ->
        T.unpack label <> ": unsupported constraint form " <> T.unpack (renderConstraint c)
      OverlappingInstances label c matches ->
        T.unpack label <> ": overlapping instances for " <> T.unpack (renderConstraint c) <> " (" <> T.unpack (T.intercalate ", " matches) <> ")"

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
      instanceCandidates = builtinInstanceCandidates <> instanceCandidatesFromEnv aliasEnv instanceEnv

  checkInstanceMethods env0 aliasEnv classEnv instanceCandidates instanceEnv
  valuePairs <- go classEnv env0 [] instanceCandidates (valueDecls decls)
  pure (foreignSchemes ++ valuePairs)
  where
    m' = desugarModule m
    decls = S.modDecls m'
    kindEnv = kindEnvFromDecls decls
    aliasEnv = buildAliasEnv decls
    sigEnv = buildSigEnv aliasEnv decls
    ctorEnv = buildCtorEnv aliasEnv decls

    foreignSchemes =
      [ (name, schemeFromQualType aliasEnv qualTy)
      | S.DeclForeignImport _ _ name qualTy <- decls
      ]

    go _ _ acc _ [] =
      Right (reverse acc)
    go classEnv0 env acc instances (decl : rest) = do
      (name, scheme) <- checkDecl classEnv0 instances env sigEnv decl
      let env' = Map.insert name scheme env
      go classEnv0 env' ((name, scheme) : acc) instances rest

    nameOf (n, _, _) = n

typecheckModuleEnv :: S.Module -> Either TypecheckError (Map Text TypeScheme)
typecheckModuleEnv m =
  Map.fromList <$> typecheckModule m

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
        S.DeclForeignImport _ _ name qualTy ->
          case kindCheckQualType kindEnv classKinds qualTy of
            Left err ->
              Left (InTypeSig name err)
            Right () ->
              Right ()
        _ ->
          Right ()

checkInstanceMethods :: TypeEnv -> AliasEnv -> CE.ClassEnv -> [InstanceCandidate] -> IE.InstanceEnv -> Either TypecheckError ()
checkInstanceMethods env0 aliasEnv classEnv instanceCandidates instanceEnv =
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
          case runInferM (inferAgainstScheme instEnv (methodLabel inst methodName) [] methodExpr scheme) of
            Left err ->
              Left (InInstanceMethod (methodLabel inst methodName) err)
            Right (givenConstraints, wantedConstraints) ->
              solveWantedConstraints classEnv instanceCandidates (methodLabel inst methodName) givenConstraints wantedConstraints

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

checkDecl :: CE.ClassEnv -> [InstanceCandidate] -> TypeEnv -> TypeEnv -> (Text, [S.Pattern], S.Expr) -> Either TypecheckError (Text, Scheme)
checkDecl classEnv instanceCandidates env sigEnv (name, args, body) =
  case Map.lookup name sigEnv of
    Just scheme -> do
      checkAgainstScheme classEnv instanceCandidates env name args body scheme
      pure (name, scheme)
    Nothing ->
      case runInferM (inferExpr env (S.Lam args body)) of
        Left err ->
          Left (InValue name err)
        Right (s, constraints, ty) ->
          let env' = applySubstEnv s env
              scheme = generalize env' constraints ty
           in Right (name, scheme)

checkAgainstScheme :: CE.ClassEnv -> [InstanceCandidate] -> TypeEnv -> Text -> [S.Pattern] -> S.Expr -> Scheme -> Either TypecheckError ()
checkAgainstScheme classEnv instanceCandidates env label args body scheme =
  case runInferM (inferAgainstScheme env label args body scheme) of
    Left err ->
      Left (InValue label err)
    Right (givenConstraints, wantedConstraints) ->
      solveWantedConstraints classEnv instanceCandidates label givenConstraints wantedConstraints

inferAgainstScheme :: TypeEnv -> Text -> [S.Pattern] -> S.Expr -> Scheme -> InferM ([Constraint], [Constraint])
inferAgainstScheme env name args body scheme = do
  (givenConstraints0, skTy) <- skolemizeScheme scheme
  (argTys, resTy) <- peelFunctionType name (length args) skTy
  argEnv <- bindTopLevelArgs args argTys
  (sBody, wanted0, bodyTy) <- inferExpr (argEnv <> env) body
  sRes <- unify (applySubstType sBody bodyTy) (applySubstType sBody resTy)
  let sFinal = sRes `composeSubst` sBody
      givenConstraints = applySubstConstraints sFinal givenConstraints0
      wantedConstraints = applySubstConstraints sFinal wanted0
  pure (givenConstraints, wantedConstraints)

builtinInstanceCandidates :: [InstanceCandidate]
builtinInstanceCandidates =
  [ InstanceCandidate
      { instanceCandidateClass = cls
      , instanceCandidateHead = builtinHeadType headCon
      , instanceCandidateLabel = cls <> " " <> renderType (builtinHeadType headCon)
      }
  | ((cls, headCon), _) <- Map.toList Builtins.builtinInstanceDicts
  ]
  where
    builtinHeadType headCon =
      case headCon of
        "Result" ->
          TApp (TCon "Result") (TVar "e")
        "Task" ->
          TApp (TCon "Task") (TVar "e")
        other ->
          TCon other

instanceCandidatesFromEnv :: AliasEnv -> IE.InstanceEnv -> [InstanceCandidate]
instanceCandidatesFromEnv aliasEnv instanceEnv =
  [ InstanceCandidate
      { instanceCandidateClass = IE.instanceInfoClass inst
      , instanceCandidateHead = headTy
      , instanceCandidateLabel = IE.instanceInfoClass inst <> " " <> renderType headTy
      }
  | inst <- Map.elems instanceEnv
  , let headTy = expandAliases aliasEnv (convertType aliasEnv (IE.instanceInfoHead inst))
  ]

solveWantedConstraints :: CE.ClassEnv -> [InstanceCandidate] -> Text -> [Constraint] -> [Constraint] -> Either TypecheckError ()
solveWantedConstraints classEnv instanceCandidates label givenConstraints wantedConstraints = do
  let givenSet = deriveSuperConstraints classEnv (Set.fromList givenConstraints)
      wantedUnique = Set.toList (Set.fromList wantedConstraints)
  mapM_ (solveOne givenSet) wantedUnique
  where
    solveOne givenSet c
      | c `Set.member` givenSet =
          Right ()
      | otherwise =
          solveViaInstances c

    solveViaInstances c =
      case constraintArgs c of
        [argTy] ->
          case headTypeCon argTy of
            Nothing ->
              Left (AmbiguousConstraint label c)
            Just _ -> do
              let matches =
                    [ instanceCandidateLabel inst
                    | inst <- instanceCandidates
                    , instanceCandidateClass inst == constraintClass c
                    , instanceMatches (instanceCandidateHead inst) argTy
                    ]
              case matches of
                [] ->
                  Left (MissingInstance label c)
                [_one] ->
                  Right ()
                many ->
                  Left (OverlappingInstances label c many)
        _ ->
          Left (UnsupportedConstraint label c)

    instanceMatches instHead wantedHead =
      case runInferM (unify instHead wantedHead) of
        Left _ ->
          False
        Right _ ->
          True

headTypeCon :: Type -> Maybe Text
headTypeCon ty =
  case ty of
    TCon n ->
      Just n
    TApp f _ ->
      headTypeCon f
    _ ->
      Nothing

deriveSuperConstraints :: CE.ClassEnv -> Set Constraint -> Set Constraint
deriveSuperConstraints classEnv direct =
  go (Set.toList direct) direct
  where
    go [] acc =
      acc
    go (c : cs) acc =
      case Map.lookup (constraintClass c) classEnv of
        Nothing ->
          go cs acc
        Just info ->
          let params = CE.classInfoParams info
              args = constraintArgs c
           in if length params /= length args
                then go cs acc
                else
                  let subst = Map.fromList (zip params args)
                      supers = map (applySubstConstraint subst) (CE.classInfoSupers info)
                      (acc', newWork) = foldl addOne (acc, []) supers
                   in go (cs <> newWork) acc'

    addOne (accSet, work) superC =
      if superC `Set.member` accSet
        then (accSet, work)
        else (Set.insert superC accSet, work <> [superC])

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
    <> Map.fromList
      [ (name, schemeFromQualType aliasEnv qualTy)
      | S.DeclForeignImport _ _ name qualTy <- decls
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
    S.DeclTypeAnn _ typeName vars ctors ->
      let resultTy = foldl TApp (TCon typeName) (map TVar vars)
       in map (ctorScheme vars resultTy) ctors
    S.DeclNewtype typeName vars ctorName ctorTy ->
      let resultTy = foldl TApp (TCon typeName) (map TVar vars)
          argTy = expandAliases aliasEnv (convertType aliasEnv ctorTy)
          ty = TArrow argTy resultTy
       in [(ctorName, Forall vars [] ty)]
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

renderTypeScheme :: TypeScheme -> Text
renderTypeScheme =
  renderScheme

renderTypeSchemeHover :: TypeScheme -> Text
renderTypeSchemeHover =
  PrettyType.renderTypeSchemeHover

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
