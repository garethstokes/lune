module Lune.Elaborate
  ( ElaborateError (..)
  , TypedModule (..)
  , prepareTypedModule
  , elaborateModule
  , elaborateExpr
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Builtins as Builtins
import qualified Lune.Check as Check
import qualified Lune.ClassEnv as CE
import Lune.Core
import Lune.Desugar (desugarModule)
import qualified Lune.Infer as I
import qualified Lune.InstanceEnv as IE
import qualified Lune.Syntax as S
import Lune.Type
import Lune.TypeConvert (AliasEnv, buildAliasEnv, convertType, expandAliases)

data ElaborateError
  = ElaborateTypecheckError Check.TypecheckError
  | ElaborateClassError CE.ClassError
  | ElaborateInstanceError IE.InstanceError
  | ElaborateDuplicateMethodName Text
  | ElaborateMissingScheme Text
  | ElaborateInferError Text I.InferError
  | ElaborateUnresolvedConstraint Constraint
  | ElaborateOverlappingInstances Constraint [Text]
  deriving (Show)

data TypedModule = TypedModule
  { tmModule :: S.Module
  , tmValueSchemes :: Map Text Scheme
  , tmClassEnv :: CE.ClassEnv
  , tmInstanceEnv :: IE.InstanceEnv
  }
  deriving (Show)

prepareTypedModule :: S.Module -> Either ElaborateError TypedModule
prepareTypedModule m = do
  let m' = desugarModule m

  classEnv <-
    case CE.buildClassEnv m' of
      Left err -> Left (ElaborateClassError err)
      Right env -> Right env

  instanceEnv <-
    case IE.buildInstanceEnv m' classEnv of
      Left err -> Left (ElaborateInstanceError err)
      Right env -> Right env

  schemesList <-
    case Check.typecheckModule m' of
      Left err -> Left (ElaborateTypecheckError err)
      Right bs -> Right bs

  pure
    TypedModule
      { tmModule = m'
      , tmValueSchemes = Map.fromList schemesList
      , tmClassEnv = classEnv
      , tmInstanceEnv = instanceEnv
      }

type MethodIndex = Map Text Text

buildMethodIndex :: CE.ClassEnv -> Either ElaborateError MethodIndex
buildMethodIndex classEnv =
  foldl step (Right Map.empty) (Map.elems classEnv)
  where
    step acc info = do
      m <- acc
      foldl addOne (Right m) (Map.keys (CE.classInfoMethods info))
      where
        addOne acc' methodName = do
          m' <- acc'
          case Map.lookup methodName m' of
            Nothing ->
              Right (Map.insert methodName (CE.classInfoName info) m')
            Just _ ->
              Left (ElaborateDuplicateMethodName methodName)

type InstanceDicts = Map (Text, Text) Text

buildInstanceDicts :: IE.InstanceEnv -> InstanceDicts
buildInstanceDicts instEnv =
  fromInstances <> Builtins.builtinInstanceDicts
  where
    fromInstances =
      Map.fromList
        [ ((IE.instanceInfoClass inst, IE.instanceInfoHeadCon inst), Builtins.instanceDictName (IE.instanceInfoClass inst) (IE.instanceInfoHeadCon inst))
        | inst <- Map.elems instEnv
        ]

elaborateModule :: TypedModule -> Either ElaborateError CoreModule
elaborateModule tm = do
  methodIndex <- buildMethodIndex (tmClassEnv tm)
  methodEnv <-
    case CE.classMethodEnv (tmClassEnv tm) of
      Left err -> Left (ElaborateClassError err)
      Right env -> Right env

  let decls = S.modDecls (tmModule tm)
      aliasEnv = buildAliasEnv decls
      ctorEnv = buildCtorEnv aliasEnv decls
      env0 = Builtins.builtinSchemes <> ctorEnv <> methodEnv <> tmValueSchemes tm
      instanceDicts = buildInstanceDicts (tmInstanceEnv tm)
      dictCandidates = buildDictCandidates aliasEnv (tmInstanceEnv tm)

  dictDecls <- mapM (elabInstanceDict methodIndex instanceDicts dictCandidates env0 (tmClassEnv tm)) (Map.elems (tmInstanceEnv tm))
  valueDecls <- mapM (elabValueDecl methodIndex instanceDicts dictCandidates env0 (tmClassEnv tm)) (valueDeclsOf decls)

  pure
    CoreModule
      { coreName = S.modName (tmModule tm)
      , coreDecls = Builtins.builtinCoreDecls <> dictDecls <> valueDecls
      }

-- Exposed for debugging/smaller tests.
elaborateExpr :: Map Text Scheme -> CE.ClassEnv -> IE.InstanceEnv -> S.Expr -> Either ElaborateError CoreExpr
elaborateExpr env classEnv instanceEnv expr = do
  methodIndex <- buildMethodIndex classEnv
  methodEnv <-
    case CE.classMethodEnv classEnv of
      Left err -> Left (ElaborateClassError err)
      Right e -> Right e
  let instanceDicts = buildInstanceDicts instanceEnv
      env0 = Builtins.builtinSchemes <> methodEnv <> env
  case I.runInferM (inferCoreExpr methodIndex env0 expr) of
    Left err ->
      Left (ElaborateInferError "<expr>" err)
    Right (s, _cs, _ty, core0) ->
      resolveInstanceDicts s (buildDictCandidates Map.empty instanceEnv) core0

elabInstanceDict :: MethodIndex -> InstanceDicts -> [DictCandidate] -> TypeEnv -> CE.ClassEnv -> IE.InstanceInfo -> Either ElaborateError CoreDecl
elabInstanceDict methodIndex instanceDicts dictCandidates env0 classEnv inst = do
  let dictName = Builtins.instanceDictName (IE.instanceInfoClass inst) (IE.instanceInfoHeadCon inst)
  fields <- mapM elabMethod (Map.toList (IE.instanceInfoMethods inst))
  superFields <- elabSuperDictFields classEnv instanceDicts inst
  pure (CoreDecl dictName (CRecord (superFields <> fields)))
  where
    elabMethod (methodName, methodExpr) =
      case I.runInferM (inferCoreExpr methodIndex env0 methodExpr) of
        Left err ->
          Left (ElaborateInferError (IE.instanceInfoClass inst <> " " <> IE.instanceInfoHeadCon inst <> "." <> methodName) err)
        Right (s, _cs, _ty, core0) -> do
          core <- resolveInstanceDicts s dictCandidates core0
          Right (methodName, core)

elabValueDecl :: MethodIndex -> InstanceDicts -> [DictCandidate] -> TypeEnv -> CE.ClassEnv -> S.Decl -> Either ElaborateError CoreDecl
elabValueDecl methodIndex instanceDicts dictCandidates env0 classEnv decl =
  case decl of
    S.DeclValue name args body -> do
      scheme <-
        case Map.lookup name env0 of
          Just s -> Right s
          Nothing -> Left (ElaborateMissingScheme name)

      case I.runInferM (inferTopLevel classEnv methodIndex env0 scheme args body) of
        Left err ->
          Left (ElaborateInferError name err)
        Right (s, core0) -> do
          core <- resolveInstanceDicts s dictCandidates core0
          Right (CoreDecl name core)
    _ ->
      Left (ElaborateMissingScheme "<non-value decl>")

valueDeclsOf :: [S.Decl] -> [S.Decl]
valueDeclsOf =
  filter isValue
  where
    isValue decl =
      case decl of
        S.DeclValue {} -> True
        _ -> False

inferTopLevel :: CE.ClassEnv -> MethodIndex -> TypeEnv -> Scheme -> [S.Pattern] -> S.Expr -> I.InferM (Subst, CoreExpr)
inferTopLevel classEnv methodIndex env0 scheme args body = do
  (topConstraints, topTy) <- instantiate scheme
  let dictParams = zipWith (dictParamPattern "dict") [0 :: Int ..] topConstraints

  (argTys, resTy) <- peelArrows (length args) topTy
  argEnv <- bindTopLevelArgs args argTys
  (sBody, _cBody, bodyTy, coreBody0) <- inferCoreExpr methodIndex (argEnv <> env0) body
  sRes <- I.unify (applySubstType sBody bodyTy) (applySubstType sBody resTy)
  let sFinal = sRes `composeSubst` sBody
      givenDirect =
        Map.fromList
          [ (applySubstConstraint sFinal c, CVar (patName dictPat))
          | (c, dictPat) <- zip topConstraints dictParams
          ]
      givenAll = deriveSuperDicts classEnv givenDirect
      coreBody1 = replaceGivenDicts sFinal givenAll coreBody0

  let allParams = dictParams <> args
      coreExpr =
        if null allParams
          then coreBody1
          else CLam allParams coreBody1
  pure (sFinal, coreExpr)
  where
    patName pat =
      case pat of
        S.PVar n -> n
        _ -> "_"

    dictParamPattern prefix i c =
      let base = "$" <> prefix <> constraintClass c <> T.pack (show i)
       in S.PVar base

peelArrows :: Int -> Type -> I.InferM ([Type], Type)
peelArrows 0 ty =
  pure ([], ty)
peelArrows n ty =
  case ty of
    TArrow a b -> do
      (as, res) <- peelArrows (n - 1) b
      pure (a : as, res)
    _ ->
      I.inferFail (I.ExpectedFunctionType ty)

bindTopLevelArgs :: [S.Pattern] -> [Type] -> I.InferM TypeEnv
bindTopLevelArgs pats tys =
  go Map.empty (zip pats tys)
  where
    go envAcc [] =
      pure envAcc
    go envAcc ((pat, ty) : rest) =
      case pat of
        S.PVar name ->
          go (Map.insert name (Forall [] [] ty) envAcc) rest
        S.PWildcard ->
          go envAcc rest
        _ ->
          I.inferFail (I.UnsupportedPattern pat)

inferCoreExpr :: MethodIndex -> TypeEnv -> S.Expr -> I.InferM (Subst, [Constraint], Type, CoreExpr)
inferCoreExpr methodIndex env expr =
  case expr of
    S.Var name ->
      inferVar methodIndex env name

    S.StringLit s ->
      pure (nullSubst, [], TCon "String", CString s)

    S.IntLit n ->
      pure (nullSubst, [], TCon "Int", CInt n)

    S.App f x -> do
      (s1, c1, t1, cf) <- inferCoreExpr methodIndex env f
      (s2, c2, t2, cx) <- inferCoreExpr methodIndex (applySubstEnv s1 env) x
      tv <- freshTypeVar
      s3 <- I.unify (applySubstType s2 t1) (TArrow t2 tv)
      let s = s3 `composeSubst` s2 `composeSubst` s1
          c = applySubstConstraints s (c1 <> c2)
      pure (s, c, applySubstType s tv, CApp cf cx)

    S.Lam pats body -> do
      (envArgs, argTypes) <- bindLambdaPatterns pats
      (s1, c1, tBody, cBody) <- inferCoreExpr methodIndex (envArgs <> env) body
      let argTypes' = map (applySubstType s1) argTypes
          c = applySubstConstraints s1 c1
      pure (s1, c, foldr TArrow tBody argTypes', CLam pats cBody)

    S.LetIn name bound body -> do
      (s1, c1, t1, cBound0) <- inferCoreExpr methodIndex env bound
      let env1 = applySubstEnv s1 env
          scheme = generalize env1 c1 t1

      -- Always abstract over all constraints for now.
      let dictParams = zipWith (dictParamPattern "letDict") [0 :: Int ..] c1
          givenMap =
            Map.fromList
              [ (applySubstConstraint s1 c, CVar (patName dictPat))
              | (c, dictPat) <- zip c1 dictParams
              ]
          cBound1 = replaceGivenDicts s1 givenMap cBound0
          cBoundFinal =
            if null dictParams
              then cBound1
              else CLam dictParams cBound1

          env2 = Map.insert name scheme env1

      (s2, c2, t2, cBody) <- inferCoreExpr methodIndex env2 body
      let sFinal = s2 `composeSubst` s1
      pure (sFinal, applySubstConstraints sFinal c2, applySubstType sFinal t2, CLet name cBoundFinal cBody)
      where
        patName pat =
          case pat of
            S.PVar n -> n
            _ -> "_"

        dictParamPattern prefix i c =
          let base = "$" <> prefix <> constraintClass c <> T.pack (show i)
           in S.PVar base

    S.Case scrut alts -> do
      (s0, c0, tScrut0, cScrut) <- inferCoreExpr methodIndex env scrut
      tvRes <- freshTypeVar
      let env0 = applySubstEnv s0 env
      (sFinal, cAlts, altsCore) <- inferCaseAlts methodIndex env0 (s0, [], []) tScrut0 tvRes alts
      let cFinal = applySubstConstraints sFinal (c0 <> cAlts)
      pure (sFinal, cFinal, applySubstType sFinal tvRes, CCase cScrut altsCore)

    S.DoBlock _ ->
      I.inferFail I.UnexpectedDoBlock

    S.RecordLiteral fields ->
      inferRecordLiteral methodIndex env fields

    S.RecordUpdate base updates ->
      inferRecordUpdate methodIndex env base updates

    S.FieldAccess base field -> do
      (s1, c1, baseTy, cBase) <- inferCoreExpr methodIndex env base
      case applySubstType s1 baseTy of
        TRecord fields ->
          case lookup field fields of
            Nothing ->
              I.inferFail (I.MissingRecordField field)
            Just fieldTy ->
              pure (s1, applySubstConstraints s1 c1, fieldTy, CSelect cBase field)
        other ->
          I.inferFail (I.NotARecord other)

inferVar :: MethodIndex -> TypeEnv -> Text -> I.InferM (Subst, [Constraint], Type, CoreExpr)
inferVar methodIndex env name =
  case Map.lookup name env of
    Nothing ->
      I.inferFail (I.UnboundVariable name)
    Just scheme -> do
      (constraints, ty) <- instantiate scheme
      let core =
            case Map.lookup name methodIndex of
              Nothing ->
                foldl CApp (CVar name) (map CDictWanted constraints)
              Just cls ->
                case break ((== cls) . constraintClass) constraints of
                  (_, []) ->
                    foldl CApp (CVar name) (map CDictWanted constraints)
                  (before, classC : after) ->
                    let base = CSelect (CDictWanted classC) name
                        otherCs = before <> after
                     in foldl CApp base (map CDictWanted otherCs)
      pure (nullSubst, constraints, ty, core)

bindLambdaPatterns :: [S.Pattern] -> I.InferM (TypeEnv, [Type])
bindLambdaPatterns pats =
  go Map.empty [] pats
  where
    go envAcc tys [] =
      pure (envAcc, tys)
    go envAcc tys (pat : rest) =
      case pat of
        S.PVar name -> do
          tv <- freshTypeVar
          go (Map.insert name (Forall [] [] tv) envAcc) (tys <> [tv]) rest
        S.PWildcard -> do
          tv <- freshTypeVar
          go envAcc (tys <> [tv]) rest
        _ ->
          I.inferFail (I.UnsupportedPattern pat)

inferCaseAlts ::
  MethodIndex ->
  TypeEnv ->
  (Subst, [Constraint], [CoreAlt]) ->
  Type ->
  Type ->
  [S.Alt] ->
  I.InferM (Subst, [Constraint], [CoreAlt])
inferCaseAlts _ _ acc _ _ [] =
  pure acc
inferCaseAlts methodIndex envBase (sAcc, cAcc, altsAcc) scrutTy resTy (S.Alt pat body : rest) = do
  let env1 = applySubstEnv sAcc envBase
      scrutTy' = applySubstType sAcc scrutTy
      resTy' = applySubstType sAcc resTy
  (patEnv, cPat, patTy) <- I.inferPattern env1 pat
  s1 <- I.unify scrutTy' patTy
  let env2 = applySubstEnv s1 (patEnv <> env1)
      c1 = applySubstConstraints s1 (cAcc <> cPat)
  (sBody, cBody, bodyTy, cBodyCore) <- inferCoreExpr methodIndex env2 body
  let s' = sBody `composeSubst` s1 `composeSubst` sAcc
      c2 = applySubstConstraints s' (c1 <> cBody)
  sRes <- I.unify (applySubstType s' resTy') (applySubstType s' bodyTy)
  let sFinal = sRes `composeSubst` s'
      altCore = CoreAlt pat cBodyCore
  inferCaseAlts methodIndex envBase (sFinal, applySubstConstraints sFinal c2, altsAcc <> [altCore]) scrutTy resTy rest

inferRecordLiteral :: MethodIndex -> TypeEnv -> [(Text, S.Expr)] -> I.InferM (Subst, [Constraint], Type, CoreExpr)
inferRecordLiteral methodIndex env fields = do
  (s, c, fieldTypes, fieldExprs) <- foldlM inferField (nullSubst, [], [], []) fields
  let recordTy = TRecord (sortOn fst fieldTypes)
  pure (s, applySubstConstraints s c, applySubstType s recordTy, CRecord fieldExprs)
  where
    foldlM f z xs = go z xs
      where
        go acc [] = pure acc
        go acc (y : ys) = f acc y >>= \acc' -> go acc' ys

    inferField (sAcc, cAcc, tysAcc, exprAcc) (name, e) = do
      (s1, c1, t1, c1expr) <- inferCoreExpr methodIndex (applySubstEnv sAcc env) e
      let sFinal = s1 `composeSubst` sAcc
          cFinal = applySubstConstraints sFinal (cAcc <> c1)
          tFinal = applySubstType sFinal t1
      pure (sFinal, cFinal, tysAcc <> [(name, tFinal)], exprAcc <> [(name, c1expr)])

inferRecordUpdate :: MethodIndex -> TypeEnv -> S.Expr -> [(Text, S.Expr)] -> I.InferM (Subst, [Constraint], Type, CoreExpr)
inferRecordUpdate methodIndex env base updates = do
  (sBase, cBase, baseTy0, cBaseExpr) <- inferCoreExpr methodIndex env base
  case applySubstType sBase baseTy0 of
    TRecord fields0 -> do
      let env1 = applySubstEnv sBase env
      (sFinal, cFinal, updatesCore) <- foldlM (checkUpdate env1 fields0) (sBase, cBase, Map.empty) updates
      let tmpName = "$r"
          finalFields =
            [ (fieldName, fieldExpr fieldName)
            | (fieldName, _) <- fields0
            ]
          fieldExpr fieldName =
            case Map.lookup fieldName updatesCore of
              Just e -> e
              Nothing -> CSelect (CVar tmpName) fieldName
      pure (sFinal, applySubstConstraints sFinal cFinal, applySubstType sFinal (TRecord fields0), CLet tmpName cBaseExpr (CRecord finalFields))
    other ->
      I.inferFail (I.NotARecord other)
  where
    foldlM f z xs = go z xs
      where
        go acc [] = pure acc
        go acc (y : ys) = f acc y >>= \acc' -> go acc' ys

    checkUpdate env1 fields (sAcc, cAcc, exprAcc) (name, expr) =
      case lookup name fields of
        Nothing ->
          I.inferFail (I.MissingRecordField name)
        Just fieldTy -> do
          (s1, c1, exprTy, cExpr) <- inferCoreExpr methodIndex (applySubstEnv sAcc env1) expr
          let expectedTy = applySubstType sAcc fieldTy
          s2 <- I.unify (applySubstType s1 expectedTy) exprTy
          let sFinal = s2 `composeSubst` s1 `composeSubst` sAcc
              cFinal = applySubstConstraints sFinal (cAcc <> c1)
          pure (sFinal, cFinal, Map.insert name cExpr exprAcc)

replaceGivenDicts :: Subst -> Map Constraint CoreExpr -> CoreExpr -> CoreExpr
replaceGivenDicts subst given =
  go
  where
    go expr =
      case expr of
        CDictWanted c ->
          case Map.lookup (applySubstConstraint subst c) given of
            Just dictExpr ->
              dictExpr
            Nothing ->
              expr
        CVar {} ->
          expr
        CString {} ->
          expr
        CInt {} ->
          expr
        CApp f x ->
          CApp (go f) (go x)
        CLam pats body ->
          CLam pats (go body)
        CLet name bound body ->
          CLet name (go bound) (go body)
        CCase scrut alts ->
          CCase (go scrut) (map goAlt alts)
        CRecord fields ->
          CRecord [(name, go e) | (name, e) <- fields]
        CSelect base field ->
          CSelect (go base) field

    goAlt (CoreAlt pat body) =
      CoreAlt pat (go body)

deriveSuperDicts :: CE.ClassEnv -> Map Constraint CoreExpr -> Map Constraint CoreExpr
deriveSuperDicts classEnv direct =
  go (Map.toList direct) direct
  where
    go [] acc =
      acc
    go ((c, dictExpr) : rest) acc =
      case Map.lookup (constraintClass c) classEnv of
        Nothing ->
          go rest acc
        Just info ->
          let params = CE.classInfoParams info
              args = constraintArgs c
           in if length params /= length args
                then go rest acc
                else
                  let subst = Map.fromList (zip params args)
                      supers = map (applySubstConstraint subst) (CE.classInfoSupers info)
                      (acc', newWork) = foldl (addOne dictExpr) (acc, []) supers
                   in go (rest <> newWork) acc'

    addOne dictExpr (accMap, work) superC =
      let field = "$super" <> constraintClass superC
          dictExpr' = CSelect dictExpr field
       in case Map.lookup superC accMap of
            Just _ ->
              (accMap, work)
            Nothing ->
              (Map.insert superC dictExpr' accMap, work <> [(superC, dictExpr')])

elabSuperDictFields :: CE.ClassEnv -> InstanceDicts -> IE.InstanceInfo -> Either ElaborateError [(Text, CoreExpr)]
elabSuperDictFields classEnv instanceDicts inst = do
  case Map.lookup (IE.instanceInfoClass inst) classEnv of
    Nothing ->
      Right []
    Just info ->
      mapM (oneSuper (IE.instanceInfoHeadCon inst)) (CE.classInfoSupers info)
  where
    oneSuper headCon superC = do
      let superCls = constraintClass superC
      dictName <-
        case Map.lookup (superCls, headCon) instanceDicts of
          Nothing ->
            Left (ElaborateUnresolvedConstraint (Constraint superCls [TCon headCon]))
          Just name ->
            Right name
      let fieldName = "$super" <> superCls
      Right (fieldName, CVar dictName)

resolveInstanceDicts :: Subst -> [DictCandidate] -> CoreExpr -> Either ElaborateError CoreExpr
resolveInstanceDicts subst dictCandidates =
  go
  where
    go expr =
      case expr of
        CDictWanted c ->
          resolveConstraint (applySubstConstraint subst c)
        CVar {} ->
          Right expr
        CString {} ->
          Right expr
        CInt {} ->
          Right expr
        CApp f x ->
          CApp <$> go f <*> go x
        CLam pats body ->
          CLam pats <$> go body
        CLet name bound body ->
          CLet name <$> go bound <*> go body
        CCase scrut alts ->
          CCase <$> go scrut <*> mapM goAlt alts
        CRecord fields ->
          CRecord <$> mapM (\(n, e) -> do e' <- go e; pure (n, e')) fields
        CSelect base field ->
          CSelect <$> go base <*> pure field

    goAlt (CoreAlt pat body) =
      CoreAlt pat <$> go body

    resolveConstraint c =
      case constraintArgs c of
        [argTy] ->
          case headTypeCon argTy of
            Nothing ->
              Left (ElaborateUnresolvedConstraint c)
            Just _ -> do
              let matches =
                    [ dictName
                    | DictCandidate cls headTy dictName <- dictCandidates
                    , cls == constraintClass c
                    , instanceMatches headTy argTy
                    ]
              case matches of
                [] ->
                  Left (ElaborateUnresolvedConstraint c)
                [dictName] ->
                  Right (CVar dictName)
                many ->
                  Left (ElaborateOverlappingInstances c many)
        _ ->
          Left (ElaborateUnresolvedConstraint c)

    instanceMatches instHead wanted =
      case I.runInferM (I.unify instHead wanted) of
        Left _ ->
          False
        Right _ ->
          True

    headTypeCon ty =
      case ty of
        TCon name ->
          Just name
        TApp f _ ->
          headTypeCon f
        _ ->
          Nothing

data DictCandidate = DictCandidate Text Type Text

buildDictCandidates :: AliasEnv -> IE.InstanceEnv -> [DictCandidate]
buildDictCandidates aliasEnv instanceEnv =
  builtinCandidates <> userCandidates
  where
    userCandidates =
      [ DictCandidate (IE.instanceInfoClass inst) headTy (Builtins.instanceDictName (IE.instanceInfoClass inst) (IE.instanceInfoHeadCon inst))
      | inst <- Map.elems instanceEnv
      , let headTy = expandAliases aliasEnv (convertType aliasEnv (IE.instanceInfoHead inst))
      ]

    builtinCandidates =
      [ DictCandidate cls (builtinHeadType headCon) dictName
      | ((cls, headCon), dictName) <- Map.toList Builtins.builtinInstanceDicts
      ]

    builtinHeadType headCon =
      case headCon of
        "Result" ->
          TApp (TCon "Result") (TVar "e")
        other ->
          TCon other

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
