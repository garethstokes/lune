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

instanceDictName :: Text -> Text -> Text
instanceDictName cls headCon =
  "$dict" <> cls <> "_" <> headCon

type InstanceDicts = Map (Text, Text) Text

buildInstanceDicts :: IE.InstanceEnv -> InstanceDicts
buildInstanceDicts instEnv =
  fromInstances <> builtinInstanceDicts
  where
    fromInstances =
      Map.fromList
        [ ((IE.instanceInfoClass inst, IE.instanceInfoHeadCon inst), instanceDictName (IE.instanceInfoClass inst) (IE.instanceInfoHeadCon inst))
        | inst <- Map.elems instEnv
        ]

    builtinInstanceDicts =
      Map.fromList
        [ (("Monad", "IO"), instanceDictName "Monad" "IO")
        , (("Monad", "Result"), instanceDictName "Monad" "Result")
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
      env0 = builtinEnv <> ctorEnv <> methodEnv <> tmValueSchemes tm
      instanceDicts = buildInstanceDicts (tmInstanceEnv tm)

  dictDecls <- mapM (elabInstanceDict methodIndex instanceDicts env0) (Map.elems (tmInstanceEnv tm))
  valueDecls <- mapM (elabValueDecl methodIndex instanceDicts env0) (valueDeclsOf decls)

  pure
    CoreModule
      { coreName = S.modName (tmModule tm)
      , coreDecls = dictDecls <> valueDecls
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
      env0 = builtinEnv <> methodEnv <> env
  case I.runInferM (inferCoreExpr methodIndex env0 expr) of
    Left err ->
      Left (ElaborateInferError "<expr>" err)
    Right (s, _cs, _ty, core0) ->
      resolveInstanceDicts s instanceDicts core0

elabInstanceDict :: MethodIndex -> InstanceDicts -> TypeEnv -> IE.InstanceInfo -> Either ElaborateError CoreDecl
elabInstanceDict methodIndex instanceDicts env0 inst = do
  let dictName = instanceDictName (IE.instanceInfoClass inst) (IE.instanceInfoHeadCon inst)
  fields <- mapM elabMethod (Map.toList (IE.instanceInfoMethods inst))
  pure (CoreDecl dictName (CRecord fields))
  where
    elabMethod (methodName, methodExpr) =
      case I.runInferM (inferCoreExpr methodIndex env0 methodExpr) of
        Left err ->
          Left (ElaborateInferError (IE.instanceInfoClass inst <> " " <> IE.instanceInfoHeadCon inst <> "." <> methodName) err)
        Right (s, _cs, _ty, core0) -> do
          core <- resolveInstanceDicts s instanceDicts core0
          Right (methodName, core)

elabValueDecl :: MethodIndex -> InstanceDicts -> TypeEnv -> S.Decl -> Either ElaborateError CoreDecl
elabValueDecl methodIndex instanceDicts env0 decl =
  case decl of
    S.DeclValue name args body -> do
      scheme <-
        case Map.lookup name env0 of
          Just s -> Right s
          Nothing -> Left (ElaborateMissingScheme name)

      case I.runInferM (inferTopLevel methodIndex env0 scheme args body) of
        Left err ->
          Left (ElaborateInferError name err)
        Right (s, core0) -> do
          core <- resolveInstanceDicts s instanceDicts core0
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

inferTopLevel :: MethodIndex -> TypeEnv -> Scheme -> [S.Pattern] -> S.Expr -> I.InferM (Subst, CoreExpr)
inferTopLevel methodIndex env0 scheme args body = do
  (topConstraints, topTy) <- instantiate scheme
  let dictParams = zipWith (dictParamPattern "dict") [0 :: Int ..] topConstraints

  (argTys, resTy) <- peelArrows (length args) topTy
  argEnv <- bindTopLevelArgs args argTys
  (sBody, _cBody, bodyTy, coreBody0) <- inferCoreExpr methodIndex (argEnv <> env0) body
  sRes <- I.unify (applySubstType sBody bodyTy) (applySubstType sBody resTy)
  let sFinal = sRes `composeSubst` sBody
      givenMapSub = Map.fromList (zip (map (applySubstConstraint sFinal) topConstraints) (map patName dictParams))
      coreBody1 = replaceGivenDicts sFinal givenMapSub coreBody0

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
          givenMap = Map.fromList (zip (map (applySubstConstraint s1) c1) (map patName dictParams))
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

replaceGivenDicts :: Subst -> Map Constraint Text -> CoreExpr -> CoreExpr
replaceGivenDicts subst given =
  go
  where
    go expr =
      case expr of
        CDictWanted c ->
          case Map.lookup (applySubstConstraint subst c) given of
            Just dictName ->
              CVar dictName
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

resolveInstanceDicts :: Subst -> InstanceDicts -> CoreExpr -> Either ElaborateError CoreExpr
resolveInstanceDicts subst instanceDicts =
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
            Just headCon ->
              case Map.lookup (constraintClass c, headCon) instanceDicts of
                Just dictName ->
                  Right (CVar dictName)
                Nothing ->
                  Left (ElaborateUnresolvedConstraint c)
            Nothing ->
              Left (ElaborateUnresolvedConstraint c)
        _ ->
          Left (ElaborateUnresolvedConstraint c)

    headTypeCon ty =
      case ty of
        TCon name ->
          Just name
        TApp f _ ->
          headTypeCon f
        _ ->
          Nothing

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
    , ("and", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool"))))
    , ("geInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("leInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("parseInt", Forall [] [] (TArrow (TCon "String") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Int"))))
    , ("putStrLn", Forall [] [] (TArrow (TCon "String") (TApp (TCon "IO") (TCon "Unit"))))
    , ("runMain", Forall [] [] (TArrow (TApp (TCon "IO") (TCon "Unit")) (TCon "Int")))
    , ("unit", Forall [] [] (TCon "Unit"))
    , ("True", Forall [] [] (TCon "Bool"))
    , ("False", Forall [] [] (TCon "Bool"))
    , ("Nil", Forall ["a"] [] (TApp (TCon "List") (TVar "a")))
    , ("Cons", Forall ["a"] [] (TArrow (TVar "a") (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))))
    , ("Ok", Forall ["e", "a"] [] (TArrow (TVar "a") (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))))
    , ("Err", Forall ["e", "a"] [] (TArrow (TVar "e") (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))))
    ]
