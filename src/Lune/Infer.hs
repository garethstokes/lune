{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lune.Infer
  ( InferError (..)
  , InferM
  , runInferM
  , inferFail
  , unify
  , varBind
  , inferExpr
  , inferLExpr
  , inferPattern
  , inferLPattern
  , skolemize
  , skolemizeScheme
  ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import qualified Control.Monad.Trans.State.Strict as State
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Lune.Syntax (Alt (..), Located, unLoc)
import qualified Lune.Syntax as S
import Lune.Type

data InferError
  = UnboundVariable Text
  | CannotUnify Type Type
  | InfiniteType Text Type
  | ExpectedFunctionType Type
  | NotARecord Type
  | MissingRecordField Text
  | ConstructorArityMismatch Text Int Int
  | UnexpectedDoBlock
  | EffectfulInterpolation Type
  | UnsupportedPattern S.Pattern
  deriving (Show)

data InferState = InferState
  { inferNextId :: !Int
  }

newtype InferM a = InferM (StateT InferState (Either InferError) a)
  deriving (Functor, Applicative, Monad)

runInferM :: InferM a -> Either InferError a
runInferM (InferM m) =
  evalStateT m (InferState 0)

throwInfer :: InferError -> InferM a
throwInfer err =
  InferM (lift (Left err))

inferFail :: InferError -> InferM a
inferFail = throwInfer

instance MonadInfer InferM where
  freshTypeVar = do
    n <- InferM (State.gets inferNextId)
    InferM (State.modify' (\st -> st {inferNextId = inferNextId st + 1}))
    pure (TVar (T.pack ("t" <> show n)))

freshSkolem :: InferM Type
freshSkolem = do
  n <- InferM (State.gets inferNextId)
  InferM (State.modify' (\st -> st {inferNextId = inferNextId st + 1}))
  pure (TCon (T.pack ("$sk" <> show n)))

unify :: Type -> Type -> InferM Subst
unify t1 t2 =
  case (t1, t2) of
    (TArrow a b, TArrow a' b') -> do
      s1 <- unify a a'
      s2 <- unify (applySubstType s1 b) (applySubstType s1 b')
      pure (s2 `composeSubst` s1)

    (TApp f x, TApp f' x') -> do
      s1 <- unify f f'
      s2 <- unify (applySubstType s1 x) (applySubstType s1 x')
      pure (s2 `composeSubst` s1)

    (TRecord fs, TRecord gs) ->
      unifyRecords fs gs

    (TVar v, t) ->
      varBind v t

    (t, TVar v) ->
      varBind v t

    (TCon a, TCon b)
      | a == b ->
          pure nullSubst

    _ ->
      throwInfer (CannotUnify t1 t2)

unifyRecords :: [(Text, Type)] -> [(Text, Type)] -> InferM Subst
unifyRecords fields1 fields2 = do
  let f1 = sortOn fst fields1
      f2 = sortOn fst fields2
  if map fst f1 /= map fst f2
    then throwInfer (CannotUnify (TRecord f1) (TRecord f2))
    else unifyFieldTypes (map snd f1) (map snd f2)
  where
    unifyFieldTypes [] [] =
      pure nullSubst
    unifyFieldTypes (t : ts) (u : us) = do
      s1 <- unify t u
      s2 <- unifyFieldTypes (map (applySubstType s1) ts) (map (applySubstType s1) us)
      pure (s2 `composeSubst` s1)
    unifyFieldTypes _ _ =
      throwInfer (CannotUnify (TRecord fields1) (TRecord fields2))

varBind :: Text -> Type -> InferM Subst
varBind v t
  | t == TVar v =
      pure nullSubst
  | v `elem` ftvType t =
      throwInfer (InfiniteType v t)
  | otherwise =
      pure (Map.singleton v t)

-- | Infer type of a Located Expr by unwrapping
inferLExpr :: TypeEnv -> Located S.Expr -> InferM (Subst, [Constraint], Type)
inferLExpr env lexpr = inferExpr env (unLoc lexpr)

inferExpr :: TypeEnv -> S.Expr -> InferM (Subst, [Constraint], Type)
inferExpr env expr =
  case expr of
    S.Var name ->
      case Map.lookup name env of
        Nothing ->
          throwInfer (UnboundVariable name)
        Just scheme -> do
          (constraints, ty) <- instantiate scheme
          pure (nullSubst, constraints, ty)

    S.StringLit _ ->
      pure (nullSubst, [], TCon "String")

    S.TemplateLit _ parts -> do
      (s, cs) <- inferTemplateParts env parts
      pure (s, applySubstConstraints s cs, TCon "Template")

    S.IntLit _ ->
      pure (nullSubst, [], TCon "Int")

    S.FloatLit _ ->
      pure (nullSubst, [], TCon "Float")

    S.CharLit _ ->
      pure (nullSubst, [], TCon "Char")

    S.App lf lx -> do
      (s1, c1, t1) <- inferLExpr env lf
      (s2, c2, t2) <- inferLExpr (applySubstEnv s1 env) lx
      tv <- freshTypeVar
      s3 <- unify (applySubstType s2 t1) (TArrow t2 tv)
      let s = s3 `composeSubst` s2 `composeSubst` s1
          c = applySubstConstraints s (c1 <> c2)
      pure (s, c, applySubstType s tv)

    S.Lam lpats lbody -> do
      (envArgs, argTypes) <- bindLambdaPatterns lpats
      (s1, c1, tBody) <- inferLExpr (envArgs <> env) lbody
      let argTypes' = map (applySubstType s1) argTypes
          c = applySubstConstraints s1 c1
      pure (s1, c, foldr TArrow tBody argTypes')

    S.LetIn name lbound lbody -> do
      (s1, c1, t1) <- inferLExpr env lbound
      let env1 = applySubstEnv s1 env
          scheme = generalize env1 c1 t1
          env2 = Map.insert name scheme env1
      (s2, c2, t2) <- inferLExpr env2 lbody
      let sFinal = s2 `composeSubst` s1
      pure (sFinal, applySubstConstraints sFinal c2, applySubstType sFinal t2)

    S.Case lscrut lalts -> do
      (s0, c0, tScrut0) <- inferLExpr env lscrut
      tvRes <- freshTypeVar
      let env0 = applySubstEnv s0 env
      (sFinal, cAlts) <- foldM (inferLAlt tScrut0 tvRes env0) (s0, []) lalts
      let cFinal = applySubstConstraints sFinal (c0 <> cAlts)
      pure (sFinal, cFinal, applySubstType sFinal tvRes)

    S.DoBlock _ ->
      throwInfer UnexpectedDoBlock

    S.RecordLiteral fields ->
      inferRecordLiteral env fields

    S.RecordUpdate lbase updates ->
      inferRecordUpdate env lbase updates

    S.FieldAccess lbase field ->
      inferFieldAccess env lbase field
  where
    inferTemplateParts :: TypeEnv -> [S.TemplatePart] -> InferM (Subst, [Constraint])
    inferTemplateParts env0 =
      go (nullSubst, [])
      where
        go acc [] =
          pure acc
        go (sAcc, cAcc) (part : rest) =
          case part of
            S.TemplateText _ ->
              go (sAcc, cAcc) rest
            S.TemplateHole le -> do
              let env1 = applySubstEnv sAcc env0
              (s1, c1, t1) <- inferLExpr env1 le
              let s = s1 `composeSubst` sAcc
                  cAcc' = applySubstConstraints s1 cAcc
                  t1' = applySubstType s1 t1
                  c = cAcc' <> c1 <> [Constraint "ToTemplate" [t1']]
              -- Best-effort purity rule: don't allow effect-typed expressions in
              -- interpolation holes (SPEC E). This is a conservative check
              -- until an effect system exists.
              if isEffectType t1'
                then throwInfer (EffectfulInterpolation t1')
                else pure ()
              go (s, c) rest
        isEffectType ty =
          case headTypeCon ty of
            Just "IO" -> True
            Just "Task" -> True
            Just "STM" -> True
            _ -> False

        headTypeCon t =
          case t of
            TCon name ->
              Just name
            TApp f _ ->
              headTypeCon f
            _ ->
              Nothing

    inferLAlt :: Type -> Type -> TypeEnv -> (Subst, [Constraint]) -> Located Alt -> InferM (Subst, [Constraint])
    inferLAlt scrutTy resTy envBase acc lalt = inferAlt scrutTy resTy envBase acc (unLoc lalt)

    inferAlt :: Type -> Type -> TypeEnv -> (Subst, [Constraint]) -> Alt -> InferM (Subst, [Constraint])
    inferAlt scrutTy resTy envBase (sAcc, cAcc) (Alt lpat lbody) = do
      let env1 = applySubstEnv sAcc envBase
          scrutTy' = applySubstType sAcc scrutTy
          resTy' = applySubstType sAcc resTy
      (patEnv, cPat, patTy) <- inferLPattern env1 lpat
      s1 <- unify scrutTy' patTy
      let env2 = applySubstEnv s1 (patEnv <> env1)
          c1 = applySubstConstraints s1 (cAcc <> cPat)
      (sBody, cBody, bodyTy) <- inferLExpr env2 lbody
      let s' = sBody `composeSubst` s1 `composeSubst` sAcc
          c2 = applySubstConstraints s' (c1 <> cBody)
      sRes <- unify (applySubstType s' resTy') (applySubstType s' bodyTy)
      let sFinal = sRes `composeSubst` s'
      pure (sFinal, applySubstConstraints sFinal c2)

-- | Bind lambda patterns (Located version)
bindLambdaPatterns :: [Located S.Pattern] -> InferM (TypeEnv, [Type])
bindLambdaPatterns lpats =
  foldM step (Map.empty, []) lpats
  where
    step (envAcc, tys) lpat =
      case unLoc lpat of
        S.PVar name -> do
          tv <- freshTypeVar
          let envAcc' = Map.insert name (Forall [] [] tv) envAcc
          pure (envAcc', tys <> [tv])
        S.PWildcard -> do
          tv <- freshTypeVar
          pure (envAcc, tys <> [tv])
        pat ->
          throwInfer (UnsupportedPattern pat)

-- | Infer pattern type (Located version)
inferLPattern :: TypeEnv -> Located S.Pattern -> InferM (TypeEnv, [Constraint], Type)
inferLPattern env lpat = inferPattern env (unLoc lpat)

inferPattern :: TypeEnv -> S.Pattern -> InferM (TypeEnv, [Constraint], Type)
inferPattern env pat =
  case pat of
    S.PVar name -> do
      tv <- freshTypeVar
      pure (Map.singleton name (Forall [] [] tv), [], tv)

    S.PWildcard -> do
      tv <- freshTypeVar
      pure (Map.empty, [], tv)

    S.PString _ ->
      pure (Map.empty, [], TCon "String")

    S.PCon conName lsubpats -> do
      scheme <-
        case Map.lookup conName env of
          Nothing -> throwInfer (UnboundVariable conName)
          Just s -> pure s
      (conConstraints, conTy) <- instantiate scheme
      let subpats = map unLoc lsubpats
      (argTys, resTy) <- peelArrows conName (length subpats) conTy
      (sFinal, cFinal, envFinal, resFinal) <- foldM (inferSubPattern env) (nullSubst, conConstraints, Map.empty, resTy) (zip subpats argTys)
      pure (applySubstEnv sFinal envFinal, applySubstConstraints sFinal cFinal, applySubstType sFinal resFinal)

peelArrows :: Text -> Int -> Type -> InferM ([Type], Type)
peelArrows _ 0 ty =
  pure ([], ty)
peelArrows name n ty =
  case ty of
    TArrow a b -> do
      (args, res) <- peelArrows name (n - 1) b
      pure (a : args, res)
    _ ->
      throwInfer (ConstructorArityMismatch name n 0)

inferSubPattern :: TypeEnv -> (Subst, [Constraint], TypeEnv, Type) -> (S.Pattern, Type) -> InferM (Subst, [Constraint], TypeEnv, Type)
inferSubPattern env (sAcc, cAcc, envAcc, resTy) (subPat, expectedTy) = do
  let expectedTy' = applySubstType sAcc expectedTy
  (subEnv, cSub, subTy) <- inferPattern (applySubstEnv sAcc env) subPat
  s1 <- unify (applySubstType sAcc subTy) expectedTy'
  let sFinal = s1 `composeSubst` sAcc
      cFinal = applySubstConstraints sFinal (cAcc <> cSub)
      envAcc' = applySubstEnv s1 (envAcc <> subEnv)
      resTy' = applySubstType s1 resTy
  pure (sFinal, cFinal, envAcc', resTy')

inferRecordLiteral :: TypeEnv -> [(Text, Located S.Expr)] -> InferM (Subst, [Constraint], Type)
inferRecordLiteral env fields = do
  (s, c, fieldTypes) <- foldM inferField (nullSubst, [], []) fields
  let recordTy = TRecord (sortOn fst fieldTypes)
  pure (s, applySubstConstraints s c, applySubstType s recordTy)
  where
    inferField (sAcc, cAcc, acc) (name, le) = do
      (s1, c1, t1) <- inferLExpr (applySubstEnv sAcc env) le
      let sFinal = s1 `composeSubst` sAcc
          cFinal = applySubstConstraints sFinal (cAcc <> c1)
      pure (sFinal, cFinal, acc <> [(name, applySubstType sFinal t1)])

inferFieldAccess :: TypeEnv -> Located S.Expr -> Text -> InferM (Subst, [Constraint], Type)
inferFieldAccess env lbase field = do
  (s1, c1, baseTy) <- inferLExpr env lbase
  case applySubstType s1 baseTy of
    TRecord fields ->
      case lookup field fields of
        Nothing -> throwInfer (MissingRecordField field)
        Just fieldTy -> pure (s1, applySubstConstraints s1 c1, fieldTy)
    TVar _ -> do
      -- Base type is not yet resolved (e.g. lambda parameter whose type
      -- will be constrained by the enclosing application). Return a fresh
      -- type variable; field validity is checked at runtime.
      tv <- freshTypeVar
      pure (s1, applySubstConstraints s1 c1, tv)
    other ->
      throwInfer (NotARecord other)

inferRecordUpdate :: TypeEnv -> Located S.Expr -> [(Text, Located S.Expr)] -> InferM (Subst, [Constraint], Type)
inferRecordUpdate env lbase updates = do
  (sBase, cBase, baseTy0) <- inferLExpr env lbase
  case applySubstType sBase baseTy0 of
    TRecord fields0 -> do
      let env1 = applySubstEnv sBase env
      (sFinal, cFinal) <- foldM (checkUpdate env1 fields0) (sBase, cBase) updates
      pure (sFinal, applySubstConstraints sFinal cFinal, applySubstType sFinal (TRecord fields0))
    other ->
      throwInfer (NotARecord other)
  where
    checkUpdate env1 fields (sAcc, cAcc) (name, lexpr) =
      case lookup name fields of
        Nothing ->
          throwInfer (MissingRecordField name)
        Just fieldTy -> do
          (s1, c1, exprTy) <- inferLExpr (applySubstEnv sAcc env1) lexpr
          let expectedTy = applySubstType sAcc fieldTy
          s2 <- unify (applySubstType s1 expectedTy) exprTy
          let sFinal = s2 `composeSubst` s1 `composeSubst` sAcc
              cFinal = applySubstConstraints sFinal (cAcc <> c1)
          pure (sFinal, cFinal)

skolemize :: Scheme -> InferM Type
skolemize (Forall vars _ ty) = do
  subst <- foldM addSkolem Map.empty vars
  pure (applySubstType subst ty)
  where
    addSkolem subst v = do
      sk <- freshSkolem
      pure (Map.insert v sk subst)

skolemizeScheme :: Scheme -> InferM ([Constraint], Type)
skolemizeScheme (Forall vars constraints ty) = do
  subst <- foldM addSkolem Map.empty vars
  pure (applySubstConstraints subst constraints, applySubstType subst ty)
  where
    addSkolem subst v = do
      sk <- freshSkolem
      pure (Map.insert v sk subst)
