{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lune.Infer
  ( InferError (..)
  , InferM
  , runInferM
  , inferFail
  , unify
  , varBind
  , inferExpr
  , inferPattern
  , skolemize
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
import Lune.Syntax (Alt (..))
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

    S.IntLit _ ->
      pure (nullSubst, [], TCon "Int")

    S.App f x -> do
      (s1, c1, t1) <- inferExpr env f
      (s2, c2, t2) <- inferExpr (applySubstEnv s1 env) x
      tv <- freshTypeVar
      s3 <- unify (applySubstType s2 t1) (TArrow t2 tv)
      let s = s3 `composeSubst` s2 `composeSubst` s1
          c = applySubstConstraints s (c1 <> c2)
      pure (s, c, applySubstType s tv)

    S.Lam pats body -> do
      (envArgs, argTypes) <- bindLambdaPatterns pats
      (s1, c1, tBody) <- inferExpr (envArgs <> env) body
      let argTypes' = map (applySubstType s1) argTypes
          c = applySubstConstraints s1 c1
      pure (s1, c, foldr TArrow tBody argTypes')

    S.LetIn name bound body -> do
      (s1, c1, t1) <- inferExpr env bound
      let env1 = applySubstEnv s1 env
          scheme = generalize env1 c1 t1
          env2 = Map.insert name scheme env1
      (s2, c2, t2) <- inferExpr env2 body
      let sFinal = s2 `composeSubst` s1
      pure (sFinal, applySubstConstraints sFinal c2, applySubstType sFinal t2)

    S.Case scrut alts -> do
      (s0, c0, tScrut0) <- inferExpr env scrut
      tvRes <- freshTypeVar
      let env0 = applySubstEnv s0 env
      (sFinal, cAlts) <- foldM (inferAlt tScrut0 tvRes env0) (s0, []) alts
      let cFinal = applySubstConstraints sFinal (c0 <> cAlts)
      pure (sFinal, cFinal, applySubstType sFinal tvRes)

    S.DoBlock _ ->
      throwInfer UnexpectedDoBlock

    S.RecordLiteral fields ->
      inferRecordLiteral env fields

    S.RecordUpdate base updates ->
      inferRecordUpdate env base updates

    S.FieldAccess base field ->
      inferFieldAccess env base field
  where
    inferAlt :: Type -> Type -> TypeEnv -> (Subst, [Constraint]) -> Alt -> InferM (Subst, [Constraint])
    inferAlt scrutTy resTy envBase (sAcc, cAcc) (Alt pat body) = do
      let env1 = applySubstEnv sAcc envBase
          scrutTy' = applySubstType sAcc scrutTy
          resTy' = applySubstType sAcc resTy
      (patEnv, cPat, patTy) <- inferPattern env1 pat
      s1 <- unify scrutTy' patTy
      let env2 = applySubstEnv s1 (patEnv <> env1)
          c1 = applySubstConstraints s1 (cAcc <> cPat)
      (sBody, cBody, bodyTy) <- inferExpr env2 body
      let s' = sBody `composeSubst` s1 `composeSubst` sAcc
          c2 = applySubstConstraints s' (c1 <> cBody)
      sRes <- unify (applySubstType s' resTy') (applySubstType s' bodyTy)
      let sFinal = sRes `composeSubst` s'
      pure (sFinal, applySubstConstraints sFinal c2)

bindLambdaPatterns :: [S.Pattern] -> InferM (TypeEnv, [Type])
bindLambdaPatterns pats =
  foldM step (Map.empty, []) pats
  where
    step (envAcc, tys) pat =
      case pat of
        S.PVar name -> do
          tv <- freshTypeVar
          let envAcc' = Map.insert name (Forall [] [] tv) envAcc
          pure (envAcc', tys <> [tv])
        S.PWildcard -> do
          tv <- freshTypeVar
          pure (envAcc, tys <> [tv])
        _ ->
          throwInfer (UnsupportedPattern pat)

inferPattern :: TypeEnv -> S.Pattern -> InferM (TypeEnv, [Constraint], Type)
inferPattern env pat =
  case pat of
    S.PVar name -> do
      tv <- freshTypeVar
      pure (Map.singleton name (Forall [] [] tv), [], tv)

    S.PWildcard -> do
      tv <- freshTypeVar
      pure (Map.empty, [], tv)

    S.PCon conName subpats -> do
      scheme <-
        case Map.lookup conName env of
          Nothing -> throwInfer (UnboundVariable conName)
          Just s -> pure s
      (conConstraints, conTy) <- instantiate scheme
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

inferRecordLiteral :: TypeEnv -> [(Text, S.Expr)] -> InferM (Subst, [Constraint], Type)
inferRecordLiteral env fields = do
  (s, c, fieldTypes) <- foldM inferField (nullSubst, [], []) fields
  let recordTy = TRecord (sortOn fst fieldTypes)
  pure (s, applySubstConstraints s c, applySubstType s recordTy)
  where
    inferField (sAcc, cAcc, acc) (name, e) = do
      (s1, c1, t1) <- inferExpr (applySubstEnv sAcc env) e
      let sFinal = s1 `composeSubst` sAcc
          cFinal = applySubstConstraints sFinal (cAcc <> c1)
      pure (sFinal, cFinal, acc <> [(name, applySubstType sFinal t1)])

inferFieldAccess :: TypeEnv -> S.Expr -> Text -> InferM (Subst, [Constraint], Type)
inferFieldAccess env base field = do
  (s1, c1, baseTy) <- inferExpr env base
  case applySubstType s1 baseTy of
    TRecord fields ->
      case lookup field fields of
        Nothing -> throwInfer (MissingRecordField field)
        Just fieldTy -> pure (s1, applySubstConstraints s1 c1, fieldTy)
    other ->
      throwInfer (NotARecord other)

inferRecordUpdate :: TypeEnv -> S.Expr -> [(Text, S.Expr)] -> InferM (Subst, [Constraint], Type)
inferRecordUpdate env base updates = do
  (sBase, cBase, baseTy0) <- inferExpr env base
  case applySubstType sBase baseTy0 of
    TRecord fields0 -> do
      let env1 = applySubstEnv sBase env
      (sFinal, cFinal) <- foldM (checkUpdate env1 fields0) (sBase, cBase) updates
      pure (sFinal, applySubstConstraints sFinal cFinal, applySubstType sFinal (TRecord fields0))
    other ->
      throwInfer (NotARecord other)
  where
    checkUpdate env1 fields (sAcc, cAcc) (name, expr) =
      case lookup name fields of
        Nothing ->
          throwInfer (MissingRecordField name)
        Just fieldTy -> do
          (s1, c1, exprTy) <- inferExpr (applySubstEnv sAcc env1) expr
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
