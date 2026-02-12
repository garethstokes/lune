{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lune.Kind
  ( Kind (..)
  , KindEnv
  , ClassEnv
  , KindError (..)
  , builtinKindEnv
  , builtinClassEnv
  , kindEnvFromDecls
  , kindCheckQualType
  ) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, evalState, gets, modify')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Syntax as S

data Kind
  = KType
  | KArr Kind Kind
  | KVar !Int
  deriving (Eq, Ord, Show)

type KindEnv = Map Text Kind

type ClassEnv = Map Text [Kind]

data KindError
  = TypeClassArityMismatch Text Int Int
  | UnknownTypeClass Text
  | KindMismatch Kind Kind
  | InfiniteKind Int Kind
  | BadConstraintArgKind Text S.Type Kind Kind

instance Show KindError where
  show err =
    case err of
      TypeClassArityMismatch cls expected got ->
        T.unpack cls
          <> " expects "
          <> show expected
          <> " type argument(s)"
          <> " (got "
          <> show got
          <> ")"
      UnknownTypeClass cls ->
        "Unknown typeclass: " <> T.unpack cls
      KindMismatch k1 k2 ->
        "Kind mismatch: expected " <> T.unpack (renderKind k1) <> ", got " <> T.unpack (renderKind k2)
      InfiniteKind v k ->
        "Infinite kind: k" <> show v <> " occurs in " <> T.unpack (renderKind k)
      BadConstraintArgKind cls arg expected got ->
        T.unpack cls
          <> " expects a type constructor of kind "
          <> T.unpack (renderKind expected)
          <> ", but "
          <> T.unpack (renderType arg)
          <> " has kind "
          <> T.unpack (renderKind got)

builtinKindEnv :: KindEnv
builtinKindEnv =
  Map.fromList
    [ ("Int", KType)
    , ("Bool", KType)
    , ("String", KType)
    , ("Unit", KType)
    , ("Char", KType)
    , ("Float", KType)
    , ("List", KArr KType KType)
    , ("Maybe", KArr KType KType)
    , ("IO", KArr KType KType)
    , ("STM", KArr KType KType)
    , ("TVar", KArr KType KType)
    , ("Fiber", KArr KType KType)
    , ("Result", KArr KType (KArr KType KType))
    , ("Pair", KArr KType (KArr KType KType))
    , ("Validation", KArr KType (KArr KType KType))
    , ("Triple", kindFromArity 3)
    , ("Quad", kindFromArity 4)
    , ("Quint", kindFromArity 5)
    , ("Atomic", KArr KType KType)
    , ("Shared", KArr KType KType)
    , ("Task", KArr KType KType)
    ]

builtinClassEnv :: ClassEnv
builtinClassEnv =
  Map.fromList
    [ ("Functor", [KArr KType KType])
    , ("Applicative", [KArr KType KType])
    , ("Monad", [KArr KType KType])
    ]

kindEnvFromDecls :: [S.Decl] -> KindEnv
kindEnvFromDecls decls =
  builtinKindEnv <> declared
  where
    declared =
      Map.fromList
        [ (name, kindFromArity (length vars))
        | decl <- decls
        , (name, vars) <-
            case decl of
              S.DeclType name vars _ -> [(name, vars)]
              S.DeclTypeAlias _ name vars _ -> [(name, vars)]
              S.DeclNewtype name vars _ _ -> [(name, vars)]
              _ -> []
        ]

kindFromArity :: Int -> Kind
kindFromArity n =
  foldr KArr KType (replicate n KType)

data KindState = KindState
  { ksNextVar :: !Int
  , ksSubst :: !(Map Int Kind)
  , ksTyVarKinds :: !(Map Text Kind)
  }

type KindM = ExceptT KindError (State KindState)

runKindM :: KindM a -> Either KindError a
runKindM m =
  evalState (runExceptT m) (KindState 0 Map.empty Map.empty)

freshKindVar :: KindM Kind
freshKindVar = do
  n <- lift (gets ksNextVar)
  lift (modify' (\st -> st {ksNextVar = n + 1}))
  pure (KVar n)

lookupTyVarKind :: Text -> KindM Kind
lookupTyVarKind name = do
  env <- lift (gets ksTyVarKinds)
  case Map.lookup name env of
    Just k ->
      pure k
    Nothing -> do
      k <- freshKindVar
      lift (modify' (\st -> st {ksTyVarKinds = Map.insert name k (ksTyVarKinds st)}))
      pure k

applySubstKind :: Map Int Kind -> Kind -> Kind
applySubstKind subst kind =
  case kind of
    KType ->
      KType
    KArr a b ->
      KArr (applySubstKind subst a) (applySubstKind subst b)
    KVar v ->
      case Map.lookup v subst of
        Nothing -> KVar v
        Just k -> applySubstKind subst k

composeKindSubst :: Map Int Kind -> Map Int Kind -> Map Int Kind
composeKindSubst s1 s2 =
  Map.map (applySubstKind s1) s2 <> s1

occursInKind :: Int -> Kind -> Bool
occursInKind v kind =
  case kind of
    KType ->
      False
    KArr a b ->
      occursInKind v a || occursInKind v b
    KVar v' ->
      v == v'

bindKindVar :: Int -> Kind -> KindM ()
bindKindVar v kind
  | kind == KVar v =
      pure ()
  | occursInKind v kind =
      throwE (InfiniteKind v kind)
  | otherwise =
      lift (modify' (\st -> st {ksSubst = composeKindSubst (Map.singleton v kind) (ksSubst st)}))

unifyKinds :: Kind -> Kind -> KindM ()
unifyKinds k1 k2 = do
  subst <- lift (gets ksSubst)
  let k1' = applySubstKind subst k1
      k2' = applySubstKind subst k2
  case (k1', k2') of
    (KType, KType) ->
      pure ()
    (KArr a b, KArr a' b') -> do
      unifyKinds a a'
      unifyKinds b b'
    (KVar v, k) ->
      bindKindVar v k
    (k, KVar v) ->
      bindKindVar v k
    _ ->
      throwE (KindMismatch k1' k2')

inferKind :: KindEnv -> S.Type -> KindM Kind
inferKind kindEnv ty =
  case ty of
    S.TypeCon name ->
      pure (Map.findWithDefault KType name kindEnv)
    S.TypeVar name ->
      lookupTyVarKind name
    S.TypeApp f x -> do
      kf <- inferKind kindEnv f
      kx <- inferKind kindEnv x
      kr <- freshKindVar
      unifyKinds kf (KArr kx kr)
      pure kr
    S.TypeArrow a b -> do
      ka <- inferKind kindEnv a
      kb <- inferKind kindEnv b
      unifyKinds ka KType
      unifyKinds kb KType
      pure KType
    S.TypeRecord fields -> do
      forM_ fields $ \(_, fieldTy, _) -> do
        k <- inferKind kindEnv fieldTy
        unifyKinds k KType
      pure KType

checkConstraint :: KindEnv -> ClassEnv -> S.Constraint -> KindM ()
checkConstraint kindEnv classEnv constraint = do
  expectedKinds <-
    case Map.lookup (S.constraintClass constraint) classEnv of
      Nothing ->
        throwE (UnknownTypeClass (S.constraintClass constraint))
      Just ks ->
        pure ks

  let args = S.constraintArgs constraint
  when (length args /= length expectedKinds) $
    throwE (TypeClassArityMismatch (S.constraintClass constraint) (length expectedKinds) (length args))

  forM_ (zip args expectedKinds) $ \(argTy, expectedKind) -> do
    argKind <- inferKind kindEnv argTy
    unifyKinds argKind expectedKind `catchE` \_ -> do
      subst <- lift (gets ksSubst)
      throwE
        ( BadConstraintArgKind
            (S.constraintClass constraint)
            argTy
            (applySubstKind subst expectedKind)
            (applySubstKind subst argKind)
        )

kindCheckQualType :: KindEnv -> ClassEnv -> S.QualType -> Either KindError ()
kindCheckQualType kindEnv classEnv (S.QualType constraints ty) =
  runKindM $ do
    mapM_ (checkConstraint kindEnv classEnv) constraints
    k <- inferKind kindEnv ty
    unifyKinds k KType

renderKind :: Kind -> Text
renderKind kind =
  case kind of
    KType ->
      "Type"
    KVar n ->
      "k" <> T.pack (show n)
    KArr a b ->
      renderKindAtom a <> " -> " <> renderKind b
  where
    renderKindAtom k =
      case k of
        KArr {} -> "(" <> renderKind k <> ")"
        _ -> renderKind k

renderType :: S.Type -> Text
renderType ty =
  case ty of
    S.TypeVar v ->
      v
    S.TypeCon n ->
      n
    S.TypeApp f x ->
      renderType f <> " " <> renderTypeAtom x
    S.TypeArrow a b ->
      renderTypeAtom a <> " -> " <> renderType b
    S.TypeRecord fields ->
      "{ " <> T.intercalate ", " [name <> " : " <> renderType t | (name, t, _) <- fields] <> " }"
  where
    renderTypeAtom t =
      case t of
        S.TypeArrow {} -> "(" <> renderType t <> ")"
        S.TypeApp {} -> "(" <> renderType t <> ")"
        _ -> renderType t
