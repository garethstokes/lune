module Lune.Type
  ( Type (..)
  , Scheme (..)
  , Subst
  , TypeEnv
  , MonadInfer (..)
  , nullSubst
  , ftvType
  , ftvScheme
  , ftvEnv
  , applySubstType
  , applySubstScheme
  , applySubstEnv
  , composeSubst
  , generalize
  , instantiate
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

data Type
  = TVar Text
  | TCon Text
  | TApp Type Type
  | TArrow Type Type
  | TRecord [(Text, Type)]
  deriving (Eq, Ord, Show)

data Scheme = Forall [Text] Type
  deriving (Eq, Show)

type Subst = Map Text Type

type TypeEnv = Map Text Scheme

class Monad m => MonadInfer m where
  freshTypeVar :: m Type

nullSubst :: Subst
nullSubst = Map.empty

ftvType :: Type -> Set Text
ftvType ty =
  case ty of
    TVar v ->
      Set.singleton v
    TCon _ ->
      Set.empty
    TApp f x ->
      ftvType f <> ftvType x
    TArrow a b ->
      ftvType a <> ftvType b
    TRecord fields ->
      foldMap (ftvType . snd) fields

ftvScheme :: Scheme -> Set Text
ftvScheme (Forall vars ty) =
  ftvType ty Set.\\ Set.fromList vars

ftvEnv :: TypeEnv -> Set Text
ftvEnv env =
  foldMap ftvScheme (Map.elems env)

applySubstType :: Subst -> Type -> Type
applySubstType subst ty =
  case ty of
    TVar v ->
      Map.findWithDefault (TVar v) v subst
    TCon _ ->
      ty
    TApp f x ->
      TApp (applySubstType subst f) (applySubstType subst x)
    TArrow a b ->
      TArrow (applySubstType subst a) (applySubstType subst b)
    TRecord fields ->
      TRecord [(name, applySubstType subst fieldTy) | (name, fieldTy) <- fields]

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme subst (Forall vars ty) =
  Forall vars (applySubstType subst' ty)
  where
    subst' = foldr Map.delete subst vars

applySubstEnv :: Subst -> TypeEnv -> TypeEnv
applySubstEnv subst =
  Map.map (applySubstScheme subst)

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 =
  Map.map (applySubstType s1) s2 <> s1

generalize :: TypeEnv -> Type -> Scheme
generalize env ty =
  Forall vars ty
  where
    vars = Set.toList (ftvType ty Set.\\ ftvEnv env)

instantiate :: MonadInfer m => Scheme -> m Type
instantiate (Forall vars ty) = do
  subst <- buildSubst vars
  pure (applySubstType subst ty)
  where
    buildSubst [] =
      pure Map.empty
    buildSubst (v : vs) = do
      fresh <- freshTypeVar
      rest <- buildSubst vs
      pure (Map.insert v fresh rest)
