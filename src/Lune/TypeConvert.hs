module Lune.TypeConvert
  ( AliasEnv
  , buildAliasEnv
  , convertType
  , convertConstraint
  , convertQualType
  , schemeFromQualType
  , expandAliases
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Lune.Syntax as S
import Lune.Type

type AliasEnv = Map Text ([Text], Type)

buildAliasEnv :: [S.Decl] -> AliasEnv
buildAliasEnv decls =
  Map.fromList
    [ (name, (vars, convertType Map.empty body))
    | S.DeclTypeAlias _ name vars body <- decls
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
      TRecord (sortOn fst [(name, convertType aliasEnv t) | (name, t, _) <- fields])

convertConstraint :: AliasEnv -> S.Constraint -> Constraint
convertConstraint aliasEnv constraint =
  Constraint
    { constraintClass = S.constraintClass constraint
    , constraintArgs = map (expandAliases aliasEnv . convertType aliasEnv) (S.constraintArgs constraint)
    }

convertQualType :: AliasEnv -> S.QualType -> ([Constraint], Type)
convertQualType aliasEnv (S.QualType constraints ty) =
  (map (convertConstraint aliasEnv) constraints, expandAliases aliasEnv (convertType aliasEnv ty))

schemeFromQualType :: AliasEnv -> S.QualType -> Scheme
schemeFromQualType aliasEnv qualTy =
  Forall vars constraints ty
  where
    (constraints, ty) = convertQualType aliasEnv qualTy
    vars = Set.toList (ftvConstraints constraints <> ftvType ty)

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
                          -- Don't expand opaque aliases (e.g. type Api e a = Api#)
                          -- where the RHS discards type parameters, as this would
                          -- sever the type variable chain needed for unification.
                          let usedParams = ftvType body
                              allUsed = all (\p -> p `Set.member` usedParams) params
                           in if allUsed
                                then
                                  let subst = Map.fromList (zip params (map (go seen) args))
                                   in go (Set.insert name seen) (applySubstType subst body)
                                else
                                  rebuildApps (TCon name) (map (go seen) args)
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

