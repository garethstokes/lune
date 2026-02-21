{-# LANGUAGE OverloadedStrings #-}

module Lune.Pretty.Type
  ( renderTypeSchemeHover
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Lune.Fmt.Doc as D
import Lune.Type

renderTypeSchemeHover :: Scheme -> Text
renderTypeSchemeHover =
  D.render 80 . prettyScheme

prettyScheme :: Scheme -> D.Doc
prettyScheme (Forall vars constraints ty) =
  forallPart <> constraintsPart <> prettyType ty
  where
    forallPart =
      case vars of
        [] -> mempty
        _ ->
          D.text "forall"
            <> D.space
            <> D.hsep (map D.text vars)
            <> D.text "."
            <> D.space

    constraintsPart =
      case constraints of
        [] -> mempty
        _ ->
          prettyConstraints constraints
            <> D.space
            <> D.text "=>"
            <> D.space

prettyConstraints :: [Constraint] -> D.Doc
prettyConstraints cs =
  case cs of
    [c] -> prettyConstraint c
    _ -> D.parens (commaSep prettyConstraint cs)

prettyConstraint :: Constraint -> D.Doc
prettyConstraint c =
  case constraintArgs c of
    [] -> D.text (constraintClass c)
    args ->
      groupSep (D.text (constraintClass c) : map prettyConstraintArg args)
  where
    prettyConstraintArg t =
      case t of
        TArrow {} -> D.parens (prettyType t)
        TApp {} -> D.parens (prettyType t)
        _ -> prettyType t

prettyType :: Type -> D.Doc
prettyType ty =
  case ty of
    TArrow {} ->
      prettyArrowChain (collectArrows ty)
    _ ->
      prettyApp ty

collectArrows :: Type -> [Type]
collectArrows ty =
  case ty of
    TArrow a b ->
      a : collectArrows b
    _ ->
      [ty]

prettyArrowChain :: [Type] -> D.Doc
prettyArrowChain parts =
  case parts of
    [] -> mempty
    [t] -> prettyType t
    _ ->
      let firstTy = head parts
          lastTy = last parts
          midTys = drop 1 (init parts)
       in D.group $
            prettyAtom firstTy
              <> D.nest 2
                ( mconcat
                    ( map (\t -> D.line <> arrowTo (prettyAtom t)) midTys
                        <> [D.line <> arrowTo (prettyType lastTy)]
                    )
                )
  where
    arrowTo rhs =
      D.text "->" <> D.space <> rhs

prettyApp :: Type -> D.Doc
prettyApp ty =
  case ty of
    TVar v ->
      D.text v
    TCon n ->
      D.text n
    TRecord fields ->
      prettyRecord fields
    TApp {} ->
      let (headTy, args) = unapplyApps ty
       in D.group $
            prettyAtom headTy
              <> D.nest 2 (mconcat [D.line <> prettyAtom a | a <- args])
    TArrow {} ->
      prettyType ty
  where
    unapplyApps =
      go []
      where
        go acc t =
          case t of
            TApp f x ->
              go (x : acc) f
            _ ->
              (t, reverse acc)

prettyAtom :: Type -> D.Doc
prettyAtom t =
  case t of
    TArrow {} -> D.parens (prettyType t)
    TApp {} -> D.parens (prettyType t)
    _ -> prettyApp t

prettyRecord :: [(Text, Type)] -> D.Doc
prettyRecord fields =
  case fields of
    [] -> D.text "{ }"
    _ ->
      D.group $
        D.text "{"
          <> D.nest 2 (D.line <> fieldDocs)
          <> D.line
          <> D.text "}"
  where
    fieldDocs =
      mconcat $
        intersperse (D.text "," <> D.line) (map prettyField fields)

    prettyField (name, ty) =
      D.text name <> D.space <> D.text ":" <> D.space <> prettyType ty

commaSep :: (a -> D.Doc) -> [a] -> D.Doc
commaSep f xs =
  mconcat (intersperse (D.text "," <> D.space) (map f xs))

groupSep :: [D.Doc] -> D.Doc
groupSep docs =
  D.group (mconcat (intersperse D.line docs))
