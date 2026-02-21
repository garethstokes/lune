{-# LANGUAGE OverloadedStrings #-}

module Lune.Fmt.Doc
  ( Doc
  , empty
  , text
  , line
  , lineBreak
  , hardLine
  , space
  , softSpace
  , (<+>)
  , nest
  , group
  , hsep
  , vsep
  , sep
  , parens
  , brackets
  , braces
  , render
  ) where

import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

data Doc
  = Empty
  | Text T.Text
  | Line
  | LineBreak
  | HardLine
  | SoftSpace
  | Concat Doc Doc
  | Nest Int Doc
  | Group Doc
  deriving (Eq, Show)

instance Semigroup Doc where
  (<>) = Concat

instance Monoid Doc where
  mempty = Empty

empty :: Doc
empty = Empty

text :: T.Text -> Doc
text t =
  if T.null t then Empty else Text t

line :: Doc
line = Line

lineBreak :: Doc
lineBreak = LineBreak

hardLine :: Doc
hardLine = HardLine

space :: Doc
space = Text " "

-- | A space when rendered in expanded (multi-line) form, but empty when a group
-- is flattened.
softSpace :: Doc
softSpace = SoftSpace

(<+>) :: Doc -> Doc -> Doc
a <+> b =
  a <> space <> b

nest :: Int -> Doc -> Doc
nest = Nest

group :: Doc -> Doc
group = Group

hsep :: [Doc] -> Doc
hsep =
  mconcat . intersperse space . filter (not . isEmpty)

vsep :: [Doc] -> Doc
vsep =
  mconcat . intersperse hardLine . filter (not . isEmpty)

sep :: [Doc] -> Doc
sep docs =
  group (vsep docs)

parens :: Doc -> Doc
parens d =
  text "(" <> d <> text ")"

brackets :: Doc -> Doc
brackets d =
  text "[" <> d <> text "]"

braces :: Doc -> Doc
braces d =
  text "{" <> d <> text "}"

render :: Int -> Doc -> T.Text
render width doc =
  TL.toStrict (B.toLazyText (best 0 [(0, doc)]))
  where
    best :: Int -> [(Int, Doc)] -> B.Builder
    best _ [] =
      mempty
    best col ((i, d) : zs) =
      case d of
        Empty ->
          best col zs
        Text t ->
          B.fromText t <> best (col + T.length t) zs
        Line ->
          B.singleton '\n' <> indent i <> best i zs
        LineBreak ->
          B.singleton '\n' <> indent i <> best i zs
        HardLine ->
          B.singleton '\n' <> indent i <> best i zs
        SoftSpace ->
          B.singleton ' ' <> best (col + 1) zs
        Concat a b ->
          best col ((i, a) : (i, b) : zs)
        Nest j a ->
          best col ((i + j, a) : zs)
        Group a ->
          let flat = flatten a
           in if fitsFlat (width - col) (i, flat) && fits (width - col) ((i, flat) : zs)
                then best col ((i, flat) : zs)
                else best col ((i, a) : zs)

    indent n =
      if n <= 0 then mempty else B.fromText (T.replicate n " ")

    flatten :: Doc -> Doc
    flatten d =
      case d of
        Empty -> Empty
        Text t -> Text t
        Line -> Text " "
        LineBreak -> Empty
        HardLine -> HardLine
        SoftSpace -> Empty
        Concat a b -> Concat (flatten a) (flatten b)
        Nest j a -> Nest j (flatten a)
        Group a -> flatten a

    -- | Check if a single flattened doc fits within the width.
    -- Does not allow early exit at Line/LineBreak since those become spaces when flattened.
    fitsFlat :: Int -> (Int, Doc) -> Bool
    fitsFlat w _ | w < 0 =
      False
    fitsFlat _ (_, Empty) =
      True
    fitsFlat w (_, Text t) =
      w >= T.length t
    fitsFlat _ (_, Line) =
      True  -- Flattened Line is a space, but we're at the end of this doc
    fitsFlat _ (_, LineBreak) =
      True  -- Flattened LineBreak is empty
    fitsFlat _ (_, HardLine) =
      True  -- HardLine forces a break
    fitsFlat w (_, SoftSpace) =
      w >= 1
    fitsFlat w (i, Concat a b) =
      fitsFlat w (i, a) && fitsFlat (w - docWidth a) (i, b)
    fitsFlat w (i, Nest j a) =
      fitsFlat w (i + j, a)
    fitsFlat w (i, Group a) =
      fitsFlat w (i, flatten a)

    -- | Calculate the width of a flattened doc (for use in fitsFlat).
    docWidth :: Doc -> Int
    docWidth d =
      case d of
        Empty -> 0
        Text t -> T.length t
        Line -> 1  -- becomes space
        LineBreak -> 0  -- becomes empty
        HardLine -> 0  -- forces break, width doesn't matter
        SoftSpace -> 0  -- becomes empty when flattened
        Concat a b -> docWidth a + docWidth b
        Nest _ a -> docWidth a
        Group a -> docWidth (flatten a)

    fits :: Int -> [(Int, Doc)] -> Bool
    fits w _ | w < 0 =
      False
    fits _ [] =
      True
    fits w ((i, d) : zs) =
      case d of
        Empty ->
          fits w zs
        Text t ->
          fits (w - T.length t) zs
        Line ->
          True
        LineBreak ->
          True
        HardLine ->
          True
        SoftSpace ->
          fits (w - 1) zs
        Concat a b ->
          fits w ((i, a) : (i, b) : zs)
        Nest j a ->
          fits w ((i + j, a) : zs)
        Group a ->
          fits w ((i, flatten a) : zs)

isEmpty :: Doc -> Bool
isEmpty d =
  case d of
    Empty -> True
    _ -> False
