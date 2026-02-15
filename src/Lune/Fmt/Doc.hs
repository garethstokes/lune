{-# LANGUAGE OverloadedStrings #-}

module Lune.Fmt.Doc
  ( Doc
  , empty
  , text
  , line
  , lineBreak
  , hardLine
  , space
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
        Concat a b ->
          best col ((i, a) : (i, b) : zs)
        Nest j a ->
          best col ((i + j, a) : zs)
        Group a ->
          let flat = flatten a
           in if fits (width - col) ((i, flat) : zs)
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
        Concat a b -> Concat (flatten a) (flatten b)
        Nest j a -> Nest j (flatten a)
        Group a -> flatten a

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
