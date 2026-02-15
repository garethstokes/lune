{-# LANGUAGE OverloadedStrings #-}

-- | Internal runtime representation and operations for 'Template'.
--
-- This implements the block-aware append rule (SPEC G):
--
--   append(a,b):
--     1) if a.isEmpty -> b
--     2) if b.isEmpty -> a
--     3) if a.endsWithNL -> concat(a,b)
--     4) else if b.startsWithNL -> concat(a,b)
--     5) else if b.isBlock -> concat(a, Text(\"\\n\"), b)
--     6) else concat(a,b)
--
-- Metadata is stored on each node to make 'append' O(1).
module Lune.Template
  ( empty
  , leaf
  , text
  , line
  , lines
  , block
  , append
  , join
  , vcat
  , hcat
  , indent
  , ensureNL
  , render
  , metaOf
  , isEmpty
  , startsWithNL
  , endsWithNL
  , isBlock
  , setBlockFlag
  ) where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Prelude hiding (lines)
import Lune.Eval.Types
  ( EvalError (..)
  , Template (..)
  , TemplateHole (..)
  , TemplateMeta (..)
  , TemplatePart (..)
  , Value (..)
  )
import Lune.Type (Type)

metaOf :: Template -> TemplateMeta
metaOf t =
  case t of
    TemplateLeaf m _ -> m
    TemplateAppend m _ _ -> m
    TemplateIndent m _ _ -> m
    TemplateEnsureNL m _ -> m

isEmpty :: Template -> Bool
isEmpty = templateIsEmpty . metaOf

startsWithNL :: Template -> Bool
startsWithNL = templateStartsWithNL . metaOf

endsWithNL :: Template -> Bool
endsWithNL = templateEndsWithNL . metaOf

isBlock :: Template -> Bool
isBlock = templateIsBlock . metaOf

emptyMeta :: TemplateMeta
emptyMeta =
  TemplateMeta
    { templateIsEmpty = True
    , templateStartsWithNL = False
    , templateEndsWithNL = False
    , templateIsBlock = False
    }

empty :: Template
empty =
  TemplateLeaf emptyMeta Seq.empty

-- | Construct a leaf template from raw parts.
--
-- Adjacent text parts are merged and empty text parts removed.
leaf :: Bool -> Seq TemplatePart -> Template
leaf blocky parts0 =
  let parts = normalizeLeafParts parts0
      meta =
        if Seq.null parts
          then emptyMeta
          else
            TemplateMeta
              { templateIsEmpty = False
              , templateStartsWithNL = leafStartsWithNL parts
              , templateEndsWithNL = leafEndsWithNL parts
              , templateIsBlock = blocky
              }
   in TemplateLeaf meta parts

normalizeLeafParts :: Seq TemplatePart -> Seq TemplatePart
normalizeLeafParts =
  foldl step Seq.empty
  where
    step acc part =
      case part of
        TText t
          | T.null t ->
              acc
          | otherwise ->
              case Seq.viewr acc of
                accInit Seq.:> TText tPrev ->
                  accInit Seq.|> TText (tPrev <> t)
                _ ->
                  acc Seq.|> TText t
        THole _ ->
          acc Seq.|> part

leafStartsWithNL :: Seq TemplatePart -> Bool
leafStartsWithNL parts =
  case Seq.viewl parts of
    TText t Seq.:< _ ->
      "\n" `T.isPrefixOf` t
    _ ->
      False

leafEndsWithNL :: Seq TemplatePart -> Bool
leafEndsWithNL parts =
  case Seq.viewr parts of
    _ Seq.:> TText t ->
      "\n" `T.isSuffixOf` t
    _ ->
      False

text :: Text -> Template
text t =
  leaf False (Seq.singleton (TText t))

line :: Text -> Template
line t =
  text (dropOneSuffix "\n" t)

lines :: [Text] -> Template
lines xs =
  case xs of
    [] ->
      empty
    [x] ->
      line x
    _ ->
      leaf True (Seq.singleton (TText (T.intercalate "\n" xs)))

-- | Block constructor used by the public API.
--
-- Normalizes CRLF/CR to LF and strips a single leading and trailing newline.
block :: Text -> Template
block t0 =
  let t1 = normalizeNewlines t0
      t2 = dropOnePrefix "\n" t1
      t3 = dropOneSuffix "\n" t2
   in leaf True (Seq.singleton (TText t3))

normalizeNewlines :: Text -> Text
normalizeNewlines =
  T.replace "\r\n" "\n" . T.replace "\r" "\n"

dropOnePrefix :: Text -> Text -> Text
dropOnePrefix prefix t =
  case T.stripPrefix prefix t of
    Nothing -> t
    Just rest -> rest

dropOneSuffix :: Text -> Text -> Text
dropOneSuffix suffix t =
  case T.stripSuffix suffix t of
    Nothing -> t
    Just rest -> rest

-- | Raw concatenation (no block-aware newline insertion).
concatRaw :: Template -> Template -> Template
concatRaw a b
  | isEmpty a = b
  | isEmpty b = a
  | otherwise =
      case (a, b) of
        (TemplateLeaf _ pa, TemplateLeaf _ pb) ->
          TemplateLeaf (concatMeta a b) (mergeLeafBoundary pa pb)
        _ ->
          TemplateAppend (concatMeta a b) a b

concatMeta :: Template -> Template -> TemplateMeta
concatMeta a b =
  TemplateMeta
    { templateIsEmpty = False
    , templateStartsWithNL = startsWithNL a
    , templateEndsWithNL = endsWithNL b
    , templateIsBlock = isBlock a || isBlock b
    }

mergeLeafBoundary :: Seq TemplatePart -> Seq TemplatePart -> Seq TemplatePart
mergeLeafBoundary a b =
  case (Seq.viewr a, Seq.viewl b) of
    (aInit Seq.:> TText ta, TText tb Seq.:< bRest) ->
      let merged = ta <> tb
       in if T.null merged
            then aInit Seq.>< bRest
            else (aInit Seq.|> TText merged) Seq.>< bRest
    _ ->
      a Seq.>< b

append :: Template -> Template -> Template
append a b
  | isEmpty a = b
  | isEmpty b = a
  | endsWithNL a = concatRaw a b
  | startsWithNL b = concatRaw a b
  | isBlock b = concatRaw a (concatRaw (text "\n") b)
  | otherwise = concatRaw a b

join :: Template -> [Template] -> Template
join sep ts =
  case ts of
    [] ->
      empty
    (t : rest) ->
      foldl (\acc x -> append (append acc sep) x) t rest

vcat :: [Template] -> Template
vcat ts =
  setBlockFlag True (foldl vappend empty ts)
  where
    vappend a b
      | isEmpty a = b
      | isEmpty b = a
      | endsWithNL a = concatRaw a b
      | startsWithNL b = concatRaw a b
      | otherwise = concatRaw a (concatRaw (text "\n") b)

hcat :: [Template] -> Template
hcat =
  foldl concatRaw empty

indent :: Int -> Template -> Template
indent n t
  | n <= 0 = t
  | isEmpty t = t
  | otherwise =
      TemplateIndent (metaOf t) n t

ensureNL :: Template -> Template
ensureNL t
  | isEmpty t =
      t
  | otherwise =
      let m = metaOf t
          m' = m {templateEndsWithNL = True}
       in TemplateEnsureNL m' t

-- | Override the 'isBlock' flag on the root node.
--
-- This is used to preserve literal flavor semantics: templates originating from
-- normal string interpolation are inline (non-blocky), even if they splice
-- block templates.
setBlockFlag :: Bool -> Template -> Template
setBlockFlag flag t
  | isEmpty t = t
  | otherwise =
      let m = metaOf t
          m' = m {templateIsBlock = flag}
       in case t of
            TemplateLeaf _ parts ->
              TemplateLeaf m' parts
            TemplateAppend _ a b ->
              TemplateAppend m' a b
            TemplateIndent _ n inner ->
              TemplateIndent m' n inner
            TemplateEnsureNL _ inner ->
              TemplateEnsureNL m' inner

-- | Render to strict text, evaluating holes.
render :: (Value -> Either EvalError Value) -> Template -> Either EvalError Text
render forceValue t = do
  forced <- forceHoles forceValue t
  builder <- renderNoHoles forced
  pure (TL.toStrict (TB.toLazyText builder))

forceHoles :: (Value -> Either EvalError Value) -> Template -> Either EvalError Template
forceHoles forceValue t =
  case t of
    TemplateLeaf m parts -> do
      combined <- foldM step empty (toList parts)
      pure (setBlockFlag (templateIsBlock m) combined)
      where
        step acc part =
          case part of
            TText txt ->
              pure (append acc (text txt))
            THole hole -> do
              v <- forceValue (templateHoleThunk hole)
              case v of
                VTemplate tmpl -> do
                  tmpl' <- forceHoles forceValue tmpl
                  pure (append acc tmpl')
                other ->
                  Left (ExpectedTemplate other)
    TemplateAppend _ a b -> do
      a' <- forceHoles forceValue a
      b' <- forceHoles forceValue b
      pure (concatRaw a' b')
    TemplateIndent _ n inner -> do
      inner' <- forceHoles forceValue inner
      pure (indent n inner')
    TemplateEnsureNL _ inner -> do
      inner' <- forceHoles forceValue inner
      pure (ensureNL inner')

renderNoHoles :: Template -> Either EvalError TB.Builder
renderNoHoles t =
  case t of
    TemplateLeaf _ parts ->
      pure (foldMap renderLeafPart (toList parts))
    TemplateAppend _ a b ->
      (<>) <$> renderNoHoles a <*> renderNoHoles b
    TemplateIndent _ n inner -> do
      innerText <- renderNoHolesText inner
      pure (TB.fromText (indentText n innerText))
    TemplateEnsureNL _ inner -> do
      innerText <- renderNoHolesText inner
      pure (TB.fromText (ensureNLText innerText))
  where
    renderLeafPart part =
      case part of
        TText txt ->
          TB.fromText txt
        THole {} ->
          mempty

renderNoHolesText :: Template -> Either EvalError Text
renderNoHolesText t = do
  b <- renderNoHoles t
  pure (TL.toStrict (TB.toLazyText b))

indentText :: Int -> Text -> Text
indentText n t
  | n <= 0 = t
  | T.null t = t
  | otherwise =
      let prefix = T.replicate n " "
          ls = T.splitOn "\n" t
          ls' =
            [ if T.null line then line else prefix <> line
            | line <- ls
            ]
       in T.intercalate "\n" ls'

ensureNLText :: Text -> Text
ensureNLText t
  | T.null t = t
  | "\n" `T.isSuffixOf` t = t
  | otherwise = t <> "\n"
