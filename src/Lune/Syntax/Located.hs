{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Lune.Syntax.Located
  ( Span(..)
  , Located(..)
  , unLoc
  , getLoc
  , getComments
  , mapLoc
  , noLoc
  , withLoc
  , withComments
  , mergeSpans
  ) where

import GHC.Generics (Generic)
import Lune.Syntax.Comment (Comments, emptyComments)

-- | Source span tracking start and end positions
data Span = Span
  { spanStartLine :: !Int   -- ^ 1-based
  , spanStartCol :: !Int    -- ^ 1-based
  , spanEndLine :: !Int     -- ^ 1-based
  , spanEndCol :: !Int      -- ^ 1-based
  }
  deriving (Eq, Show, Generic)

-- | A value paired with source location and attached comments
data Located a = Located
  { locSpan :: !Span
  , locComments :: !Comments
  , locValue :: !a
  }
  deriving (Eq, Show, Generic, Functor)

unLoc :: Located a -> a
unLoc = locValue

getLoc :: Located a -> Span
getLoc = locSpan

getComments :: Located a -> Comments
getComments = locComments

mapLoc :: (a -> b) -> Located a -> Located b
mapLoc f (Located s c a) = Located s c (f a)

-- | Create a Located with dummy span and no comments (for generated code)
noLoc :: a -> Located a
noLoc a = Located (Span 0 0 0 0) emptyComments a

-- | Create a Located with span but no comments
withLoc :: Span -> a -> Located a
withLoc s a = Located s emptyComments a

-- | Add comments to a Located value
withComments :: Comments -> Located a -> Located a
withComments c (Located s _ a) = Located s c a

-- | Merge two spans to cover both ranges
mergeSpans :: Span -> Span -> Span
mergeSpans s1 s2 =
  let (startL, startC) = minPos (spanStartLine s1, spanStartCol s1)
                                (spanStartLine s2, spanStartCol s2)
      (endL, endC) = maxPos (spanEndLine s1, spanEndCol s1)
                            (spanEndLine s2, spanEndCol s2)
  in Span startL startC endL endC
  where
    minPos (l1, c1) (l2, c2)
      | l1 < l2 = (l1, c1)
      | l2 < l1 = (l2, c2)
      | otherwise = (l1, min c1 c2)
    maxPos (l1, c1) (l2, c2)
      | l1 > l2 = (l1, c1)
      | l2 > l1 = (l2, c2)
      | otherwise = (l1, max c1 c2)
