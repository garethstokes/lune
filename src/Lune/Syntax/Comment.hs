{-# LANGUAGE DeriveGeneric #-}

module Lune.Syntax.Comment
  ( Comment(..)
  , CommentKind(..)
  , CommentPosition(..)
  , Comments(..)
  , emptyComments
  , hasComments
  , leadingComments
  , trailingComments
  , innerComments
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

data CommentKind
  = LineComment    -- ^ -- style comment
  | BlockComment   -- ^ {- -} style comment
  | DocComment     -- ^ {-| -} doc comment
  deriving (Eq, Show, Generic)

-- | Where the comment appears relative to its attached node
data CommentPosition
  = Leading   -- ^ Before the node, on its own line(s)
  | Trailing  -- ^ After the node, on the same line
  | Inner     -- ^ Inside a compound node (e.g., between list elements)
  deriving (Eq, Show, Generic)

data Comment = Comment
  { commentKind :: !CommentKind
  , commentPosition :: !CommentPosition
  , commentText :: !Text
  , commentLine :: !Int        -- ^ 1-based line number
  , commentCol :: !Int         -- ^ 1-based column
  , commentEndLine :: !Int     -- ^ End line for block comments
  , commentEndCol :: !Int      -- ^ End column
  }
  deriving (Eq, Show, Generic)

data Comments = Comments
  { commentsLeading :: ![Comment]
  , commentsTrailing :: ![Comment]
  , commentsInner :: ![Comment]
  }
  deriving (Eq, Show, Generic)

emptyComments :: Comments
emptyComments = Comments [] [] []

hasComments :: Comments -> Bool
hasComments (Comments l t i) = not (null l && null t && null i)

leadingComments :: Comments -> [Comment]
leadingComments = commentsLeading

trailingComments :: Comments -> [Comment]
trailingComments = commentsTrailing

innerComments :: Comments -> [Comment]
innerComments = commentsInner
