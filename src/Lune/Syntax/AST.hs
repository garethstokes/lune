{-# LANGUAGE DeriveGeneric #-}

module Lune.Syntax.AST
  ( -- Re-export Located types
    module Lune.Syntax.Located
  , module Lune.Syntax.Comment
    -- New located AST type aliases
  , LExpr
  , LPattern
  , LType
  , LDecl
  , LStmt
  , LAlt
  ) where

import Lune.Syntax.Located
import Lune.Syntax.Comment
import qualified Lune.Syntax as S

-- | Located expression
type LExpr = Located S.Expr

-- | Located pattern
type LPattern = Located S.Pattern

-- | Located type
type LType = Located S.Type

-- | Located declaration
type LDecl = Located S.Decl

-- | Located statement
type LStmt = Located S.Stmt

-- | Located case alternative
type LAlt = Located S.Alt
