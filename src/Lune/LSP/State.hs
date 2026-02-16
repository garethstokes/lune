{-# LANGUAGE OverloadedStrings #-}

module Lune.LSP.State
  ( LspState (..)
  , emptyLspState
  , setOpenDoc
  , removeOpenDoc
  , lookupOpenDoc
  , setLastDiagnostics
  , lookupLastDiagnostics
  , clearLastDiagnostics
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Language.LSP.Protocol.Types (Diagnostic)

data LspState = LspState
  { openDocs :: Map FilePath Text
  , lastDiagnostics :: Map FilePath [Diagnostic]
  }

emptyLspState :: LspState
emptyLspState =
  LspState
    { openDocs = Map.empty
    , lastDiagnostics = Map.empty
    }

setOpenDoc :: FilePath -> Text -> LspState -> LspState
setOpenDoc path contents st =
  st {openDocs = Map.insert path contents (openDocs st)}

removeOpenDoc :: FilePath -> LspState -> LspState
removeOpenDoc path st =
  st {openDocs = Map.delete path (openDocs st)}

lookupOpenDoc :: FilePath -> LspState -> Maybe Text
lookupOpenDoc path st =
  Map.lookup path (openDocs st)

setLastDiagnostics :: FilePath -> [Diagnostic] -> LspState -> LspState
setLastDiagnostics path diags st =
  st {lastDiagnostics = Map.insert path diags (lastDiagnostics st)}

lookupLastDiagnostics :: FilePath -> LspState -> [Diagnostic]
lookupLastDiagnostics path st =
  Map.findWithDefault [] path (lastDiagnostics st)

clearLastDiagnostics :: FilePath -> LspState -> LspState
clearLastDiagnostics path st =
  st {lastDiagnostics = Map.delete path (lastDiagnostics st)}
