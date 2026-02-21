{-# LANGUAGE OverloadedStrings #-}

module Lune.LSP.State
  ( LspState (..)
  , SemanticInfo (..)
  , OpenDocInfo (..)
  , emptyLspState
  , setOpenDoc
  , removeOpenDoc
  , lookupOpenDoc
  , lookupOpenDocText
  , setLastDiagnostics
  , lookupLastDiagnostics
  , clearLastDiagnostics
  , setSemanticInfo
  , lookupSemanticInfoByPath
  , lookupSemanticInfoByModule
  , clearSemanticInfoByPath
  , clearSemanticInfoByModule
  , incrementCheckVersion
  , getAffectedFiles
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.LSP.Protocol.Types (Diagnostic)
import qualified Lune.Check as Check
import Lune.LSP.Convert (Span)

-- | Information about an open document, including parsed module metadata.
data OpenDocInfo = OpenDocInfo
  { odiText :: Text
  , odiModuleName :: Maybe Text       -- Nothing if parse failed
  , odiImports :: Set Text            -- Empty if parse failed
  , odiImportAliases :: Map Text Text -- alias -> module name
  }
  deriving (Show)

data SemanticInfo = SemanticInfo
  { siModuleName :: Text
  , siPath :: FilePath
  , siTypesAt :: [(Span, Check.TypeScheme)]
  , siValueEnv :: Map Text Check.TypeScheme
  , siDocs :: Map Text Text
  , siModuleDoc :: Maybe Text
  }
  deriving (Show)

data LspState = LspState
  { openDocs :: Map FilePath OpenDocInfo
  , lastDiagnostics :: Map FilePath [Diagnostic]
  , semanticByModule :: Map Text SemanticInfo
  , semanticByPath :: Map FilePath SemanticInfo
  , checkVersion :: Int
  }

emptyLspState :: LspState
emptyLspState =
  LspState
    { openDocs = Map.empty
    , lastDiagnostics = Map.empty
    , semanticByModule = Map.empty
    , semanticByPath = Map.empty
    , checkVersion = 0
    }

setOpenDoc :: FilePath -> OpenDocInfo -> LspState -> LspState
setOpenDoc path info st =
  st {openDocs = Map.insert path info (openDocs st)}

removeOpenDoc :: FilePath -> LspState -> LspState
removeOpenDoc path st =
  st {openDocs = Map.delete path (openDocs st)}

lookupOpenDoc :: FilePath -> LspState -> Maybe OpenDocInfo
lookupOpenDoc path st =
  Map.lookup path (openDocs st)

lookupOpenDocText :: FilePath -> LspState -> Maybe Text
lookupOpenDocText path st =
  odiText <$> lookupOpenDoc path st

incrementCheckVersion :: LspState -> (LspState, Int)
incrementCheckVersion st =
  let newVer = checkVersion st + 1
  in (st { checkVersion = newVer }, newVer)

setLastDiagnostics :: FilePath -> [Diagnostic] -> LspState -> LspState
setLastDiagnostics path diags st =
  st {lastDiagnostics = Map.insert path diags (lastDiagnostics st)}

lookupLastDiagnostics :: FilePath -> LspState -> [Diagnostic]
lookupLastDiagnostics path st =
  Map.findWithDefault [] path (lastDiagnostics st)

clearLastDiagnostics :: FilePath -> LspState -> LspState
clearLastDiagnostics path st =
  st {lastDiagnostics = Map.delete path (lastDiagnostics st)}

setSemanticInfo :: SemanticInfo -> LspState -> LspState
setSemanticInfo info st =
  st
    { semanticByModule = Map.insert (siModuleName info) info (semanticByModule st)
    , semanticByPath = Map.insert (siPath info) info (semanticByPath st)
    }

lookupSemanticInfoByPath :: FilePath -> LspState -> Maybe SemanticInfo
lookupSemanticInfoByPath path st =
  Map.lookup path (semanticByPath st)

lookupSemanticInfoByModule :: Text -> LspState -> Maybe SemanticInfo
lookupSemanticInfoByModule modName st =
  Map.lookup modName (semanticByModule st)

clearSemanticInfoByPath :: FilePath -> LspState -> LspState
clearSemanticInfoByPath path st =
  st {semanticByPath = Map.delete path (semanticByPath st)}

clearSemanticInfoByModule :: Text -> LspState -> LspState
clearSemanticInfoByModule modName st =
  st {semanticByModule = Map.delete modName (semanticByModule st)}

-- | Get all open files affected by a change to the given file.
-- Returns the changed file plus all open files that directly import its module.
getAffectedFiles :: FilePath -> LspState -> [FilePath]
getAffectedFiles changedPath st =
  case lookupOpenDoc changedPath st of
    Nothing -> [changedPath]
    Just info ->
      case odiModuleName info of
        Nothing -> [changedPath]
        Just changedModName ->
          -- Find all open files that import this module
          let importers = Map.foldrWithKey (findImporters changedModName) [] (openDocs st)
          in changedPath : importers
  where
    findImporters targetMod path info acc
      | path == changedPath = acc  -- Don't include self
      | Set.member targetMod (odiImports info) = path : acc
      | otherwise = acc
