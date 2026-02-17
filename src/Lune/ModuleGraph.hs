module Lune.ModuleGraph
  ( ModuleError (..)
  , LoadedModule (..)
  , Program (..)
  , ModuleLoader (..)
  , defaultDiskLoader
  , loadProgram
  , loadProgramWithEntryModule
  , loadProgramWithEntryModuleUsing
  , resolveModulePath
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (try, SomeException, displayException)
import qualified Data.Text.IO as TIO
import qualified Lune.Parser as Parser
import qualified Lune.Syntax as S
import qualified Lune.Derive as Derive
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (splitDirectories, takeDirectory, (</>), isAbsolute)
import Text.Megaparsec (errorBundlePretty)

data ModuleError
  = ModuleParseError FilePath String
  | ModuleReadError FilePath String
  | MissingModule Text [FilePath]
  | ModuleNameMismatch Text Text FilePath
  | DuplicateModuleName Text FilePath FilePath
  | ImportCycle [Text]
  | DeriveExpansionError Derive.DeriveError
  deriving (Show)

data LoadedModule = LoadedModule
  { lmName :: Text
  , lmPath :: FilePath
  , lmModule :: S.Module
  }
  deriving (Show)

data Program = Program
  { progEntryName :: Text
  , progModules :: Map Text LoadedModule
  , progOrder :: [Text]
  }
  deriving (Show)

-- | Abstraction for loading module file contents.
-- Allows injection of VFS (virtual file system) for LSP.
newtype ModuleLoader = ModuleLoader
  { mlReadFileText :: FilePath -> IO (Either String Text)
  }

-- | Default loader that reads files from disk.
defaultDiskLoader :: ModuleLoader
defaultDiskLoader = ModuleLoader
  { mlReadFileText = \path -> do
      result <- try (TIO.readFile path) :: IO (Either SomeException Text)
      pure $ case result of
        Left e -> Left (displayException e)
        Right t -> Right t
  }

loadProgram :: FilePath -> IO (Either ModuleError Program)
loadProgram entryPath = do
  entryResult <- Parser.parseFileEither entryPath
  case entryResult of
    Left err ->
      pure (Left (ModuleParseError entryPath err))
    Right entryModule ->
      loadProgramWithEntryModule entryPath entryModule

loadProgramWithEntryModule :: FilePath -> S.Module -> IO (Either ModuleError Program)
loadProgramWithEntryModule = loadProgramWithEntryModuleUsing defaultDiskLoader

loadProgramWithEntryModuleUsing :: ModuleLoader -> FilePath -> S.Module -> IO (Either ModuleError Program)
loadProgramWithEntryModuleUsing loader entryPath entryModule = do
  let entryDir = takeDirectory entryPath
  case Derive.expandDerives entryModule of
    Left deriveErr ->
      pure (Left (DeriveExpansionError deriveErr))
    Right expandedModule -> do
      let entryName = S.modName expandedModule
          entryLoaded =
            LoadedModule
              { lmName = entryName
              , lmPath = entryPath
              , lmModule = expandedModule
              }
      loaded <- loadModuleUsing loader entryDir [] Map.empty entryLoaded
      case loaded of
        Left err ->
          pure (Left err)
        Right (mods, order) ->
          pure
            ( Right
                Program
                  { progEntryName = entryName
                  , progModules = mods
                  , progOrder = order
                  }
            )

loadModuleUsing :: ModuleLoader -> FilePath -> [Text] -> Map Text LoadedModule -> LoadedModule -> IO (Either ModuleError (Map Text LoadedModule, [Text]))
loadModuleUsing loader entryDir stack loaded m =
  case Map.lookup (lmName m) loaded of
    Just existing ->
      if lmPath existing == lmPath m
        then pure (Right (loaded, []))
        else pure (Left (DuplicateModuleName (lmName m) (lmPath existing) (lmPath m)))
    Nothing ->
      if lmName m `elem` stack
        then pure (Left (ImportCycle (reverse (lmName m : stack))))
        else do
          deps <- loadImportsUsing loader entryDir (lmName m : stack) loaded (implicitPreludeImports m (S.modImports (lmModule m)))
          case deps of
            Left err ->
              pure (Left err)
            Right (loaded', depOrder) -> do
              let loaded'' = Map.insert (lmName m) m loaded'
              pure (Right (loaded'', depOrder <> [lmName m]))

implicitPreludeImports :: LoadedModule -> [S.Import] -> [S.Import]
implicitPreludeImports m imports =
  if shouldImplicitPreludeImport m && not (any ((== preludeModuleName) . S.impName) imports)
    then S.Import {S.impName = preludeModuleName, S.impAs = Nothing, S.impExposing = Nothing} : imports
    else imports
  where
    preludeModuleName = "Lune.Prelude"

shouldImplicitPreludeImport :: LoadedModule -> Bool
shouldImplicitPreludeImport m =
  lmName m /= "Lune.Prelude" && not (isPreludePath (lmPath m))

isPreludePath :: FilePath -> Bool
isPreludePath path =
  case splitDirectories path of
    ("prelude" : _) -> True
    _ ->
      -- Also check for absolute Nix store paths containing prelude
      "lune-prelude" `isInfixOf` path
  where
    isInfixOf needle haystack = needle `elem` words (map (\c -> if c == '/' || c == '-' then ' ' else c) haystack)

loadImportsUsing :: ModuleLoader -> FilePath -> [Text] -> Map Text LoadedModule -> [S.Import] -> IO (Either ModuleError (Map Text LoadedModule, [Text]))
loadImportsUsing loader entryDir stack loaded imports =
  go loaded [] imports
  where
    go accLoaded accOrder [] =
      pure (Right (accLoaded, accOrder))
    go accLoaded accOrder (imp : rest) = do
      let modName = S.impName imp
      case Map.lookup modName accLoaded of
        Just _ ->
          go accLoaded accOrder rest
        Nothing -> do
          resolved <- resolveModulePath entryDir modName
          case resolved of
            Left err ->
              pure (Left err)
            Right path -> do
              eText <- mlReadFileText loader path
              case eText of
                Left ioErr ->
                  pure (Left (ModuleReadError path ioErr))
                Right txt ->
                  case Parser.parseTextBundle path txt of
                    Left bundle ->
                      pure (Left (ModuleParseError path (errorBundlePretty bundle)))
                    Right m -> do
                      if S.modName m /= modName
                        then pure (Left (ModuleNameMismatch modName (S.modName m) path))
                        else do
                          let modInfo =
                                LoadedModule
                                  { lmName = modName
                                  , lmPath = path
                                  , lmModule = m
                                  }
                          loadedRes <- loadModuleUsing loader entryDir stack accLoaded modInfo
                          case loadedRes of
                            Left err ->
                              pure (Left err)
                            Right (loaded', order') ->
                              go loaded' (accOrder <> order') rest

resolveModulePath :: FilePath -> Text -> IO (Either ModuleError FilePath)
resolveModulePath entryDir modName = do
  preludePath <- lookupEnv "LUNE_PRELUDE_PATH"
  let (relDir, relDot) = moduleNameToPaths modName
      localCandidates =
        [ entryDir </> relDir
        , entryDir </> relDot
        , "." </> relDir
        , "." </> relDot
        ]
      preludeDir = maybe "prelude" id preludePath
      candidates =
        dedupePaths (localCandidates <> [preludeDir </> relDir])
  found <- firstExisting candidates
  case found of
    Nothing ->
      pure (Left (MissingModule modName candidates))
    Just p ->
      pure (Right p)

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting paths =
  go paths
  where
    go [] =
      pure Nothing
    go (p : ps) = do
      exists <- doesFileExist p
      if exists then pure (Just p) else go ps

moduleNameToPaths :: Text -> (FilePath, FilePath)
moduleNameToPaths name =
  (moduleNameToDirPath name, moduleNameToDotPath name)

moduleNameToDirPath :: Text -> FilePath
moduleNameToDirPath name =
  foldl (</>) "" (splitDirectories (T.unpack (T.replace "." "/" name))) <> ".lune"

moduleNameToDotPath :: Text -> FilePath
moduleNameToDotPath name =
  T.unpack name <> ".lune"

dedupePaths :: [FilePath] -> [FilePath]
dedupePaths =
  go Map.empty
  where
    go _ [] =
      []
    go seen (p : ps) =
      case Map.lookup p seen of
        Just () ->
          go seen ps
        Nothing ->
          p : go (Map.insert p () seen) ps
