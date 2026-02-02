module Lune.ModuleGraph
  ( ModuleError (..)
  , LoadedModule (..)
  , Program (..)
  , loadProgram
  , resolveModulePath
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Parser as Parser
import qualified Lune.Syntax as S
import System.Directory (doesFileExist)
import System.FilePath (splitDirectories, (</>))

data ModuleError
  = ModuleParseError FilePath String
  | MissingModule Text [FilePath]
  | ModuleNameMismatch Text Text FilePath
  | DuplicateModuleName Text FilePath FilePath
  | ImportCycle [Text]
  deriving (Eq, Show)

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

loadProgram :: FilePath -> IO (Either ModuleError Program)
loadProgram entryPath = do
  entryResult <- Parser.parseFileEither entryPath
  case entryResult of
    Left err ->
      pure (Left (ModuleParseError entryPath err))
    Right entryModule -> do
      let entryName = S.modName entryModule
      let entryLoaded =
            LoadedModule
              { lmName = entryName
              , lmPath = entryPath
              , lmModule = entryModule
              }
      loaded <- loadModule [] Map.empty entryLoaded
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

loadModule :: [Text] -> Map Text LoadedModule -> LoadedModule -> IO (Either ModuleError (Map Text LoadedModule, [Text]))
loadModule stack loaded m =
  case Map.lookup (lmName m) loaded of
    Just existing ->
      if lmPath existing == lmPath m
        then pure (Right (loaded, []))
        else pure (Left (DuplicateModuleName (lmName m) (lmPath existing) (lmPath m)))
    Nothing ->
      if lmName m `elem` stack
        then pure (Left (ImportCycle (reverse (lmName m : stack))))
        else do
          deps <- loadImports (lmName m : stack) loaded (implicitPreludeImports m (S.modImports (lmModule m)))
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
    _ -> False

loadImports :: [Text] -> Map Text LoadedModule -> [S.Import] -> IO (Either ModuleError (Map Text LoadedModule, [Text]))
loadImports stack loaded imports =
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
          resolved <- resolveModulePath modName
          case resolved of
            Left err ->
              pure (Left err)
            Right path -> do
              parsed <- Parser.parseFileEither path
              case parsed of
                Left perr ->
                  pure (Left (ModuleParseError path perr))
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
                      loadedRes <- loadModule stack accLoaded modInfo
                      case loadedRes of
                        Left err ->
                          pure (Left err)
                        Right (loaded', order') ->
                          go loaded' (accOrder <> order') rest

resolveModulePath :: Text -> IO (Either ModuleError FilePath)
resolveModulePath modName = do
  let rel = moduleNameToPath modName
      candidates = ["." </> rel, "prelude" </> rel]
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

moduleNameToPath :: Text -> FilePath
moduleNameToPath name =
  foldl (</>) "" (splitDirectories (T.unpack (T.replace "." "/" name))) <> ".lune"
