{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lune.LSP.Handlers
  ( handlers
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.LSP.Server
  ( Handlers
  , LspM
  , notificationHandler
  , requestHandler
  , sendNotification
  )
import Language.LSP.Protocol.Message
  ( Method (..)
  , MessageResult
  , SMethod (..)
  , TNotificationMessage (..)
  , TRequestMessage (..)
  , TResponseError
  )
import Language.LSP.Protocol.Types
  ( Diagnostic (..)
  , DiagnosticSeverity (..)
  , DidChangeTextDocumentParams (..)
  , DidCloseTextDocumentParams (..)
  , DidOpenTextDocumentParams (..)
  , DocumentFormattingParams (..)
  , Hover (..)
  , HoverParams (..)
  , MarkupContent (..)
  , MarkupKind (..)
  , Null (..)
  , Position (..)
  , PublishDiagnosticsParams (..)
  , Range (..)
  , TextDocumentContentChangeEvent (..)
  , TextDocumentContentChangePartial (..)
  , TextDocumentContentChangeWholeDocument (..)
  , TextDocumentIdentifier (..)
  , TextDocumentItem (..)
  , TextEdit (..)
  , Uri
  , VersionedTextDocumentIdentifier (..)
  , type (|?)(..)
  )
import qualified Lune.Check as Check
import Lune.Desugar (desugarModule)
import qualified Lune.Docs as Docs
import qualified Lune.Fmt as Fmt
import Lune.ModuleGraph (ModuleLoader (..))
import qualified Lune.ModuleGraph as MG
import qualified Lune.Parser as Parser
import qualified Lune.Resolve as Resolve
import qualified Lune.Syntax as S
import Lune.Validate (validateModule)
import Lune.LSP.Convert
  ( Span (..)
  , filePathToUri'
  , fullDocumentRange
  , spanFromSourcePos
  , spanToRange
  , utf16ColToCodepoints
  , uriToFilePath'
  )
import Lune.LSP.State
  ( LspState
  , OpenDocInfo (..)
  , SemanticInfo (..)
  , checkVersion
  , clearLastDiagnostics
  , clearSemanticInfoByModule
  , clearSemanticInfoByPath
  , getAffectedFiles
  , incrementCheckVersion
  , lookupLastDiagnostics
  , lookupOpenDoc
  , lookupOpenDocText
  , lookupSemanticInfoByModule
  , lookupSemanticInfoByPath
  , removeOpenDoc
  , setLastDiagnostics
  , setSemanticInfo
  , setOpenDoc
  )
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec
  ( ParseErrorBundle
  , bundleErrors
  , bundlePosState
  , errorBundlePretty
  , errorOffset
  , reachOffset
  )
import Text.Megaparsec.State (pstateSourcePos)
import Data.Void (Void)

handlers :: IORef LspState -> Handlers (LspM ())
handlers stateRef =
  mconcat
    [ diagnosticsHandlers stateRef
    , formattingHandlers stateRef
    , hoverHandlers stateRef
    ]

-- =============================================================================
-- Diagnostics
-- =============================================================================

diagnosticsHandlers :: IORef LspState -> Handlers (LspM ())
diagnosticsHandlers stateRef =
  mconcat
    [ notificationHandler SMethod_TextDocumentDidOpen (handleDidOpen stateRef)
    , notificationHandler SMethod_TextDocumentDidChange (handleDidChange stateRef)
    , notificationHandler SMethod_TextDocumentDidClose (handleDidClose stateRef)
    ]

handleDidOpen :: IORef LspState -> TNotificationMessage 'Method_TextDocumentDidOpen -> LspM () ()
handleDidOpen stateRef (TNotificationMessage _ _ (DidOpenTextDocumentParams docItem)) = do
  let TextDocumentItem uri _langId _ver contents = docItem
  case uriToFilePath' uri of
    Nothing ->
      pure ()
    Just path -> do
      let (modName, imports, importAliases) = parseModuleInfo path contents
          info = OpenDocInfo
            { odiText = contents
            , odiModuleName = modName
            , odiImports = imports
            , odiImportAliases = importAliases
            }
      liftIO $
        atomicModifyIORef' stateRef $ \st ->
          let st1 = setOpenDoc path info st
           in (st1, ())
      publishWorkspaceDiagnostics stateRef path

handleDidChange :: IORef LspState -> TNotificationMessage 'Method_TextDocumentDidChange -> LspM () ()
handleDidChange stateRef (TNotificationMessage _ _ (DidChangeTextDocumentParams docId changes)) = do
  let VersionedTextDocumentIdentifier uri _ver = docId
  case uriToFilePath' uri of
    Nothing ->
      pure ()
    Just path ->
      case changes of
        [] ->
          pure ()
        cs -> do
          let newText =
                case last cs of
                  TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument t)) -> t
                  TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial _ _ t)) -> t
              (modName, imports, importAliases) = parseModuleInfo path newText
              info = OpenDocInfo
                { odiText = newText
                , odiModuleName = modName
                , odiImports = imports
                , odiImportAliases = importAliases
                }
          liftIO $
            atomicModifyIORef' stateRef $ \st ->
              let st1 = setOpenDoc path info st
               in (st1, ())
          publishWorkspaceDiagnostics stateRef path

handleDidClose :: IORef LspState -> TNotificationMessage 'Method_TextDocumentDidClose -> LspM () ()
handleDidClose stateRef (TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri))) = do
  case uriToFilePath' uri of
    Nothing ->
      pure ()
    Just path -> do
      liftIO $
        atomicModifyIORef' stateRef $ \st ->
          let oldMod = odiModuleName =<< lookupOpenDoc path st
              st1 = clearSemanticInfoByPath path st
              st2 = maybe st1 (`clearSemanticInfoByModule` st1) oldMod
              st3 = clearLastDiagnostics path (removeOpenDoc path st2)
           in (st3, ())
      sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing [])

publishCheckedDiagnostics :: IORef LspState -> FilePath -> Text -> LspM () ()
publishCheckedDiagnostics stateRef path contents = do
  CheckResult diags semantics <- liftIO (checkFileWithSemantic stateRef path contents)
  let lspDiags = map (toLspDiagnostic contents) diags
      uri = filePathToUri' path
  liftIO $
    atomicModifyIORef' stateRef $ \st ->
      let st1 = setLastDiagnostics path lspDiags st
       in case semantics of
            [] ->
              (st1, ())
            _ ->
              let (st2, ver) = incrementCheckVersion st1
                  semantics' = map (\s -> s {siVersion = ver}) semantics
                  st3 = foldr setSemanticInfo st2 semantics'
               in (st3, ())
  sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing lspDiags)

-- | Publish diagnostics for the changed file and all files that import it.
-- Processes the changed file first, then dependents in sorted order.
publishWorkspaceDiagnostics :: IORef LspState -> FilePath -> LspM () ()
publishWorkspaceDiagnostics stateRef changedPath = do
  st <- liftIO (readIORef stateRef)
  let affected = getAffectedFiles changedPath st
      -- Process changed file first, then others in sorted order
      others = sort (filter (/= changedPath) affected)
      ordered = changedPath : others
  -- Publish diagnostics for each affected file
  mapM_ (publishDiagsForFile st) ordered
  where
    publishDiagsForFile st path =
      -- Get current text from open docs or skip if not open
      case lookupOpenDocText path st of
        Nothing -> pure ()
        Just contents -> do
          CheckResult diags semantics <- liftIO (checkFileWithSemantic stateRef path contents)
          let lspDiags = map (toLspDiagnostic contents) diags
              uri = filePathToUri' path
          liftIO $
            atomicModifyIORef' stateRef $ \st' ->
              let st1 = setLastDiagnostics path lspDiags st'
               in case semantics of
                    [] ->
                      (st1, ())
                    _ ->
                      let (st2, ver) = incrementCheckVersion st1
                          semantics' = map (\s -> s {siVersion = ver}) semantics
                          st3 = foldr setSemanticInfo st2 semantics'
                       in (st3, ())
          sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing lspDiags)

-- =============================================================================
-- Formatting
-- =============================================================================

formattingHandlers :: IORef LspState -> Handlers (LspM ())
formattingHandlers stateRef =
  mconcat
    [ requestHandler SMethod_TextDocumentFormatting (handleFormatting stateRef)
    ]

handleFormatting ::
  IORef LspState ->
  TRequestMessage 'Method_TextDocumentFormatting ->
  (Either (TResponseError 'Method_TextDocumentFormatting) (MessageResult 'Method_TextDocumentFormatting) -> LspM () ()) ->
  LspM () ()
handleFormatting stateRef (TRequestMessage _ _ _ (DocumentFormattingParams _token (TextDocumentIdentifier uri) _opts)) responder = do
  case uriToFilePath' uri of
    Nothing ->
      responder (Right (InL []))
    Just path -> do
      contentsOrErr <- liftIO $ do
        st <- readIORef stateRef
        case lookupOpenDocText path st of
          Just t ->
            pure (Right t)
          Nothing ->
            -- Fall back to disk if we don't have an open buffer.
            (try (TIO.readFile path) :: IO (Either SomeException Text))

      case contentsOrErr of
        Left e -> do
          publishFormattingFailure stateRef path uri (T.pack (displayException e))
          responder (Right (InL []))
        Right contents0 ->
          case Fmt.formatText path contents0 of
            Left err -> do
              publishFormattingFailure stateRef path uri (Fmt.renderFmtError err)
              responder (Right (InL []))
            Right formatted -> do
              let edit = TextEdit (fullDocumentRange contents0) formatted
              responder (Right (InL [edit]))

publishFormattingFailure :: IORef LspState -> FilePath -> Uri -> Text -> LspM () ()
publishFormattingFailure stateRef path uri msg = do
  st <- liftIO (readIORef stateRef)
  let base = lookupLastDiagnostics path st
      infoDiag =
        Diagnostic
          (Range (Position 0 0) (Position 0 0))
          (Just DiagnosticSeverity_Information)
          Nothing
          Nothing
          (Just "lune")
          ("Formatting failed: " <> msg)
          Nothing
          Nothing
          Nothing
      allDiags = base <> [infoDiag]
  liftIO $ atomicModifyIORef' stateRef (\st' -> (setLastDiagnostics path allDiags st', ()))
  sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing allDiags)

-- =============================================================================
-- Hover
-- =============================================================================

hoverHandlers :: IORef LspState -> Handlers (LspM ())
hoverHandlers stateRef =
  mconcat
    [ requestHandler SMethod_TextDocumentHover (handleHover stateRef)
    ]

handleHover ::
  IORef LspState ->
  TRequestMessage 'Method_TextDocumentHover ->
  (Either (TResponseError 'Method_TextDocumentHover) (MessageResult 'Method_TextDocumentHover) -> LspM () ()) ->
  LspM () ()
handleHover stateRef (TRequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _token)) responder =
  case uriToFilePath' uri of
    Nothing ->
      responder (Right (InR Null))
    Just path -> do
      st <- liftIO (readIORef stateRef)
      dbg <- liftIO (lookupEnv "LUNE_LSP_DEBUG")
      let debugLog msg =
            case dbg of
              Nothing -> pure ()
              Just _ -> liftIO (hPutStrLn stderr msg)
      -- Prefer open buffers; fall back to disk.
      docOrErr <- liftIO $ do
        case lookupOpenDoc path st of
          Just info ->
            pure (Right (odiText info, odiModuleName info, odiImportAliases info))
          Nothing -> do
            contentsOrErr <- (try (TIO.readFile path) :: IO (Either SomeException Text))
            pure $
              case contentsOrErr of
                Left e -> Left e
                Right contents ->
                  let (modName, _imports, importAliases) = parseModuleInfo path contents
                   in Right (contents, modName, importAliases)

      case docOrErr of
        Left e -> do
          liftIO $ hPutStrLn stderr ("lune: hover: failed to read " <> path <> ": " <> displayException e)
          responder (Right (InR Null))
        Right (docText, modName, importAliases) -> do
          let tokenAndRange = wordAtPosition docText pos

          case tokenAndRange of
            Nothing -> do
              debugLog $
                "lune: hover: uri="
                  <> show uri
                  <> " pos="
                  <> show pos
                  <> " token=<none>"
                  <> " semanticFound=false"
                  <> " semanticVersion=<none>"
                  <> " checkVersion="
                  <> show (checkVersion st)
              responder (Right (InR Null))
            Just (range, tok) -> do
              let mTarget = resolveHoverTarget st path modName importAliases tok
                  mSem = fst <$> mTarget
                  (semanticFound, semanticVer) =
                    case mSem of
                      Nothing -> (False, "<none>")
                      Just sem -> (True, show (siVersion sem))
                  (mScheme, mDoc) =
                    case mTarget of
                      Nothing ->
                        (Nothing, Nothing)
                      Just (sem, name) ->
                        ( lookupSchemeInSemantic sem name
                        , Map.lookup name (siDocs sem)
                        )

                  md =
                    case (mScheme, mDoc) of
                      (Just scheme, doc) ->
                        let typeBlock =
                              T.intercalate
                                "\n"
                                [ "### " <> tok
                                , ""
                                , "```lune"
                                , tok <> " : " <> Check.renderTypeSchemeHover scheme
                                , "```"
                                ]
                         in case doc of
                              Nothing -> typeBlock
                              Just d -> typeBlock <> "\n\n" <> d
                      (Nothing, Just d) ->
                        T.intercalate "\n" ["### " <> tok, "", d]
                      (Nothing, Nothing) ->
                        case lookupModuleDocForToken st importAliases tok of
                          Just d ->
                            T.intercalate "\n" ["### " <> tok, "", d]
                          Nothing ->
                            T.concat
                              [ "### "
                              , tok
                              , case modName of
                                  Nothing -> ""
                                  Just m -> "\n\n_Module: " <> m <> "_"
                              ]

                  hover =
                    Hover
                      { _contents = InL (MarkupContent MarkupKind_Markdown md)
                      , _range = Just range
                      }

              debugLog $
                "lune: hover: uri="
                  <> show uri
                  <> " pos="
                  <> show pos
                  <> " token="
                  <> T.unpack tok
                  <> " semanticFound="
                  <> show semanticFound
                  <> " semanticVersion="
                  <> semanticVer
                  <> " checkVersion="
                  <> show (checkVersion st)
              responder (Right (InL hover))
  where
    resolveHoverTarget st path modName importAliases tok =
      case T.splitOn "." tok of
        [modRef, name] -> do
          sem <- lookupQualifiedSemantic st modRef importAliases
          pure (sem, name)
        _ -> do
          sem <- lookupUnqualifiedSemantic st path modName
          pure (sem, tok)

    lookupUnqualifiedSemantic st path modName = do
      case lookupSemanticInfoByPath path st of
        Just s -> Just s
        Nothing ->
          modName >>= \m -> lookupSemanticInfoByModule m st

    lookupQualifiedSemantic st modRef importAliases = do
      targetMod <-
        case lookupSemanticInfoByModule modRef st of
          Just _ -> Just modRef
          Nothing -> Map.lookup modRef importAliases
      lookupSemanticInfoByModule targetMod st

    lookupSchemeInSemantic sem name =
      lookupFirst [siModuleName sem <> "." <> name, name] (siValueEnv sem)

    lookupModuleDocForToken st importAliases tok =
      let fromModule m =
            siModuleDoc =<< lookupSemanticInfoByModule m st
       in case fromModule tok of
            Just d -> Just d
            Nothing ->
              case Map.lookup tok importAliases of
                Nothing -> Nothing
                Just m -> fromModule m

    lookupFirst keys env =
      case keys of
        [] ->
          Nothing
        (k : ks) ->
          case Map.lookup k env of
            Just v -> Just v
            Nothing -> lookupFirst ks env

-- | Get the token at the cursor position.
--
-- Token characters: A-Z a-z 0-9 _ .
--
wordAtPosition :: Text -> Position -> Maybe (Range, Text)
wordAtPosition doc (Position line0 char0) = do
  let ls = T.splitOn "\n" doc
      li = fromIntegral line0 :: Int
  lineText <- if li < 0 || li >= length ls then Nothing else Just (ls !! li)

  let len = T.length lineText
      ciUtf16 = fromIntegral char0 :: Int
      ci = utf16ColToCodepoints lineText ciUtf16
      isTokenChar ch =
        isAlphaNum ch || ch == '_' || ch == '.'
      charInBounds i = i >= 0 && i < len
      at i = T.index lineText i
      isTok = isTokenChar

      -- Prefer the character under the cursor; if it's not a token, fall back
      -- to the character just before the cursor (common when hovering at end).
      startIx
        | charInBounds ci && isTok (at ci) = Just ci
        | charInBounds (ci - 1) && isTok (at (ci - 1)) = Just (ci - 1)
        | otherwise = Nothing

  ix <- startIx

  let goLeft i
        | i <= 0 = 0
        | isTok (at (i - 1)) = goLeft (i - 1)
        | otherwise = i

      goRight i
        | i >= len = len
        | isTok (at i) = goRight (i + 1)
        | otherwise = i

      left = goLeft ix
      right = goRight (ix + 1)
      tok = T.take (right - left) (T.drop left lineText)
      sp =
        Span
          { spanStartLine = li + 1
          , spanStartCol = left + 1
          , spanEndLine = li + 1
          , spanEndCol = right + 1
          }
      range = spanToRange doc sp

  if T.null tok then Nothing else Just (range, tok)

-- =============================================================================
-- Lune diagnostics
-- =============================================================================

data LuneDiag = LuneDiag
  { luneDiagSpan :: Maybe Span
  , luneDiagSeverity :: DiagnosticSeverity
  , luneDiagMessage :: Text
  }

data CheckResult = CheckResult
  { crDiagnostics :: [LuneDiag]
  , crSemantics :: [SemanticInfo]
  }

toLspDiagnostic :: Text -> LuneDiag -> Diagnostic
toLspDiagnostic doc (LuneDiag sp sev msg) =
  let range =
        case sp of
          Nothing -> Range (Position 0 0) (Position 0 0)
          Just s -> spanToRange doc s
   in Diagnostic
        range
        (Just sev)
        Nothing
        Nothing
        (Just "lune")
        msg
        Nothing
        Nothing
        Nothing

checkFileWithSemantic :: IORef LspState -> FilePath -> Text -> IO CheckResult
checkFileWithSemantic stateRef path contents =
  handleExceptions $ do
    let baseLoader = mkLspModuleLoader stateRef
    srcCacheRef <- newIORef (Map.singleton path contents)
    let loader =
          ModuleLoader
            { mlReadFileText = \p -> do
                res <- mlReadFileText baseLoader p
                case res of
                  Right txt ->
                    atomicModifyIORef' srcCacheRef (\m -> (Map.insert p txt m, ()))
                  Left _ ->
                    pure ()
                pure res
            }
    case parseModuleFromText path contents of
      Left bundle ->
        pure CheckResult {crDiagnostics = [diagFromParseBundle bundle], crSemantics = []}
      Right m -> do
        loaded <- MG.loadProgramWithEntryModuleUsing loader path m
        case loaded of
          Left err ->
            pure CheckResult {crDiagnostics = [LuneDiag Nothing DiagnosticSeverity_Error (T.pack (show err))], crSemantics = []}
          Right prog ->
            case Resolve.resolveProgram prog of
              Left err ->
                pure CheckResult {crDiagnostics = [LuneDiag Nothing DiagnosticSeverity_Error (T.pack (show err))], crSemantics = []}
              Right resolvedMods -> do
                let order = MG.progOrder prog
                resolvedModulesInOrder <-
                  case traverse (`Map.lookup` resolvedMods) order of
                    Nothing ->
                      pure (Left "resolveProgram did not return all modules in progOrder")
                    Just ms ->
                      pure (Right ms)

                case resolvedModulesInOrder of
                  Left msg ->
                    pure CheckResult {crDiagnostics = [LuneDiag Nothing DiagnosticSeverity_Error msg], crSemantics = []}
                  Right ms -> do
                    let combined =
                          S.Module
                            { S.modName = MG.progEntryName prog
                            , S.modExports = []
                            , S.modImports = []
                            , S.modDecls = concatMap S.modDecls ms
                            }
                        mod' = desugarModule combined
                    case validateModule mod' of
                      Left err ->
                        pure CheckResult {crDiagnostics = [LuneDiag Nothing DiagnosticSeverity_Error (T.pack (show err))], crSemantics = []}
                      Right () ->
                        case Check.typecheckModuleEnv mod' of
                          Left err ->
                            pure CheckResult {crDiagnostics = [LuneDiag Nothing DiagnosticSeverity_Error (T.pack (show err))], crSemantics = []}
                          Right valueEnv -> do
                            let progModules = MG.progModules prog

                            srcCache <- readIORef srcCacheRef
                            docsByModule <-
                              fmap Map.fromList $
                                traverse
                                  ( \modName -> do
                                      let emptyTable = Docs.DocTable {Docs.dtModuleDoc = Nothing, Docs.dtDeclDocs = Map.empty}
                                      case Map.lookup modName progModules of
                                        Nothing ->
                                          pure (modName, emptyTable)
                                        Just lm -> do
                                          let srcOrErr =
                                                if MG.lmPath lm == path
                                                  then Right contents
                                                  else case Map.lookup (MG.lmPath lm) srcCache of
                                                    Just src -> Right src
                                                    Nothing -> Left "missing source"
                                          let table =
                                                case srcOrErr of
                                                  Left _ -> emptyTable
                                                  Right src -> Docs.extractDocTable src
                                          pure (modName, table)
                                  )
                                  order

                            let semantics =
                                  [ let table = Map.findWithDefault (Docs.DocTable Nothing Map.empty) modName docsByModule
                                    in SemanticInfo
                                          { siModuleName = modName
                                          , siPath = MG.lmPath lm
                                          , siVersion = 0
                                          , siTypesAt = []
                                          , siValueEnv = Map.filterWithKey (\k _ -> (modName <> ".") `T.isPrefixOf` k) valueEnv
                                          , siDocs = Docs.dtDeclDocs table
                                          , siModuleDoc = Docs.dtModuleDoc table
                                          }
                                  | modName <- order
                                  , Just lm <- [Map.lookup modName progModules]
                                  ]
                            pure CheckResult {crDiagnostics = [], crSemantics = semantics}
  where
    handleExceptions action = do
      res <- (try action :: IO (Either SomeException CheckResult))
      case res of
        Right r ->
          pure r
        Left e -> do
          hPutStrLn stderr ("lune: LSP checkFile failed: " <> displayException e)
          pure
            CheckResult
              { crDiagnostics = [LuneDiag Nothing DiagnosticSeverity_Error ("Internal error: " <> T.pack (displayException e))]
              , crSemantics = []
              }

diagFromParseBundle :: ParseErrorBundle Text Void -> LuneDiag
diagFromParseBundle bundle =
  let firstErr = NE.head (bundleErrors bundle)
      offset = errorOffset firstErr
      (_, posState) = reachOffset offset (bundlePosState bundle)
      sp = spanFromSourcePos (pstateSourcePos posState)
   in LuneDiag (Just sp) DiagnosticSeverity_Error (T.pack (errorBundlePretty bundle))

parseModuleFromText :: FilePath -> Text -> Either (ParseErrorBundle Text Void) S.Module
parseModuleFromText = Parser.parseTextBundle

-- | Parse module to extract just name and imports.
-- Returns (Nothing, empty) if parsing fails.
parseModuleInfo :: FilePath -> Text -> (Maybe Text, Set Text, Map Text Text)
parseModuleInfo path contents =
  case Parser.parseTextBundle path contents of
    Left _ -> (Nothing, Set.empty, Map.empty)
    Right m ->
      let imports = Set.fromList (map S.impName (S.modImports m))
          importAliases =
            Map.fromList
              [ (aliasFor imp, S.impName imp)
              | imp <- S.modImports m
              ]
       in (Just (S.modName m), imports, importAliases)
  where
    aliasFor imp =
      case S.impAs imp of
        Just a -> a
        Nothing -> defaultAlias (S.impName imp)

    defaultAlias name =
      last (T.splitOn "." name)

-- | Create a ModuleLoader that reads from open buffers first, then disk.
mkLspModuleLoader :: IORef LspState -> ModuleLoader
mkLspModuleLoader stateRef = ModuleLoader
  { mlReadFileText = \path -> do
      st <- readIORef stateRef
      case lookupOpenDocText path st of
        Just txt -> pure (Right txt)
        Nothing -> do
          result <- try (TIO.readFile path) :: IO (Either SomeException Text)
          pure $ case result of
            Left e -> Left (displayException e)
            Right t -> Right t
  }
