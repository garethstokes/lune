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
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import qualified Data.List.NonEmpty as NE
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
import qualified Lune.Fmt as Fmt
import qualified Lune.ModuleGraph as MG
import qualified Lune.Parser as Parser
import qualified Lune.Resolve as Resolve
import qualified Lune.Syntax as S
import Lune.Validate (validateModule)
import Lune.LSP.Convert
  ( Span
  , filePathToUri'
  , fullDocumentRange
  , spanFromSourcePos
  , spanToRange
  , uriToFilePath'
  )
import Lune.LSP.State
  ( LspState
  , clearLastDiagnostics
  , lookupLastDiagnostics
  , lookupOpenDoc
  , removeOpenDoc
  , setLastDiagnostics
  , setOpenDoc
  )
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
      liftIO $ atomicModifyIORef' stateRef (\st -> (setOpenDoc path contents st, ()))
      publishCheckedDiagnostics stateRef path contents

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
          liftIO $ atomicModifyIORef' stateRef (\st -> (setOpenDoc path newText st, ()))
          publishCheckedDiagnostics stateRef path newText

handleDidClose :: IORef LspState -> TNotificationMessage 'Method_TextDocumentDidClose -> LspM () ()
handleDidClose stateRef (TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri))) = do
  case uriToFilePath' uri of
    Nothing ->
      pure ()
    Just path -> do
      liftIO $
        atomicModifyIORef' stateRef (\st -> (clearLastDiagnostics path (removeOpenDoc path st), ()))
      sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing [])

publishCheckedDiagnostics :: IORef LspState -> FilePath -> Text -> LspM () ()
publishCheckedDiagnostics stateRef path contents = do
  diags <- liftIO (checkFile path contents)
  let lspDiags = map (toLspDiagnostic contents) diags
      uri = filePathToUri' path
  liftIO $
    atomicModifyIORef' stateRef (\st -> (setLastDiagnostics path lspDiags st, ()))
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
        case lookupOpenDoc path st of
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
-- Lune diagnostics
-- =============================================================================

data LuneDiag = LuneDiag
  { luneDiagSpan :: Maybe Span
  , luneDiagSeverity :: DiagnosticSeverity
  , luneDiagMessage :: Text
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

checkFile :: FilePath -> Text -> IO [LuneDiag]
checkFile path contents =
  handleExceptions $ do
    case parseModuleFromText path contents of
      Left bundle ->
        pure [diagFromParseBundle bundle]
      Right m -> do
        loaded <- MG.loadProgramWithEntryModule path m
        case loaded of
          Left err ->
            pure [LuneDiag Nothing DiagnosticSeverity_Error (T.pack (show err))]
          Right prog ->
            case Resolve.resolveProgram prog of
              Left err ->
                pure [LuneDiag Nothing DiagnosticSeverity_Error (T.pack (show err))]
              Right resolved -> do
                let mod' = desugarModule resolved
                case validateModule mod' of
                  Left err ->
                    pure [LuneDiag Nothing DiagnosticSeverity_Error (T.pack (show err))]
                  Right () ->
                    case Check.typecheckModule mod' of
                      Left err ->
                        pure [LuneDiag Nothing DiagnosticSeverity_Error (T.pack (show err))]
                      Right _ ->
                        pure []
  where
    handleExceptions action = do
      res <- (try action :: IO (Either SomeException [LuneDiag]))
      case res of
        Right ds ->
          pure ds
        Left e -> do
          hPutStrLn stderr ("lune: LSP checkFile failed: " <> displayException e)
          pure [LuneDiag Nothing DiagnosticSeverity_Error ("Internal error: " <> T.pack (displayException e))]

diagFromParseBundle :: ParseErrorBundle Text Void -> LuneDiag
diagFromParseBundle bundle =
  let firstErr = NE.head (bundleErrors bundle)
      offset = errorOffset firstErr
      (_, posState) = reachOffset offset (bundlePosState bundle)
      sp = spanFromSourcePos (pstateSourcePos posState)
   in LuneDiag (Just sp) DiagnosticSeverity_Error (T.pack (errorBundlePretty bundle))

parseModuleFromText :: FilePath -> Text -> Either (ParseErrorBundle Text Void) S.Module
parseModuleFromText = Parser.parseTextBundle
