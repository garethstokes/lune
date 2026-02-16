{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lune.LSP.Server
  ( runStdio
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Data.IORef (newIORef)
import qualified Language.LSP.Server as LSP
import Language.LSP.Server
  ( Options (..)
  , ServerDefinition (..)
  , defaultOptions
  , runLspT
  , runServer
  )
import Language.LSP.Protocol.Message (TRequestMessage (..))
import Language.LSP.Protocol.Types
  ( InitializeParams (..)
  , ServerInfo (..)
  , TextDocumentSyncKind (TextDocumentSyncKind_Full)
  , TextDocumentSyncOptions (..)
  , Uri (..)
  , Null (..)
  , type (|?)(..)
  )
import Lune.LSP.Handlers (handlers)
import Lune.LSP.State (emptyLspState)
import System.IO
  ( BufferMode (NoBuffering)
  , hPutStrLn
  , hSetBinaryMode
  , hSetBuffering
  , stderr
  , stdin
  , stdout
  )

runStdio :: IO ()
runStdio = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hPutStrLn stderr "lune: starting LSP server (stdio)"
  stateRef <- newIORef emptyLspState

  let syncOptions =
        TextDocumentSyncOptions
          { _openClose = Just True
          , _change = Just TextDocumentSyncKind_Full
          , _willSave = Nothing
          , _willSaveWaitUntil = Nothing
          , _save = Nothing
          }

      opts =
        defaultOptions
          { optTextDocumentSync = Just syncOptions
          , optServerInfo = Just (ServerInfo {_name = "lune", _version = Nothing})
          }

      serverDefinition :: ServerDefinition ()
      serverDefinition =
        ServerDefinition
          { defaultConfig = ()
          , configSection = "lune"
          , parseConfig = \_cfg _v -> Right ()
          , onConfigChange = \_cfg -> pure ()
          , doInitialize = \env (TRequestMessage _ _ _ params) -> do
              let rootUriText =
                    case _rootUri params of
                      InR Null -> "<none>"
                      InL (Uri t) -> t
              hPutStrLn stderr ("lune: initialize (rootUri=" <> show rootUriText <> ")")
              pure (Right env)
          , staticHandlers = \_caps -> handlers stateRef
          , interpretHandler = \env -> LSP.Iso (runLspT env) liftIO
          , options = opts
          }

  void (runServer serverDefinition)
