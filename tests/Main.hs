{-# LANGUAGE OverloadedStrings #-}

-- | Golden test suite for the Lune compiler.
--
-- Runs the compiler at various stages (parse, core, eval) and compares
-- output against committed snapshots. Negative tests verify that invalid
-- programs produce expected error messages.
--
-- Usage:
--   cabal test golden                    -- Run all tests
--   cabal test golden --test-options="--accept"  -- Accept new golden output
--   cabal test golden --test-options="-p '/Parse/'"  -- Run only parse tests

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket, finally)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value (..), (.=), encode, object)
import Data.Aeson.Types (parseMaybe, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (isSuffixOf, sort)
import Data.Char (isSpace, toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeBaseName)
import System.IO (BufferMode (NoBuffering), Handle, hClose, hFlush, hPutStrLn, hSetBinaryMode, hSetBuffering, hWaitForInput, openTempFile, stderr)
import System.Process
  ( CreateProcess (std_err, std_in, std_out)
  , ProcessHandle
  , StdStream (CreatePipe)
  , createProcess
  , proc
  , readProcessWithExitCode
  , terminateProcess
  , getProcessExitCode
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit (testCase, assertEqual)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8

main :: IO ()
main = do
  -- Discover example files
  examples <- discoverExamples "examples"
  invalidExamples <- discoverExamples "examples/invalid"
  fmtCases <- discoverFmtCases "tests/fmt"

  -- Build test tree
  defaultMain $ testGroup "Lune Golden Tests"
    [ testGroup "Parse" (map parseTest examples)
    , testGroup "Core" (map coreTest examples)
    , testGroup "Eval" (map evalTest examples)
    , testGroup "Negative" (map negativeTest invalidExamples)
    , testGroup "Fmt" (map fmtGoldenTest fmtCases)
    , testGroup "Fmt Idempotent" (map fmtIdempotentTest fmtCases)
    , testGroup "Fmt Check" (map fmtCheckTest fmtCases)
    , testGroup "LSP" [lspIntegrationTest]
    ]

-- | Discover .lune files in a directory, sorted for determinism.
discoverExamples :: FilePath -> IO [FilePath]
discoverExamples dir = do
  files <- listDirectory dir
  let luneFiles = filter (".lune" `isSuffixOf`) files
  pure $ map (dir </>) (sort luneFiles)

data FmtCase = FmtCase
  { fmtName :: String
  , fmtInputPath :: FilePath
  , fmtExpectedPath :: FilePath
  }

discoverFmtCases :: FilePath -> IO [FmtCase]
discoverFmtCases dir = do
  files <- listDirectory dir
  let inputFiles = sort (filter (".input.lune" `isSuffixOf`) files)
  pure $
    map
      ( \f ->
          let base0 = takeBaseName f
              base = dropSuffix ".input" base0
              inputPath = dir </> f
              expectedPath = dir </> (base ++ ".expected.lune")
           in FmtCase {fmtName = base, fmtInputPath = inputPath, fmtExpectedPath = expectedPath}
      )
      inputFiles
  where
    dropSuffix suf s =
      case reverse suf `stripPrefix` reverse s of
        Just rest -> reverse rest
        Nothing -> s

    stripPrefix [] ys = Just ys
    stripPrefix (x : xs) (y : ys)
      | x == y = stripPrefix xs ys
    stripPrefix _ _ = Nothing

-- | Run the lune compiler with given flags.
runLune :: [String] -> FilePath -> IO (ExitCode, String, String)
runLune flags file =
  readProcessWithExitCode "cabal" (["run", "-v0", "lune", "--"] ++ flags ++ [file]) ""

-- | Normalize output for stable snapshots:
--   - Trim trailing whitespace per line
--   - Remove trailing empty lines
--   - Ensure single trailing newline
normalizeOutput :: String -> LBS.ByteString
normalizeOutput s =
  let ls = lines s
      trimmed = map trimTrailing ls
      nonEmpty = dropWhileEnd null trimmed
      result = if null nonEmpty then "" else unlines nonEmpty
  in LBS8.pack result
  where
    trimTrailing = reverse . dropWhile (== ' ') . reverse
    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Get golden file path for a given test.
goldenPath :: String -> FilePath -> FilePath
goldenPath stage sourceFile =
  "tests/golden" </> stage </> takeBaseName sourceFile ++ ".golden"

-- | Diff command for comparing outputs.
diffCmd :: FilePath -> FilePath -> [String]
diffCmd ref new = ["diff", "-u", ref, new]

rawOutput :: String -> LBS.ByteString
rawOutput =
  LBS8.pack

-- =============================================================================
-- Parse Tests
-- =============================================================================

-- | Test parsing a .lune file and snapshot the AST.
parseTest :: FilePath -> TestTree
parseTest file = goldenVsStringDiff testName diffCmd (goldenPath "parse" file) $ do
  (_, stdout, stderr) <- runLune [] file
  pure $ normalizeOutput (stderr ++ stdout)
  where
    testName = takeBaseName file

-- =============================================================================
-- Core Tests
-- =============================================================================

-- | Test Core IR elaboration and snapshot the output.
coreTest :: FilePath -> TestTree
coreTest file = goldenVsStringDiff testName diffCmd (goldenPath "core" file) $ do
  (_, stdout, stderr) <- runLune ["--core"] file
  pure $ normalizeOutput (stderr ++ stdout)
  where
    testName = takeBaseName file

-- =============================================================================
-- Eval Tests
-- =============================================================================

-- | Test evaluation and snapshot the result/trace.
evalTest :: FilePath -> TestTree
evalTest file = goldenVsStringDiff testName diffCmd (goldenPath "eval" file) $ do
  (_, stdout, stderr) <- runLune ["--eval"] file
  pure $ normalizeOutput (stderr ++ stdout)
  where
    testName = takeBaseName file

-- =============================================================================
-- Negative Tests
-- =============================================================================

-- | Test that invalid programs produce expected error messages.
negativeTest :: FilePath -> TestTree
negativeTest file = goldenVsStringDiff testName diffCmd (goldenPath "neg" file) $ do
  (exitCode, stdout, stderr) <- runLune ["--typecheck"] file
  let output = case exitCode of
        ExitSuccess ->
          "UNEXPECTED SUCCESS: Expected compilation to fail but it succeeded.\n" ++ stdout
        ExitFailure _ ->
          stderr ++ stdout
  pure $ normalizeOutput output
  where
    testName = takeBaseName file

-- =============================================================================
-- Fmt Tests
-- =============================================================================

fmtGoldenTest :: FmtCase -> TestTree
fmtGoldenTest c =
  goldenVsStringDiff (fmtName c) diffCmd (fmtExpectedPath c) $ do
    (_, stdout, stderr) <- runLune ["--fmt", "--stdout"] (fmtInputPath c)
    pure $ rawOutput (stderr ++ stdout)

fmtIdempotentTest :: FmtCase -> TestTree
fmtIdempotentTest c =
  testCase (fmtName c) $ do
    expected <- readFile (fmtExpectedPath c)
    (_, stdout, stderr) <- runLune ["--fmt", "--stdout"] (fmtExpectedPath c)
    assertEqual "fmt(fmt(expected)) == expected" expected (stderr ++ stdout)

fmtCheckTest :: FmtCase -> TestTree
fmtCheckTest c =
  testCase (fmtName c) $ do
    (exitIn, _outIn, _errIn) <- runLune ["--fmt", "--check"] (fmtInputPath c)
    (exitEx, _outEx, _errEx) <- runLune ["--fmt", "--check"] (fmtExpectedPath c)
    assertEqual "fmt --check input fails" False (exitIn == ExitSuccess)
    assertEqual "fmt --check expected succeeds" True (exitEx == ExitSuccess)

-- =============================================================================
-- LSP Tests
-- =============================================================================

lspIntegrationTest :: TestTree
lspIntegrationTest =
  testCase "DiagnosticsAndFormatting" $ do
    logMsg "starting"
    luneBin <- getLuneBin
    logMsg ("luneBin=" <> luneBin)
    withLspServer luneBin $ \hin hout ph -> do
      logMsg "server started"
      -- Initialize handshake
      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "id" .= (1 :: Int)
          , "method" .= ("initialize" :: T.Text)
          , "params"
              .= object
                [ "processId" .= (Nothing :: Maybe Int)
                , "rootUri" .= (Nothing :: Maybe T.Text)
                , "capabilities" .= object []
                ]
          ]

      logMsg "sent initialize"
      _initResp <- waitForResponseId 1 hout
      logMsg "got initialize response"

      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("initialized" :: T.Text)
          , "params" .= object []
          ]

      -- Diagnostics: open a document with a parse error.
      let badText =
            T.unlines
              [ "module Bad exposing (x)"
              , ""
              , "x ="
              ]

      badUri <- tempFileUri "lune-lsp-bad" ".lune"
      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("textDocument/didOpen" :: T.Text)
          , "params"
              .= object
                [ "textDocument"
                    .= object
                      [ "uri" .= badUri
                      , "languageId" .= ("lune" :: T.Text)
                      , "version" .= (0 :: Int)
                      , "text" .= badText
                      ]
                ]
          ]

      logMsg "sent didOpen (bad doc)"
      badDiags <- waitForPublishDiagnostics badUri hout
      logMsg ("got diagnostics (count=" <> show (length badDiags) <> ")")
      assertEqual "parse error publishes diagnostics" False (null badDiags)

      -- Formatting: compare LSP formatting output to `lune --fmt --stdout`.
      logMsg "starting formatting test"
      fmtInput <- TIO.readFile "tests/fmt/layout.input.lune"
      withTempFile "lune-lsp-format" ".lune" $ \tmpPath -> do
        TIO.writeFile tmpPath fmtInput
        let tmpUri = fileUri tmpPath

        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("textDocument/didOpen" :: T.Text)
            , "params"
                .= object
                  [ "textDocument"
                      .= object
                        [ "uri" .= tmpUri
                        , "languageId" .= ("lune" :: T.Text)
                        , "version" .= (0 :: Int)
                        , "text" .= fmtInput
                        ]
                  ]
            ]

        logMsg "sent didOpen (format doc)"
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (2 :: Int)
            , "method" .= ("textDocument/formatting" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= tmpUri]
                  , "options"
                      .= object
                        [ "tabSize" .= (2 :: Int)
                        , "insertSpaces" .= True
                        ]
                  ]
            ]

        logMsg "sent formatting request"
        fmtResult <- waitForResponseId 2 hout
        logMsg "got formatting response"
        let lspFormatted =
              case extractFirstEditNewText fmtResult of
                Nothing -> error "missing formatting result edit newText"
                Just t -> t

        (ecFmt, stdoutFmt, _stderrFmt) <- readProcessWithExitCode luneBin ["--fmt", "--stdout", tmpPath] ""
        logMsg ("ran cli formatter (exit=" <> show ecFmt <> ")")
        assertEqual "lune --fmt succeeds" ExitSuccess ecFmt
        assertEqual "LSP formatting matches CLI" (T.pack stdoutFmt) lspFormatted

      -- Shutdown/exit
      logMsg "sending shutdown"
      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "id" .= (3 :: Int)
          , "method" .= ("shutdown" :: T.Text)
          , "params" .= Null
          ]
      _ <- waitForResponseId 3 hout
      logMsg "got shutdown response"
      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("exit" :: T.Text)
          , "params" .= Null
          ]

      logMsg "sent exit"

      mExit <- waitForExitCode 5000000 ph
      case mExit of
        Just _ ->
          pure ()
        Nothing -> do
          terminateProcess ph
          _ <- waitForExitCode 2000000 ph
          pure ()

logMsg :: String -> IO ()
logMsg msg = do
  dbg <- lookupEnv "LUNE_LSP_TEST_DEBUG"
  case dbg of
    Nothing -> pure ()
    Just _ -> do
      hPutStrLn stderr ("[LSP test] " <> msg)
      hFlush stderr

getLuneBin :: IO FilePath
getLuneBin = do
  (ec, out, err) <- readProcessWithExitCode "cabal" ["list-bin", "lune"] ""
  case ec of
    ExitSuccess ->
      case lines out of
        (p : _) -> pure p
        [] -> error "cabal list-bin lune returned empty output"
    ExitFailure _ ->
      error ("cabal list-bin lune failed: " <> err)

withLspServer :: FilePath -> (Handle -> Handle -> ProcessHandle -> IO a) -> IO a
withLspServer luneBin action =
  bracket acquire release use
  where
    acquire = do
      (mIn, mOut, mErr, ph) <-
        createProcess
          (proc luneBin ["lsp", "--stdio"])
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
      hin <- maybe (error "failed to open stdin pipe") pure mIn
      hout <- maybe (error "failed to open stdout pipe") pure mOut
      herr <- maybe (error "failed to open stderr pipe") pure mErr
      hSetBinaryMode hin True
      hSetBinaryMode hout True
      hSetBinaryMode herr True
      hSetBuffering hin NoBuffering
      hSetBuffering hout NoBuffering
      hSetBuffering herr NoBuffering
      _ <- forkIO (drainStderr herr)
      pure (hin, hout, herr, ph)

    release (hin, hout, herr, ph) = do
      hClose hin `finally` hClose hout `finally` hClose herr
      terminateProcess ph
      _ <- waitForExitCode 5000000 ph
      pure ()

    use (hin, hout, _herr, ph) =
      action hin hout ph

drainStderr :: Handle -> IO ()
drainStderr h = do
  chunk <- BS.hGetSome h 4096
  if BS.null chunk then pure () else drainStderr h

sendLsp :: Handle -> Value -> IO ()
sendLsp h v = do
  let body = encode v
      len = LBS.length body
      header = "Content-Length: " <> show len <> "\r\n\r\n"
  BS8.hPutStr h (BS8.pack header)
  LBS.hPutStr h body
  hFlush h

recvLsp :: Handle -> IO Value
recvLsp h = do
  len <- readHeaders Nothing
  body <- hGetExact len
  case Aeson.eitherDecodeStrict' body of
    Left e -> error ("failed to decode LSP message: " <> e)
    Right v -> pure v
  where
    readHeaders mLen = do
      ok <- hWaitForInput h 5000
      if not ok then error "timeout waiting for LSP headers" else pure ()
      line0 <- BS8.hGetLine h
      let line = BS8.takeWhile (/= '\r') line0
      if BS8.null line
        then case mLen of
          Just n -> pure n
          Nothing -> error "missing Content-Length header"
        else
          case BS8.break (== ':') line of
            (k, rest) ->
              let k' = BS8.map toLower k
               in if k' == "content-length"
                    then
                      let v = BS8.dropWhile isSpace (BS8.drop 1 rest)
                       in readHeaders (Just (read (BS8.unpack v)))
                    else readHeaders mLen

    hGetExact n0 =
      go n0 []
      where
        go 0 acc =
          pure (BS.concat (reverse acc))
        go n acc = do
          ok <- hWaitForInput h 5000
          if not ok then error "timeout waiting for LSP body" else pure ()
          chunk <- BS.hGetSome h n
          if BS.null chunk
            then error "unexpected EOF while reading LSP body"
            else go (n - BS.length chunk) (chunk : acc)

waitForResponseId :: Int -> Handle -> IO Value
waitForResponseId wanted h = do
  m <- timeout 5000000 (go)
  case m of
    Nothing -> error ("timeout waiting for response id " <> show wanted)
    Just v -> pure v
  where
    go = do
      msg <- recvLsp h
      case matchResponse wanted msg of
        Just r -> pure r
        Nothing -> go

matchResponse :: Int -> Value -> Maybe Value
matchResponse wanted =
  parseMaybe $ \v -> do
    o <- case v of
      Object o -> pure o
      _ -> fail "not an object"
    i <- o .: "id"
    if (i :: Int) == wanted
      then o .: "result"
      else fail "not the id"

waitForPublishDiagnostics :: T.Text -> Handle -> IO [Value]
waitForPublishDiagnostics wantedUri h = do
  m <- timeout 5000000 go
  case m of
    Nothing -> error "timeout waiting for publishDiagnostics"
    Just diags -> pure diags
  where
    go = do
      msg <- recvLsp h
      case matchPublishDiagnostics wantedUri msg of
        Just ds -> pure ds
        Nothing -> go

matchPublishDiagnostics :: T.Text -> Value -> Maybe [Value]
matchPublishDiagnostics wantedUri =
  parseMaybe $ \v -> do
    o <- case v of
      Object o -> pure o
      _ -> fail "not an object"
    method <- o .: "method"
    if (method :: T.Text) /= "textDocument/publishDiagnostics"
      then fail "not publishDiagnostics"
      else do
        params <- o .: "params"
        uri <- params .: "uri"
        if (uri :: T.Text) /= wantedUri
          then fail "uri mismatch"
          else params .: "diagnostics"

extractFirstEditNewText :: Value -> Maybe T.Text
extractFirstEditNewText =
  parseMaybe $ \v -> do
    edits <- case v of
      Array _ -> Aeson.parseJSON v
      _ -> fail "formatting result not array"
    case (edits :: [Value]) of
      (Object e : _) ->
        e .: "newText"
      _ ->
        fail "missing edit"

fileUri :: FilePath -> T.Text
fileUri path =
  T.pack ("file://" <> path)

tempFileUri :: String -> String -> IO T.Text
tempFileUri prefix suffix = do
  tmp <- getTemporaryDirectory
  (p, h) <- openTempFile tmp (prefix <> suffix)
  hClose h
  removeFile p
  pure (fileUri p)

withTempFile :: String -> String -> (FilePath -> IO a) -> IO a
withTempFile prefix suffix action = do
  tmp <- getTemporaryDirectory
  bracket
    (do
      (p, h) <- openTempFile tmp (prefix <> suffix)
      hClose h
      pure p
    )
    removeFile
    action

waitForExitCode :: Int -> ProcessHandle -> IO (Maybe ExitCode)
waitForExitCode timeoutMicros ph =
  go timeoutMicros
  where
    step = 50000

    go remaining = do
      ec <- getProcessExitCode ph
      case ec of
        Just _ ->
          pure ec
        Nothing ->
          if remaining <= 0
            then pure Nothing
            else do
              let sleepFor = min step remaining
              threadDelay sleepFor
              go (remaining - sleepFor)
