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
import Control.Exception (SomeException, bracket, finally, try)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value (..), (.=), encode, object)
import Data.Aeson.Types (parseMaybe, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (isSuffixOf, sort)
import Data.Char (isSpace, toLower, ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.Directory (getTemporaryDirectory, removeFile, getCurrentDirectory)
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
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  -- Discover example files
  examples <- discoverExamples "examples"
  invalidExamples <- discoverExamples "examples/invalid"
  fmtCases <- discoverFmtCases "tests/fmt"

  let interactiveExamples =
        [ "50_HealthAgent"
        ]

      shouldSkipInteractive file =
        takeBaseName file `elem` interactiveExamples

      parseExamples = filter (not . shouldSkipInteractive) examples
      evalExamples = filter (not . shouldSkipInteractive) examples

  -- Build test tree
  defaultMain $ testGroup "Lune Golden Tests"
    [ testGroup "Parse" (map parseTest parseExamples)
    , testGroup "Core" (map coreTest examples)
    , testGroup "Eval" (map evalTest evalExamples)
    , testGroup "Negative" (map negativeTest invalidExamples)
    , testGroup "Fmt" (map fmtGoldenTest fmtCases)
    , testGroup "Fmt Idempotent" (map fmtIdempotentTest fmtCases)
    , testGroup "Fmt Check" (map fmtCheckTest fmtCases)
    , testGroup "LSP" [lspIntegrationTest, hoverIntegrationTest, typedHoverIntegrationTest, unicodeHoverIntegrationTest, hoverRetentionIntegrationTest, vfsImportPropagationTest]
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

hoverIntegrationTest :: TestTree
hoverIntegrationTest =
  testCase "HoverIdentifier" $ do
    logMsg "starting hover test"
    luneBin <- getLuneBin
    withLspServer luneBin $ \hin hout _ph -> do
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

      initResp <- waitForResponseId 1 hout
      let hoverProvider =
            (parseMaybe $ \v -> do
              o <- case v of
                Object o -> pure o
                _ -> fail "initialize result not object"
              caps <- o .: "capabilities"
              caps .: "hoverProvider"
            ) initResp :: Maybe Value
      assertEqual "hoverProvider is advertised" True (hoverProvider /= Nothing)

      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("initialized" :: T.Text)
          , "params" .= object []
          ]

      let diskText =
            T.unlines
              [ "module A exposing (main, foo)"
              , ""
              , "foo : Int"
              , "foo = 1"
              , ""
              , "main : Int"
              , "main = foo"
              ]
          vfsText =
            T.unlines
              [ "module A exposing (main, bar)"
              , ""
              , "bar : Int"
              , "bar = 1"
              , ""
              , "main : Int"
              , "main = bar"
              ]

      withTempFile "lune-lsp-hover" ".lune" $ \tmpPath -> do
        TIO.writeFile tmpPath diskText
        let tmpUri = fileUri tmpPath
            hoverPos = object ["line" .= (6 :: Int), "character" .= (8 :: Int)]

        -- Hover over disk-backed document (not opened).
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (2 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= tmpUri]
                  , "position" .= hoverPos
                  ]
            ]

        diskHover <- waitForResponseId 2 hout
        let diskHoverJson = TE.decodeUtf8 (LBS.toStrict (encode diskHover))
        assertEqual "disk hover contains foo" True ("foo" `T.isInfixOf` diskHoverJson)

        -- Now open the same URI with different contents (VFS should win).
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
                        , "text" .= vfsText
                        ]
                  ]
            ]

        _ <- waitForPublishDiagnostics tmpUri hout

        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (3 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= tmpUri]
                  , "position" .= hoverPos
                  ]
            ]

        vfsHover <- waitForResponseId 3 hout
        let vfsHoverJson = TE.decodeUtf8 (LBS.toStrict (encode vfsHover))
        assertEqual "VFS hover contains bar" True ("bar" `T.isInfixOf` vfsHoverJson)

      -- Shutdown/exit
      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "id" .= (4 :: Int)
          , "method" .= ("shutdown" :: T.Text)
          , "params" .= Null
          ]
      _ <- waitForResponseId 4 hout
      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("exit" :: T.Text)
          , "params" .= Null
          ]

typedHoverIntegrationTest :: TestTree
typedHoverIntegrationTest =
  testCase "HoverTypes" $ do
    logMsg "starting typed hover test"
    luneBin <- getLuneBin

    withTempDirectory "lune-lsp-typed-hover" $ \tmpDir -> do
      let pathA = tmpDir </> "A.lune"
          pathB = tmpDir </> "B.lune"
          pathDecode = tmpDir </> "Lune" </> "Json" </> "Decode.lune"
          uriA = fileUri pathA
          uriB = fileUri pathB

          textB =
            T.unlines
              [ "module B exposing (bar)"
              , ""
              , "bar = 1"
              ]

          textBBroken =
            T.unlines
              [ "module B exposing (bar)"
              , ""
              , "bar = \"x\""
              ]

          textA =
            T.unlines
              [ "module A exposing (main, usesB, usesField, usesAlias)"
              , ""
              , "import B"
              , "import Lune.Json.Decode"
              , "import Lune.Json.Decode as D"
              , ""
              , "usesB = B.bar"
              , "usesField = Decode.field"
              , "usesAlias = D.field"
              , "usesLong = Decode.longArrows"
              , "main = usesB"
              ]

          hoverBBarPos = object ["line" .= (6 :: Int), "character" .= (10 :: Int)]
          hoverDecodeFieldPos = object ["line" .= (7 :: Int), "character" .= (20 :: Int)]
          hoverAliasFieldPos = object ["line" .= (8 :: Int), "character" .= (15 :: Int)]
          hoverLongArrowsPos = object ["line" .= (9 :: Int), "character" .= (20 :: Int)]

      TIO.writeFile pathB textB
      createDirectoryIfMissing True (tmpDir </> "Lune" </> "Json")
      TIO.writeFile pathDecode $
        T.unlines
          [ "module Lune.Json.Decode exposing (Decoder, field, longArrows)"
          , ""
          , "import Lune.Json exposing (Json)"
          , ""
          , "type alias Decoder a = Json -> a"
          , ""
          , "-- | Extract a field from a JSON object."
          , "--   Fails if missing."
          , "field : String -> Decoder a -> Decoder b -> Decoder b"
          , "field _ _ fallback ="
          , "  fallback"
          , ""
          , "longArrows : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int"
          , "longArrows a b c d e f g h i j k l m ="
          , "  a"
          ]
      TIO.writeFile pathA textA

      withLspServer luneBin $ \hin hout _ph -> do
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

        _ <- waitForResponseId 1 hout

        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("initialized" :: T.Text)
            , "params" .= object []
            ]

        -- Open A (B stays disk-backed).
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("textDocument/didOpen" :: T.Text)
            , "params"
                .= object
                  [ "textDocument"
                      .= object
                        [ "uri" .= uriA
                        , "languageId" .= ("lune" :: T.Text)
                        , "version" .= (0 :: Int)
                        , "text" .= textA
                        ]
                  ]
            ]

        diagsA1 <- waitForPublishDiagnostics uriA hout
        assertEqual "A has no initial errors" 0 (length diagsA1)

        -- Hover qualified identifier from non-open module (B.bar).
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (2 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= uriA]
                  , "position" .= hoverBBarPos
                  ]
            ]

        hoverBBar1 <- waitForResponseId 2 hout
        let hoverBBar1Md = fromMaybe "" (extractHoverMarkdown hoverBBar1)
        assertEqual "hover B.bar contains ```lune" True ("```lune" `T.isInfixOf` hoverBBar1Md)
        assertEqual "hover B.bar contains B.bar" True ("B.bar" `T.isInfixOf` hoverBBar1Md)
        assertEqual "hover B.bar contains Int" True ("Int" `T.isInfixOf` hoverBBar1Md)

        -- Hover qualified identifier via default import alias (Decode.field).
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (3 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= uriA]
                  , "position" .= hoverDecodeFieldPos
                  ]
            ]

        hoverDecodeField <- waitForResponseId 3 hout
        let hoverDecodeFieldMd = fromMaybe "" (extractHoverMarkdown hoverDecodeField)
        assertEqual "hover Decode.field contains ```lune" True ("```lune" `T.isInfixOf` hoverDecodeFieldMd)
        assertEqual "hover Decode.field contains field :" True ("field :" `T.isInfixOf` hoverDecodeFieldMd)
        assertEqual "hover Decode.field contains Extract a field" True ("Extract a field" `T.isInfixOf` hoverDecodeFieldMd)

        -- Hover qualified identifier via explicit alias (D.field).
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (4 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= uriA]
                  , "position" .= hoverAliasFieldPos
                  ]
            ]

        hoverAliasField <- waitForResponseId 4 hout
        let hoverAliasFieldMd = fromMaybe "" (extractHoverMarkdown hoverAliasField)
        assertEqual "hover D.field contains ```lune" True ("```lune" `T.isInfixOf` hoverAliasFieldMd)
        assertEqual "hover D.field contains Extract a field" True ("Extract a field" `T.isInfixOf` hoverAliasFieldMd)

        -- Hover long arrow chain; should be wrapped across lines.
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (7 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= uriA]
                  , "position" .= hoverLongArrowsPos
                  ]
            ]

        hoverLongArrows <- waitForResponseId 7 hout
        let hoverLongArrowsMd = fromMaybe "" (extractHoverMarkdown hoverLongArrows)
        assertEqual "hover Decode.longArrows contains field :" True ("longArrows :" `T.isInfixOf` hoverLongArrowsMd)
        assertEqual "hover Decode.longArrows wraps arrows" True ("\n  ->" `T.isInfixOf` hoverLongArrowsMd)

        -- Open B with disk contents, then change it in-memory; A's hover should update.
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("textDocument/didOpen" :: T.Text)
            , "params"
                .= object
                  [ "textDocument"
                      .= object
                        [ "uri" .= uriB
                        , "languageId" .= ("lune" :: T.Text)
                        , "version" .= (0 :: Int)
                        , "text" .= textB
                        ]
                  ]
            ]

        diagsB1 <- waitForPublishDiagnostics uriB hout
        diagsA2 <- waitForPublishDiagnostics uriA hout
        assertEqual "B has no errors after open" 0 (length diagsB1)
        assertEqual "A has no errors after B open" 0 (length diagsA2)

        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("textDocument/didChange" :: T.Text)
            , "params"
                .= object
                  [ "textDocument"
                      .= object
                        [ "uri" .= uriB
                        , "version" .= (1 :: Int)
                        ]
                  , "contentChanges" .= [object ["text" .= textBBroken]]
                  ]
            ]

        diagsB2 <- waitForPublishDiagnostics uriB hout
        diagsA3 <- waitForPublishDiagnostics uriA hout
        assertEqual "B has no errors after change" 0 (length diagsB2)
        assertEqual "A has no errors after B change" 0 (length diagsA3)

        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (5 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= uriA]
                  , "position" .= hoverBBarPos
                  ]
            ]

        hoverBBar2 <- waitForResponseId 5 hout
        let hoverBBar2Json = TE.decodeUtf8 (LBS.toStrict (encode hoverBBar2))
        assertEqual "hover B.bar contains String after B change" True ("String" `T.isInfixOf` hoverBBar2Json)

        -- Shutdown/exit
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (6 :: Int)
            , "method" .= ("shutdown" :: T.Text)
            , "params" .= Null
            ]
        _ <- waitForResponseId 6 hout
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("exit" :: T.Text)
            , "params" .= Null
            ]

unicodeHoverIntegrationTest :: TestTree
unicodeHoverIntegrationTest =
  testCase "HoverUtf16Unicode" $ do
    logMsg "starting unicode hover test"
    luneBin <- getLuneBin
    cwd <- getCurrentDirectory
    let fixturePath = cwd </> "tests" </> "lsp" </> "test_unicode.lune"
    fixtureText <- TIO.readFile fixturePath

    withLspServer luneBin $ \hin hout _ph -> do
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

      _ <- waitForResponseId 1 hout

      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("initialized" :: T.Text)
          , "params" .= object []
          ]

      let uri = fileUri fixturePath

      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("textDocument/didOpen" :: T.Text)
          , "params"
              .= object
                [ "textDocument"
                    .= object
                      [ "uri" .= uri
                      , "languageId" .= ("lune" :: T.Text)
                      , "version" .= (0 :: Int)
                      , "text" .= fixtureText
                      ]
                ]
          ]

      diags <- waitForPublishDiagnostics uri hout
      assertEqual "unicode file has no errors" 0 (length diags)

      let ls = T.splitOn "\n" fixtureText
          mainLine =
            case [(ix, l) | (ix, l) <- zip [0 :: Int ..] ls, "module " `T.isPrefixOf` l] of
              (x : _) -> x
              [] -> error "missing module header line in test_unicode.lune"
          (mainLineIx, mainLineText) = mainLine
          (prefix, rest) = T.breakOn "main" mainLineText
          mainStartCp =
            if T.null rest
              then error "missing token `main` on module header line"
              else T.length prefix
          -- Hover at the start of the final character in `main` so an incorrect
          -- UTF-16/codepoint conversion lands outside the token.
          hoverCharUtf16 = codePointsToUtf16Col mainLineText (mainStartCp + 3)

      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "id" .= (2 :: Int)
          , "method" .= ("textDocument/hover" :: T.Text)
          , "params"
              .= object
                [ "textDocument" .= object ["uri" .= uri]
                , "position" .= object ["line" .= mainLineIx, "character" .= hoverCharUtf16]
                ]
          ]

      hoverRes <- waitForResponseId 2 hout
      let md = fromMaybe "" (extractHoverMarkdown hoverRes)
      assertEqual "unicode hover includes type block" True ("```lune" `T.isInfixOf` md)
      assertEqual "unicode hover includes `main : String`" True ("main : String" `T.isInfixOf` md)

      -- Shutdown/exit
      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "id" .= (3 :: Int)
          , "method" .= ("shutdown" :: T.Text)
          , "params" .= Null
          ]
      _ <- waitForResponseId 3 hout
      sendLsp hin $
        object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("exit" :: T.Text)
          , "params" .= Null
          ]

hoverRetentionIntegrationTest :: TestTree
hoverRetentionIntegrationTest =
  testCase "HoverRetentionAcrossSyntaxError" $ do
    logMsg "starting hover retention test"
    luneBin <- getLuneBin
    withTempFile "lune-lsp-hover-retain" ".lune" $ \tmpPath -> do
      let uri = fileUri tmpPath
          goodText =
            T.unlines
              [ "module A exposing (main)"
              , ""
              , "main : String"
              , "main = \"ok\""
              , ""
              , "other = 1"
              ]
          brokenText =
            T.unlines
              [ "module A exposing (main)"
              , ""
              , "main : String"
              , "main = \"ok\""
              , ""
              , "other = ("
              ]

          mainLineIx =
            case [ix | (ix, l) <- zip [0 :: Int ..] (T.splitOn "\n" goodText), "main =" `T.isPrefixOf` l] of
              (ix : _) -> ix
              [] -> error "missing `main =` line in hover retention test"

          hoverPos = object ["line" .= mainLineIx, "character" .= (1 :: Int)]

      withLspServer luneBin $ \hin hout _ph -> do
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

        _ <- waitForResponseId 1 hout

        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("initialized" :: T.Text)
            , "params" .= object []
            ]

        -- Open good document and verify hover works.
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("textDocument/didOpen" :: T.Text)
            , "params"
                .= object
                  [ "textDocument"
                      .= object
                        [ "uri" .= uri
                        , "languageId" .= ("lune" :: T.Text)
                        , "version" .= (0 :: Int)
                        , "text" .= goodText
                        ]
                  ]
            ]

        diags0 <- waitForPublishDiagnostics uri hout
        assertEqual "good doc has no errors" 0 (length diags0)

        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (2 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= uri]
                  , "position" .= hoverPos
                  ]
            ]

        hoverGood <- waitForResponseId 2 hout
        let hoverGoodMd = fromMaybe "" (extractHoverMarkdown hoverGood)
        assertEqual "initial hover includes type block" True ("```lune" `T.isInfixOf` hoverGoodMd)
        assertEqual "initial hover includes `main : String`" True ("main : String" `T.isInfixOf` hoverGoodMd)

        -- Introduce a syntax error elsewhere and ensure hover still uses last-good semantics.
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("textDocument/didChange" :: T.Text)
            , "params"
                .= object
                  [ "textDocument"
                      .= object
                        [ "uri" .= uri
                        , "version" .= (1 :: Int)
                        ]
                  , "contentChanges" .= [object ["text" .= brokenText]]
                  ]
            ]

        diagsBroken <- waitForPublishDiagnostics uri hout
        assertEqual "broken doc publishes diagnostics" True (length diagsBroken > 0)

        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (3 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= uri]
                  , "position" .= hoverPos
                  ]
            ]

        hoverBroken <- waitForResponseId 3 hout
        let hoverBrokenMd = fromMaybe "" (extractHoverMarkdown hoverBroken)
        assertEqual "hover still includes type block after syntax error" True ("```lune" `T.isInfixOf` hoverBrokenMd)
        assertEqual "hover still includes `main : String` after syntax error" True ("main : String" `T.isInfixOf` hoverBrokenMd)

        -- Fix the syntax error and ensure hover still works.
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("textDocument/didChange" :: T.Text)
            , "params"
                .= object
                  [ "textDocument"
                      .= object
                        [ "uri" .= uri
                        , "version" .= (2 :: Int)
                        ]
                  , "contentChanges" .= [object ["text" .= goodText]]
                  ]
            ]

        diagsFixed <- waitForPublishDiagnostics uri hout
        assertEqual "fixed doc is clean again" 0 (length diagsFixed)

        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (4 :: Int)
            , "method" .= ("textDocument/hover" :: T.Text)
            , "params"
                .= object
                  [ "textDocument" .= object ["uri" .= uri]
                  , "position" .= hoverPos
                  ]
            ]

        hoverFixed <- waitForResponseId 4 hout
        let hoverFixedMd = fromMaybe "" (extractHoverMarkdown hoverFixed)
        assertEqual "hover includes type block after fix" True ("```lune" `T.isInfixOf` hoverFixedMd)
        assertEqual "hover includes `main : String` after fix" True ("main : String" `T.isInfixOf` hoverFixedMd)

        -- Shutdown/exit
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (5 :: Int)
            , "method" .= ("shutdown" :: T.Text)
            , "params" .= Null
            ]
        _ <- waitForResponseId 5 hout
        sendLsp hin $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "method" .= ("exit" :: T.Text)
            , "params" .= Null
            ]

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
  -- Ensure the binary is up-to-date, since this test suite shells out to it.
  (ecBuild, _outBuild, errBuild) <- readProcessWithExitCode "cabal" ["build", "-v0", "exe:lune"] ""
  case ecBuild of
    ExitSuccess -> pure ()
    ExitFailure _ -> error ("cabal build exe:lune failed: " <> errBuild)
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
  chunkOrErr <- (try (BS.hGetSome h 4096) :: IO (Either SomeException BS.ByteString))
  case chunkOrErr of
    Left _ ->
      pure ()
    Right chunk ->
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

extractHoverMarkdown :: Value -> Maybe T.Text
extractHoverMarkdown =
  parseMaybe $ \v -> do
    o <- case v of
      Object o -> pure o
      _ -> fail "hover not an object"
    contents <- o .: "contents"
    c <- case contents of
      Object c -> pure c
      _ -> fail "hover.contents not an object"
    c .: "value"

codePointsToUtf16Col :: T.Text -> Int -> Int
codePointsToUtf16Col line codePointCol0 =
  T.foldl' (\acc ch -> acc + if ord ch <= 0xFFFF then 1 else 2) 0 (T.take codePointCol0 line)

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

withTempDirectory :: String -> (FilePath -> IO a) -> IO a
withTempDirectory prefix action = do
  tmpBase <- getTemporaryDirectory
  let dir = tmpBase </> prefix
  bracket
    (createDirectoryIfMissing True dir >> pure dir)
    removeDirectoryRecursive
    action

-- | Test that VFS-aware imports propagate diagnostics across modules.
-- When module B is changed (without saving), module A (which imports B)
-- should see updated diagnostics reflecting B's unsaved changes.
vfsImportPropagationTest :: TestTree
vfsImportPropagationTest =
  testCase "VfsImportPropagation" $ do
    logMsg "starting VFS propagation test"
    luneBin <- getLuneBin

    -- Create temp directory with two modules: A imports B
    withTempDirectory "lune-vfs-test" $ \tmpDir -> do
      let pathA = tmpDir </> "A.lune"
          pathB = tmpDir </> "B.lune"

      -- Write initial valid modules to disk
      -- Note: Using simple identity function to avoid needing prelude operators
      TIO.writeFile pathB $ T.unlines
        [ "module B exposing (helper)"
        , ""
        , "helper : Int -> Int"
        , "helper x = x"
        ]

      TIO.writeFile pathA $ T.unlines
        [ "module A exposing (main)"
        , ""
        , "import B"
        , ""
        , "main : Int"
        , "main = B.helper 42"
        ]

      withLspServer luneBin $ \hin hout _ph -> do
        -- Initialize
        sendLsp hin $ object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "id" .= (1 :: Int)
          , "method" .= ("initialize" :: T.Text)
          , "params" .= object
              [ "processId" .= (Nothing :: Maybe Int)
              , "rootUri" .= (Nothing :: Maybe T.Text)
              , "capabilities" .= object []
              ]
          ]
        _ <- waitForResponseId 1 hout

        sendLsp hin $ object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("initialized" :: T.Text)
          , "params" .= object []
          ]

        let uriA = fileUri pathA
            uriB = fileUri pathB

        -- Open both files (should have no errors)
        -- Note: Using simple identity function to avoid needing prelude operators
        let textA = T.unlines
              [ "module A exposing (main)"
              , ""
              , "import B"
              , ""
              , "main : Int"
              , "main = B.helper 42"
              ]
            textB = T.unlines
              [ "module B exposing (helper)"
              , ""
              , "helper : Int -> Int"
              , "helper x = x"
              ]

        -- Open B first
        sendLsp hin $ object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("textDocument/didOpen" :: T.Text)
          , "params" .= object
              [ "textDocument" .= object
                  [ "uri" .= uriB
                  , "languageId" .= ("lune" :: T.Text)
                  , "version" .= (0 :: Int)
                  , "text" .= textB
                  ]
              ]
          ]

        diagsB1 <- waitForPublishDiagnostics uriB hout
        logMsg ("B initial diags: " <> show (length diagsB1))

        -- Open A
        sendLsp hin $ object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("textDocument/didOpen" :: T.Text)
          , "params" .= object
              [ "textDocument" .= object
                  [ "uri" .= uriA
                  , "languageId" .= ("lune" :: T.Text)
                  , "version" .= (0 :: Int)
                  , "text" .= textA
                  ]
              ]
          ]

        diagsA1 <- waitForPublishDiagnostics uriA hout
        logMsg ("A initial diags: " <> show (length diagsA1))

        -- Both should have no errors initially
        assertEqual "B has no initial errors" 0 (length diagsB1)
        assertEqual "A has no initial errors" 0 (length diagsA1)

        -- Now change B in memory to introduce a type error
        -- Change helper to return String instead of Int
        let textBBroken = T.unlines
              [ "module B exposing (helper)"
              , ""
              , "helper : Int -> String"
              , "helper x = \"broken\""
              ]

        sendLsp hin $ object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("textDocument/didChange" :: T.Text)
          , "params" .= object
              [ "textDocument" .= object
                  [ "uri" .= uriB
                  , "version" .= (1 :: Int)
                  ]
              , "contentChanges" .= [ object [ "text" .= textBBroken ] ]
              ]
          ]

        -- We should get diagnostics for B (no error in B itself)
        diagsB2 <- waitForPublishDiagnostics uriB hout
        logMsg ("B after change diags: " <> show (length diagsB2))

        -- And we should get diagnostics for A (type mismatch: expects Int, got String)
        diagsA2 <- waitForPublishDiagnostics uriA hout
        logMsg ("A after B change diags: " <> show (length diagsA2))

        -- A should have an error because B.helper now returns String, not Int
        assertEqual "A has type error after B change" True (length diagsA2 > 0)

        -- Fix B again
        sendLsp hin $ object
          [ "jsonrpc" .= ("2.0" :: T.Text)
          , "method" .= ("textDocument/didChange" :: T.Text)
          , "params" .= object
              [ "textDocument" .= object
                  [ "uri" .= uriB
                  , "version" .= (2 :: Int)
                  ]
              , "contentChanges" .= [ object [ "text" .= textB ] ]
              ]
          ]

        diagsB3 <- waitForPublishDiagnostics uriB hout
        diagsA3 <- waitForPublishDiagnostics uriA hout

        logMsg ("B after fix diags: " <> show (length diagsB3))
        logMsg ("A after fix diags: " <> show (length diagsA3))

        -- Both should be clean again
        assertEqual "B is clean after fix" 0 (length diagsB3)
        assertEqual "A is clean after fix" 0 (length diagsA3)

        logMsg "VFS propagation test passed"
