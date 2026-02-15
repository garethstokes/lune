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

import Data.List (isSuffixOf, sort)
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeBaseName)
import System.Process (readProcessWithExitCode)
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
