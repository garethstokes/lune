module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Lune.Desugar (desugarModule)
import Lune.Parser (parseFile)
import Lune.Validate (validateModule)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left msg ->
      putStrLn msg
    Right opts ->
      run opts

data Options = Options
  { optPath :: FilePath
  , optDesugar :: Bool
  , optValidate :: Bool
  }

parseArgs :: [String] -> Either String Options
parseArgs args =
  case args of
    [path] ->
      Right Options {optPath = path, optDesugar = False, optValidate = False}
    _ ->
      case reverse args of
        path : flagsRev
          | "--" `isPrefixOf` path ->
              Left usage
          | otherwise ->
              let flags = reverse flagsRev
                  desugar = "--desugar" `elem` flags
                  validate = "--validate" `elem` flags
                  unknown = filter (`notElem` ["--parse", "--desugar", "--validate"]) flags
               in if null unknown
                    then Right Options {optPath = path, optDesugar = desugar, optValidate = validate}
                    else Left usage
        _ ->
          Left usage
  where
    usage = "Usage: lune [--parse|--desugar] [--validate] <file.lune>"
    isPrefixOf prefix str = take (length prefix) str == prefix

run :: Options -> IO ()
run opts = do
  surface <- parseFile (optPath opts)
  let mod' = if optDesugar opts then desugarModule surface else surface
  if optValidate opts
    then case validateModule mod' of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right () ->
        putStrLn "OK"
    else print mod'
