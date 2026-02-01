module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Text as T
import Lune.Desugar (desugarModule)
import Lune.Parser (parseFile)
import Lune.Check (typecheckModule, renderScheme)
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
  , optTypecheck :: Bool
  }

parseArgs :: [String] -> Either String Options
parseArgs args =
  case args of
    [path] ->
      Right Options {optPath = path, optDesugar = False, optValidate = False, optTypecheck = False}
    _ ->
      case reverse args of
        path : flagsRev
          | "--" `isPrefixOf` path ->
              Left usage
          | otherwise ->
              let flags = reverse flagsRev
                  desugar = "--desugar" `elem` flags
                  validate = "--validate" `elem` flags
                  typecheck = "--typecheck" `elem` flags
                  unknown = filter (`notElem` ["--parse", "--desugar", "--validate", "--typecheck"]) flags
               in if null unknown
                    then
                      Right Options {optPath = path, optDesugar = desugar, optValidate = validate, optTypecheck = typecheck}
                    else Left usage
        _ ->
          Left usage
  where
    usage = "Usage: lune [--parse|--desugar] [--validate] [--typecheck] <file.lune>"
    isPrefixOf prefix str = take (length prefix) str == prefix

run :: Options -> IO ()
run opts = do
  surface <- parseFile (optPath opts)
  let mod' =
        if optDesugar opts || optTypecheck opts
          then desugarModule surface
          else surface

  let shouldValidate = optValidate opts || optTypecheck opts
  if shouldValidate
    then case validateModule mod' of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right () ->
        pure ()
    else pure ()

  if optTypecheck opts
    then case typecheckModule mod' of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right bindings -> do
        putStrLn "OK"
        mapM_ (\(name, scheme) -> putStrLn (T.unpack (name <> " : " <> renderScheme scheme))) bindings
    else
      if optValidate opts
        then putStrLn "OK"
        else print mod'
