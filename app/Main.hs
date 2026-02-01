module Main where

import System.Environment (getArgs)
import Lune.Desugar (desugarModule)
import Lune.Parser (parseFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--parse", path] ->
      runParse path

    ["--desugar", path] ->
      runDesugar path

    [path] ->
      runParse path

    _ ->
      putStrLn "Usage: lune [--parse|--desugar] <file.lune>"

runParse :: FilePath -> IO ()
runParse path = do
  result <- parseFile path
  print result

runDesugar :: FilePath -> IO ()
runDesugar path = do
  result <- parseFile path
  print (desugarModule result)
