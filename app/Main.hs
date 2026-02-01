module Main where

import System.Environment (getArgs)
import Lune.Parser (parseFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--parse", path] ->
      runParse path

    [path] ->
      runParse path

    _ ->
      putStrLn "Usage: lune [--parse] <file.lune>"

runParse :: FilePath -> IO ()
runParse path = do
  result <- parseFile path
  print result
