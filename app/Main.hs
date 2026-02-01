module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Lune.Parser (parseFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      result <- parseFile path
      print result

    _ ->
      putStrLn "Usage: lune <file.lune>"

