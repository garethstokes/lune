module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Lune.Desugar (desugarModule)
import qualified Lune.Elaborate as Elab
import qualified Lune.Eval as Eval
import qualified Lune.Core as Core
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
  , optCore :: Bool
  , optEval :: Bool
  }

parseArgs :: [String] -> Either String Options
parseArgs args =
  case args of
    [path] ->
      Right Options {optPath = path, optDesugar = False, optValidate = False, optTypecheck = False, optCore = False, optEval = False}
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
                  core = "--core" `elem` flags
                  evalFlag = "--eval" `elem` flags
                  unknown = filter (`notElem` ["--parse", "--desugar", "--validate", "--typecheck", "--core", "--eval"]) flags
               in if null unknown
                    then
                      Right Options {optPath = path, optDesugar = desugar, optValidate = validate, optTypecheck = typecheck, optCore = core, optEval = evalFlag}
                    else Left usage
        _ ->
          Left usage
  where
    usage = "Usage: lune [--parse|--desugar] [--validate] [--typecheck] [--core] [--eval] <file.lune>"
    isPrefixOf prefix str = take (length prefix) str == prefix

run :: Options -> IO ()
run opts = do
  surface <- parseFile (optPath opts)
  let mod' =
        if optDesugar opts || optTypecheck opts || optCore opts || optEval opts
          then desugarModule surface
          else surface

  let shouldValidate = optValidate opts || optTypecheck opts || optCore opts || optEval opts
  if shouldValidate
    then case validateModule mod' of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right () ->
        pure ()
    else pure ()

  if optEval opts
    then case Elab.prepareTypedModule mod' >>= Elab.elaborateModule of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right coreMod ->
        case Eval.evalModule coreMod of
          Left err -> do
            putStrLn (show err)
            exitFailure
          Right env ->
            case Map.lookup "idList" env of
              Just _ ->
                case Eval.evalExpr env (Core.CVar "idList") of
                  Left err -> do
                    putStrLn (show err)
                    exitFailure
                  Right v ->
                    print v
              Nothing ->
                print (Map.keys env)
    else
      if optCore opts
        then case Elab.prepareTypedModule mod' >>= Elab.elaborateModule of
          Left err -> do
            putStrLn (show err)
            exitFailure
          Right coreMod ->
            print coreMod
        else
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
