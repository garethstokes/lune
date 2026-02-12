module Main where

import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.Process (readProcessWithExitCode)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Lune.Codegen.C as CodegenC
import Lune.Desugar (desugarModule)
import qualified Lune.Elaborate as Elab
import qualified Lune.Eval as Eval
import qualified Lune.Core as Core
import Lune.Parser (parseFile)
import Lune.Check (typecheckModule, renderScheme)
import Lune.Validate (validateModule)
import qualified Lune.ModuleGraph as MG
import qualified Lune.Resolve as Resolve

main :: IO ()
main = do
  args <- getArgs
  case parseCommand args of
    Left msg ->
      putStrLn msg
    Right cmd ->
      runCommand cmd

data Command
  = CmdRun Options
  | CmdBuild BuildOptions

data BuildTarget
  = BuildTargetC
  | BuildTargetGo
  deriving (Eq, Show)

data BuildOptions = BuildOptions
  { buildInput :: FilePath
  , buildOutput :: FilePath
  , buildTarget :: BuildTarget
  }

parseCommand :: [String] -> Either String Command
parseCommand args =
  case args of
    ("build" : rest) ->
      CmdBuild <$> parseBuildArgs rest
    _ ->
      CmdRun <$> parseArgs args

parseBuildArgs :: [String] -> Either String BuildOptions
parseBuildArgs args =
  case args of
    [path, "-o", out] ->
      Right (BuildOptions path out BuildTargetC)
    [path, "--output", out] ->
      Right (BuildOptions path out BuildTargetC)
    [path, "-o", out, "--target", target] ->
      BuildOptions path out <$> parseBuildTarget target
    [path, "--output", out, "--target", target] ->
      BuildOptions path out <$> parseBuildTarget target
    _ ->
      Left buildUsage
  where
    buildUsage = "Usage: lune build <file.lune> -o <exe> [--target c|go]"

parseBuildTarget :: String -> Either String BuildTarget
parseBuildTarget target =
  case target of
    "c" -> Right BuildTargetC
    "go" -> Right BuildTargetGo
    other -> Left ("Unknown build target: " <> other)

runCommand :: Command -> IO ()
runCommand cmd =
  case cmd of
    CmdRun opts ->
      run opts
    CmdBuild opts ->
      build opts

data Options = Options
  { optPath :: FilePath
  , optDesugar :: Bool
  , optValidate :: Bool
  , optTypecheck :: Bool
  , optCore :: Bool
  , optEval :: Bool
  , optRun :: Bool
  }

parseArgs :: [String] -> Either String Options
parseArgs args =
  case args of
    [path] ->
      Right Options {optPath = path, optDesugar = False, optValidate = False, optTypecheck = False, optCore = False, optEval = False, optRun = False}
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
                  runFlag = "--run" `elem` flags
                  unknown = filter (`notElem` ["--parse", "--desugar", "--validate", "--typecheck", "--core", "--eval", "--run"]) flags
               in if null unknown
                    then
                      Right Options {optPath = path, optDesugar = desugar, optValidate = validate, optTypecheck = typecheck, optCore = core, optEval = evalFlag, optRun = runFlag}
                    else Left usage
        _ ->
          Left usage
  where
    usage = "Usage: lune [--parse|--desugar] [--validate] [--typecheck] [--core] [--eval] [--run] <file.lune>"
    isPrefixOf prefix str = take (length prefix) str == prefix

run :: Options -> IO ()
run opts = do
  if optDesugar opts || optValidate opts || optTypecheck opts || optCore opts || optEval opts || optRun opts
    then runProgramPipeline opts
    else do
      surface <- parseFile (optPath opts)
      print surface

runProgramPipeline :: Options -> IO ()
runProgramPipeline opts = do
  loaded <- MG.loadProgram (optPath opts)
  program <-
    case loaded of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right p ->
        pure p

  resolved <-
    case Resolve.resolveProgram program of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right m ->
        pure m

  let mod' =
        if optDesugar opts || optTypecheck opts || optCore opts || optEval opts || optRun opts
          then desugarModule resolved
          else resolved

      entryName = MG.progEntryName program
      qualify n = entryName <> "." <> n

  let shouldValidate = optValidate opts || optTypecheck opts || optCore opts || optEval opts || optRun opts
  if shouldValidate
    then case validateModule mod' of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right () ->
        pure ()
    else pure ()

  if optRun opts
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
            case Map.lookup (qualify "main") env of
              Nothing -> do
                putStrLn "No `main` binding found."
                exitFailure
              Just _ ->
                case Eval.evalExpr env (Core.CVar (qualify "main")) of
                  Left err -> do
                    putStrLn (show err)
                    exitFailure
                  Right v -> do
                    result <- Eval.runIO v
                    case result of
                      Left err -> do
                        putStrLn (show err)
                        exitFailure
                      Right (world, _) ->
                        mapM_ (putStrLn . T.unpack) (Eval.worldStdout world)
    else
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
                case Map.lookup (qualify "idList") env of
                  Just _ ->
                    case Eval.evalExpr env (Core.CVar (qualify "idList")) of
                      Left err -> do
                        putStrLn (show err)
                        exitFailure
                      Right v ->
                        print v
                  Nothing ->
                    let demoNames = filter (`Map.member` env) (map qualify ["demo", "demo2"])
                        mainName = qualify "main"
                     in if not (null demoNames)
                          then mapM_ (evalAndPrint env) demoNames
                          else
                            if Map.member mainName env
                              then case Eval.evalExpr env (Core.CVar mainName) of
                                Left err -> do
                                  putStrLn (show err)
                                  exitFailure
                                Right v -> do
                                  result <- Eval.runIO v
                                  case result of
                                    Left _ ->
                                      print v
                                    Right (world, _) ->
                                      mapM_ (putStrLn . T.unpack) (Eval.worldStdout world)
                              else print (Map.keys env)
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
                    let entryPrefix = entryName <> "."
                        onlyEntry =
                          filter (\(name, _) -> entryPrefix `T.isPrefixOf` name) bindings
                    mapM_ (\(name, scheme) -> putStrLn (T.unpack (name <> " : " <> renderScheme scheme))) onlyEntry
                else
                  if optValidate opts
                    then putStrLn "OK"
                    else print mod'

evalAndPrint :: Eval.Env -> T.Text -> IO ()
evalAndPrint env name =
  case Eval.evalExpr env (Core.CVar name) of
    Left err -> do
      putStrLn (show err)
      exitFailure
    Right v ->
      print v

build :: BuildOptions -> IO ()
build opts = do
  surface <- parseFile (buildInput opts)
  let mod' = desugarModule surface

  case validateModule mod' of
    Left err -> do
      putStrLn (show err)
      exitFailure
    Right () ->
      pure ()

  case typecheckModule mod' of
    Left err -> do
      putStrLn (show err)
      exitFailure
    Right _ ->
      pure ()

  coreMod <-
    case Elab.prepareTypedModule mod' >>= Elab.elaborateModule of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right coreMod ->
        pure coreMod

  case buildTarget opts of
    BuildTargetC ->
      buildC opts coreMod
    BuildTargetGo ->
      putStrLn "Go target not implemented yet."

buildC :: BuildOptions -> Core.CoreModule -> IO ()
buildC opts coreMod = do
  cSource <-
    case CodegenC.codegenHelloModule coreMod of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right cSrc ->
        pure cSrc

  let cPath = buildOutput opts <> ".c"
  writeFile cPath (T.unpack cSource)

  (ec, ccOut, ccErr) <-
    readProcessWithExitCode
      "cc"
      ["-std=c99", "-O2", "-o", buildOutput opts, cPath]
      ""

  case ec of
    ExitSuccess ->
      pure ()
    ExitFailure _ -> do
      putStrLn ccOut
      putStrLn ccErr
      exitFailure
