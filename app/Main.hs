module Main where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , makeAbsolute
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode (..), exitFailure)
import System.Process
  ( CreateProcess (cwd, env)
  , proc
  , readCreateProcessWithExitCode
  , readProcessWithExitCode
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import qualified Lune.Codegen.C as CodegenC
import qualified Lune.Codegen.Go as CodegenGo
import Lune.Desugar (desugarModule)
import qualified Lune.Elaborate as Elab
import qualified Lune.Eval as Eval
import qualified Lune.Core as Core
import qualified Lune.Pass.ANF as ANF
import Lune.Parser (parseFile)
import Lune.Check (typecheckModule, renderScheme)
import Lune.Validate (validateModule)
import qualified Lune.ModuleGraph as MG
import qualified Lune.Resolve as Resolve
import qualified Lune.Fmt as Fmt
import qualified Lune.LSP.Server as LSP
import System.FilePath (takeDirectory, (</>))

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
  | CmdFmt FmtOptions
  | CmdLsp LspOptions

data LspTransport
  = LspStdio
  deriving (Eq, Show)

data LspOptions = LspOptions
  { lspTransport :: LspTransport
  }

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
    ("lsp" : rest) ->
      CmdLsp <$> parseLspArgs rest
    ("build" : rest) ->
      CmdBuild <$> parseBuildArgs rest
    _ | "--fmt" `elem` args ->
          CmdFmt <$> parseFmtArgs args
    _ ->
      CmdRun <$> parseArgs args

parseLspArgs :: [String] -> Either String LspOptions
parseLspArgs args =
  case args of
    [] ->
      Right (LspOptions {lspTransport = LspStdio})
    ["--stdio"] ->
      Right (LspOptions {lspTransport = LspStdio})
    _ ->
      Left lspUsage
  where
    lspUsage = "Usage: lune lsp [--stdio]"

data FmtOptions = FmtOptions
  { fmtPath :: FilePath
  , fmtCheck :: Bool
  , fmtStdout :: Bool
  }

parseFmtArgs :: [String] -> Either String FmtOptions
parseFmtArgs args =
  let rest = filter (/= "--fmt") args
      (flags, positionals) = partitionFlags rest
      check = "--check" `elem` flags
      stdout = "--stdout" `elem` flags
      unknown = filter (`notElem` ["--check", "--stdout"]) flags
   in case () of
        _
          | not (null unknown) ->
              Left fmtUsage
          | check && stdout ->
              Left fmtUsage
          | [path] <- positionals ->
              Right (FmtOptions {fmtPath = path, fmtCheck = check, fmtStdout = stdout})
          | otherwise ->
              Left fmtUsage
  where
    fmtUsage = "Usage: lune --fmt [--check|--stdout] <file.lune>"

    partitionFlags =
      foldr
        ( \a (fs, ps) ->
            if "--" `isPrefixOf` a
              then (a : fs, ps)
              else (fs, a : ps)
        )
        ([], [])

    isPrefixOf prefix str = take (length prefix) str == prefix

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
    CmdFmt opts ->
      runFmt opts
    CmdLsp opts ->
      runLsp opts

runLsp :: LspOptions -> IO ()
runLsp opts =
  case lspTransport opts of
    LspStdio -> LSP.runStdio

runFmt :: FmtOptions -> IO ()
runFmt opts = do
  contents <- TIO.readFile (fmtPath opts)
  formatted <-
    case Fmt.formatText (fmtPath opts) contents of
      Left err -> do
        TIO.putStrLn (Fmt.renderFmtError err)
        exitFailure
      Right t ->
        pure t

  if fmtStdout opts
    then TIO.putStr formatted
    else
      if fmtCheck opts
        then
          if formatted == contents
            then pure ()
            else exitFailure
        else TIO.writeFile (fmtPath opts) formatted

data Options = Options
  { optPath :: FilePath
  , optDesugar :: Bool
  , optValidate :: Bool
  , optTypecheck :: Bool
  , optCore :: Bool
  , optAnf :: Bool
  , optEval :: Bool
  , optRun :: Bool
  }

parseArgs :: [String] -> Either String Options
parseArgs args =
  case args of
    [path] ->
      Right Options {optPath = path, optDesugar = False, optValidate = False, optTypecheck = False, optCore = False, optAnf = False, optEval = False, optRun = False}
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
                  anf = "--anf" `elem` flags
                  evalFlag = "--eval" `elem` flags
                  runFlag = "--run" `elem` flags
                  unknown = filter (`notElem` ["--parse", "--desugar", "--validate", "--typecheck", "--core", "--anf", "--eval", "--run"]) flags
               in if null unknown
                    then
                      let coreOut = core || anf
                       in Right Options {optPath = path, optDesugar = desugar, optValidate = validate, optTypecheck = typecheck, optCore = coreOut, optAnf = anf, optEval = evalFlag, optRun = runFlag}
                    else Left usage
        _ ->
          Left usage
  where
    usage = "Usage: lune [--parse|--desugar] [--validate] [--typecheck] [--core] [--anf] [--eval] [--run] <file.lune>"
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
                if optAnf opts
                  then print (ANF.anfModule coreMod)
                  else print coreMod
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
  loaded <- MG.loadProgram (buildInput opts)
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

  let mod' = desugarModule resolved

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

  createDirectoryIfMissing True (takeDirectory (buildOutput opts))

  case buildTarget opts of
    BuildTargetC ->
      buildC opts coreMod
    BuildTargetGo ->
      buildGo opts coreMod

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

buildGo :: BuildOptions -> Core.CoreModule -> IO ()
buildGo opts coreMod = do
  let goModulePath = "lune.local/generated"
      rtImportPath = goModulePath <> "/rt"
      goDir = buildOutput opts <> ".go-build"
      goModPath = goDir </> "go.mod"
      mainPath = goDir </> "main.go"
      runtimeTemplateRoot = "runtime/go"
      coreMod' = ANF.anfModule coreMod

  runtimeExists <- doesDirectoryExist runtimeTemplateRoot
  if runtimeExists
    then pure ()
    else do
      putStrLn ("Missing Go runtime templates: " <> runtimeTemplateRoot)
      exitFailure

  goDirExists <- doesDirectoryExist goDir
  if goDirExists
    then removeDirectoryRecursive goDir
    else pure ()

  goDirIsFile <- doesFileExist goDir
  if goDirIsFile
    then removeFile goDir
    else pure ()

  goSource <-
    case CodegenGo.codegenModule (T.pack rtImportPath) coreMod' of
      Left err -> do
        putStrLn (show err)
        exitFailure
      Right goSrc ->
        pure goSrc

  createDirectoryIfMissing True goDir
  copyDirectoryRecursive runtimeTemplateRoot goDir
  writeFile mainPath (T.unpack goSource)
  writeFile goModPath (T.unpack (goModSource goModulePath))

  outAbs <- makeAbsolute (buildOutput opts)
  goDirAbs <- makeAbsolute goDir

  let goCacheDir = goDirAbs </> ".gocache"
      goModCacheDir = goDirAbs </> ".gomodcache"
      goPathDir = goDirAbs </> ".gopath"
      goTmpDir = goDirAbs </> ".gotmp"

  createDirectoryIfMissing True goCacheDir
  createDirectoryIfMissing True goModCacheDir
  createDirectoryIfMissing True goPathDir
  createDirectoryIfMissing True goTmpDir

  env0 <- getEnvironment
  let overrides =
        [ ("GOCACHE", goCacheDir)
        , ("GOMODCACHE", goModCacheDir)
        , ("GOPATH", goPathDir)
        , ("GOTMPDIR", goTmpDir)
        ]
      dropKeys = ["GOCACHE", "GOMODCACHE", "GOPATH", "GOTMPDIR"]
      env1 =
        Just (overrides <> filter (\(k, _) -> k `notElem` dropKeys) env0)

  goResult <-
    ( try
        ( readCreateProcessWithExitCode
            (proc "go" ["build", "-gcflags=all=-l", "-o", outAbs, "."]) {cwd = Just goDirAbs, env = env1}
            ""
        )
    ) ::
      IO (Either IOException (ExitCode, String, String))

  case goResult of
    Left e -> do
      putStrLn ("Failed to run go: " <> show e)
      putStrLn ("Go module emitted to: " <> goDir)
      putStrLn ("To build manually: cd " <> goDir <> " && go build -o " <> outAbs <> " .")
      exitFailure
    Right (ec, goOut, goErr) ->
      case ec of
        ExitSuccess ->
          pure ()
        ExitFailure _ -> do
          putStrLn goOut
          putStrLn goErr
          putStrLn ("Go module emitted to: " <> goDir)
          exitFailure

goModSource :: String -> T.Text
goModSource modulePath =
  T.unlines
    [ "module " <> T.pack modulePath
    , ""
    , "go 1.18"
    ]

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  forM_ entries $ \entry -> do
    let srcPath = src </> entry
        dstPath = dst </> entry
    isDir <- doesDirectoryExist srcPath
    if isDir
      then copyDirectoryRecursive srcPath dstPath
      else copyFile srcPath dstPath
