module Good where

import Control.Monad.Extra

import System.FilePath
import System.Directory
import System.Process

import Data.String

import Data.ByteString.Lazy (ByteString)

import Test.Tasty
import Test.Tasty.Golden

import Language.Arepa

----------------------------------------
-- Golden testing

goodStandaloneTestsDir :: FilePath
goodStandaloneTestsDir = "test" </> "unit" </> "good" </> "standalone"

goodPartialTestsDir :: FilePath
goodPartialTestsDir = "test" </> "unit" </> "good" </> "partial"

preludePath :: FilePath
preludePath = "lib" </> "prelude"

loadGoodTests :: IO TestTree
loadGoodTests = do
  standaloneTests <- mkGoldenTests []            =<< findByExtension [".ar"] goodStandaloneTestsDir
  partialTests    <- mkGoldenTests [preludePath] =<< findByExtension [".ar"] goodPartialTestsDir
  return $
    testGroup "good" [
      testGroup "standalone" standaloneTests,
      testGroup "partial"    partialTests
    ]

mkGoldenTests :: [FilePath] -> [FilePath] -> IO [TestTree]
mkGoldenTests extraFiles arepaFiles = do
  forM arepaFiles $ \arepaFile -> do
    let name = takeBaseName arepaFile
    let goldenFile = replaceExtension arepaFile "golden"
    return $
      testGroup name [
        goldenVsString "compiler"      goldenFile (runCompilerHook    extraFiles arepaFile),
        goldenVsString "interpreter"   goldenFile (runInterpreterHook extraFiles arepaFile)
      ]

----------------------------------------
-- Pipelines
----------------------------------------

runInterpreterHook :: [FilePath] -> FilePath -> IO ByteString
runInterpreterHook extraFiles arepaFile = do
  mapM_ runDependencyPrecompiler extraFiles
  let stdinFile = replaceExtension arepaFile "stdin"
  hasStdin <- doesFileExist stdinFile
  let stdoutFile = replaceExtension arepaFile "stdout"
  runArepa'
    defaultOpts {
      optInput = arepaFile,
      optInclude = extraFiles,
      optInterpretStdout = stdoutFile,
      optInterpretStdin = if hasStdin then stdinFile else "/dev/stdin",
      optStrict = null extraFiles
    }
    (runFrontend >>= interpretCodeStore)
  output <- readFile stdoutFile
  removeFileIfExists stdoutFile
  return (fromString output)

runCompilerHook :: [FilePath] -> FilePath -> IO ByteString
runCompilerHook extraFiles arepaFile = do
  mapM_ runDependencyPrecompiler extraFiles
  let binFile  = replaceExtension arepaFile "elf"
  tmpFiles <- runArepa'
    defaultOpts {
      optInput = arepaFile,
      optInclude = extraFiles,
      optOutput = Just binFile,
      optStrict = null extraFiles
    }
    (runFrontend >>= runBackend)
  let stdinFile = replaceExtension arepaFile "stdin"
  hasStdin <- doesFileExist stdinFile
  stdin <- if hasStdin then readFile stdinFile else return mempty
  output <- readProcess binFile [] stdin
  mapM_ removeFileIfExists (binFile : tmpFiles)
  return (fromString output)

runDependencyPrecompiler :: FilePath -> IO ()
runDependencyPrecompiler extraFile = do
  let opts = defaultOpts {
        optInput = mkArepaFilePath extraFile,
        optNoLinking = True
      }
  runArepa' opts $ do
    let tim_path = mkTIMFilePath  extraFile
    let h_path   = mkHFilePath    extraFile
    let c_path   = mkCFilePath    extraFile
    let ll_path  = mkLLVMFilePath extraFile
    let helpers  = [tim_path, h_path, c_path, ll_path]
    unlessM (andM [ liftIO (doesFileExist path) | path <- helpers ]) $ do
      warning $ "PRECOMPILING DEPENDENCY: " <> prettyPrint extraFile
      store <- runFrontend
      let tim_text = encodeCodeStore store
      (h_text, c_text) <- renderC =<< emitC store
      -- ll_text <- renderLLVM =<< emitLLVM store
      writeCompiledFile tim_path tim_text
      writeCompiledFile h_path  h_text
      writeCompiledFile c_path  c_text
      -- writeCompiledFile ll_path ll_text

-- Common precompiler/frontend/backend

runFrontend :: MonadArepa m => m CodeStore
runFrontend = do
  readArepaSourceFile
  >>= parseModule
  >>= lintModule
  >>= renameModule
  >>= lambdaLiftModule
  >>= translateModule

runBackend :: MonadArepa m => CodeStore -> m [FilePath]
runBackend store = do
  tim_path <- compiledTIMPath
  h_path <- compiledHPath
  c_path <- compiledCPath
  (h_text, c_text) <- renderC =<< emitC store
  writeCompiledFile h_path h_text
  writeCompiledFile c_path c_text
  runClang =<< mkClangArgs
  return [tim_path, h_path, c_path]

----------------------------------------
-- Utilities

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  whenM (doesFileExist path) (removeFile path)