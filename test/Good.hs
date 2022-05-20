module Good where

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

goodTestsDir :: FilePath
goodTestsDir = "test" </> "unit" </> "good"

loadGoodTests :: IO TestTree
loadGoodTests = do
  arepaFiles <- findByExtension [".ar"] goodTestsDir
  return (testGroup "good" (mkGoldenTests <$> arepaFiles))


mkGoldenTests :: FilePath -> TestTree
mkGoldenTests arepaFile = do
  let name = takeBaseName arepaFile
  let goldenFile = replaceExtension arepaFile "golden"
  testGroup name [
      goldenVsString "compiler"    goldenFile (runCompilerHook    arepaFile),
      goldenVsString "interpreter" goldenFile (runInterpreterHook arepaFile)
    ]


runInterpreterHook :: FilePath -> IO ByteString
runInterpreterHook arepaFile = do
  let stdinFile = replaceExtension arepaFile "stdin"
  hasStdin <- doesFileExist stdinFile
  let stdoutFile = replaceExtension arepaFile "stdout"
  let opts = defaultOpts {
        optInput = Just arepaFile,
        optInterpretStdout = stdoutFile,
        optInterpretStdin = if hasStdin then stdinFile else "/dev/stdin",
        optStrict = True
      }
  runArepa' opts $ do
    readArepaSourceFile
      >>= parseModule
      >>= renameModule
      >>= typeCheckModule
      >>= lambdaLiftModule
      >>= translateModule
      >>= interpretCodeStore
  output <- readFile stdoutFile
  removeFile stdoutFile
  return (fromString output)


runCompilerHook :: FilePath -> IO ByteString
runCompilerHook arepaFile = do
  let binFile  = replaceExtension arepaFile "elf"
  let opts = defaultOpts {
        optInput = Just arepaFile,
        optOutput = Just binFile,
        optStrict = True
      }
  runArepa' opts $ do
    readArepaSourceFile
      >>= parseModule
      >>= renameModule
      >>= typeCheckModule
      >>= lambdaLiftModule
      >>= translateModule
      >>= emitLLVM
      >>= renderLLVM
      >>= writeLLVMModule
    mkClangArgs
      >>= runClang
  let stdinFile = replaceExtension arepaFile "stdin"
  hasStdin <- doesFileExist stdinFile
  stdin <- if hasStdin then readFile stdinFile else return mempty
  output <- readProcess binFile [] stdin
  removeFile binFile
  return (fromString output)
