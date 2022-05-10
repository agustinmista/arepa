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
runInterpreterHook path = do
  let stdout = replaceExtension path "out"
  let opts = defaultOpts { optInput = Just path, optInterpretStdout = stdout }
  runArepa' opts $ do
    readArepaInput >>= parseModule >>= typeCheckModule >>= translateModule >>= interpretCodeStore
  output <- readFile stdout
  removeFile stdout
  return (fromString output)


runCompilerHook :: FilePath -> IO ByteString
runCompilerHook arepaFile = do
  let binFile  = replaceExtension arepaFile "elf"
  let opts = defaultOpts { optInput = Just arepaFile, optOutput = Just binFile }
  runArepa' opts $ do
    readArepaInput >>= parseModule >>= typeCheckModule >>= translateModule >>= emitLLVM >>= renderLLVM >>= writeLLVMOutput
    mkClangArgs >>= runClang
  output <- readProcess binFile [] []
  removeFile binFile
  return (fromString output)
