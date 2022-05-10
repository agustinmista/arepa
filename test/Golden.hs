module Golden where

import System.FilePath
import System.Directory
import System.Process
import System.IO.Silently

import Data.String
import Data.ByteString.Lazy

import Test.Tasty
import Test.Tasty.Golden

import Language.Arepa

----------------------------------------
-- Golden testing

goldenDir :: FilePath
goldenDir = "test" </> "golden"

loadGoldenTests :: IO TestTree
loadGoldenTests = do
  arepaFiles <- findByExtension [".ar"] goldenDir
  let goldenTests = testGroup "golden" (mkGoldenTests <$> arepaFiles)
  return goldenTests

mkGoldenTests :: FilePath -> TestTree
mkGoldenTests arepaFile = do
  let name = takeBaseName arepaFile
  let goldenFile = replaceExtension arepaFile "golden"
  testGroup name [
      goldenVsString "interpreter" goldenFile (runInterpreterHook arepaFile),
      goldenVsString "compiler"    goldenFile (runCompilerHook    arepaFile)
    ]

runInterpreterHook :: FilePath -> IO ByteString
runInterpreterHook path = do
  let opts = defaultOpts { optInput = Just path }
  output <- capture_ $ runArepa' opts $ do
    readArepaInput >>= parseModule >>= typeCheckModule >>= translateModule >>= interpretCodeStore
  return (fromString output)

runCompilerHook :: FilePath -> IO ByteString
runCompilerHook arepaFile = do
  let binFile  = replaceExtension arepaFile "out"
  let opts = defaultOpts { optInput = Just arepaFile, optOutput = Just binFile }
  runArepa' opts $ do
    readArepaInput >>= parseModule >>= typeCheckModule >>= translateModule >>= emitLLVM >>= renderLLVM >>= writeLLVMOutput
    mkClangArgs >>= runClang
  stdout <- readProcess binFile [] []
  removeFile binFile
  return (fromString stdout)
