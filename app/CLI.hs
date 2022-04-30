module CLI
  ( parseCliOpts
  ) where

import Options.Applicative
import Options.Applicative qualified as OptParse

import Language.Arepa.Compiler

----------------------------------------
-- Command-line options parser
----------------------------------------

parseCliOpts :: IO ArepaOpts
parseCliOpts = do
  let cliPrefs = prefs (showHelpOnEmpty <> columns 150)
  let desc = fullDesc <> progDesc "The migthy arepa compiler"
  customExecParser cliPrefs (info (helper <*> cliOpts) desc)
  where
    cliOpts = ArepaOpts <$>
      optionalStr (long "input"     <> short 'i' <> metavar "PATH" <>                 help "input file") <*>
      optionalStr (long "output"    <> short 'o' <> metavar "PATH" <>                 help "output binary") <*>
      dumpOpts    (long "dump"      <> short 'd' <> metavar "DUMP" <>                 help "dump an internal structure") <*>
      switch      (long "verbose"   <> short 'v'                   <>                 help "show extra debug information") <*>
      switch      (long "interpret" <> short 'x'                   <>                 help "interpret the input instead of compiling it") <*>
      strOption   (long "entry"     <> short 'e'                   <> value "main" <> help "set the module's entry point") <*>
      option auto (long "args"      <> short 'a'                   <> value []     <> help "set the module's entry point") <*>
      option auto (                    short 'O'                   <> value 0      <> help "set the optimization level")

optionalStr :: Mod OptionFields String -> OptParse.Parser (Maybe String)
optionalStr = optional . strOption

dumpOpts :: Mod OptionFields DumpOpt -> OptParse.Parser [DumpOpt]
dumpOpts desc = many (option dumpReader desc)
  where
    dumpReader = eitherReader $ \s -> do
      case s of
        "ast"  -> Right AST
        "ppr"  -> Right PPR
        "tim"  -> Right TIM
        "llvm" -> Right LLVM
        _      -> Left ("invalid dumpOpts option " <> s)