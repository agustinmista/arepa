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
  let cliPrefs = prefs (showHelpOnEmpty <> columns 140)
  let desc = fullDesc <> progDesc "The migthy arepa compiler"
  customExecParser cliPrefs (info (helper <*> cliOpts) desc)

cliOpts :: OptParse.Parser ArepaOpts
cliOpts = ArepaOpts <$>
  optionalStr (long "input"     <> short 'i' <> metavar "PATH" <>                    help "Input file") <*>
  optionalStr (long "output"    <> short 'o' <> metavar "PATH" <>                    help "Output binary") <*>
  switch      (                    short 'c' <>                                      help "Compile and assemble, but do not link") <*>
  dumpOpts    (long "dump"      <> short 'd' <> metavar "DUMP" <>                    help "Dump an internal structure (ast,ppr,tim,llvm)") <*>
  switch      (long "verbose"   <> short 'v'                   <>                    help "Show extra debug information") <*>
  switch      (long "interpret" <> short 'x'                   <>                    help "Interpret the input instead of compiling it") <*>
  strOption   (long "stdout"    <>                                value "out.txt" <> help "Interpret the input instead of compiling it") <*>
  optionalStr (long "entry"     <> short 'e' <> metavar "NAME" <>                    help "Set the module's entry point") <*>
  option auto (                    short 'O' <> metavar "NUM"  <> value 0 <>         help "Set the optimization level") <*>
  switch      (long "strict"    <> short 's' <>                                      help "Disable implicit externs") <*>
  switch      (long "emit-main" <> short 'm' <>                                      help "Always emit a compiled main()") <*>
  manyStr     (long "include"   <> short 'I' <> metavar "PATH" <>                    help "Include extra LLVM/C files during linking") <*>
  switch      (long "debug"     <> short 'D' <>                                      help "Enable debug messages in the compiled binary")

optionalStr :: Mod OptionFields String -> OptParse.Parser (Maybe String)
optionalStr = optional . strOption

manyStr :: Mod OptionFields FilePath -> OptParse.Parser [FilePath]
manyStr = many . strOption

dumpOpts :: Mod OptionFields DumpOpt -> OptParse.Parser [DumpOpt]
dumpOpts desc = many (option dumpReader desc)
  where
    dumpReader = eitherReader $ \s -> do
      case s of
        "ast"  -> Right AST
        "ppr"  -> Right PPR
        "tim"  -> Right TIM
        "llvm" -> Right LLVM
        _      -> Left ("invalid dump option " <> s)