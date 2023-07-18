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
  strArgument (
    metavar "PATH" <>
    help "Input file"
  ) <*>
  optionalStr (
    long "output" <>
    short 'o' <>
    metavar "PATH" <>
    help "Output binary"
  ) <*>
  switch (
    short 'c' <>
    help "Compile and assemble, but do not link"
  ) <*>
  dumpOpts (
    long "dump" <>
    short 'd' <>
    metavar "DUMP" <>
    help "Dump an internal structure (ast,ppr,tim,llvm)"
  ) <*>
  switch (
    long "verbose" <>
    short 'v' <>
    help "Show extra debug information"
  ) <*>
  switch (
    long "interpret" <>
    short 'x' <>
    help "Interpret the input instead of compiling it"
  ) <*>
  strOption (
    long "stdin" <>
    value "/dev/stdin" <>
    help "Set the interpreter's stdin (defaults to /dev/stdin)"
  ) <*>
  strOption (
    long "stdout" <>
    value "/dev/stdout" <>
    help "Set the interpreter's stdout (defaults to /dev/stdout)"
  ) <*>
  strOption (
    long "entry" <>
    short 'e' <>
    metavar "NAME" <>
    value "main" <>
    help "Set the module's entry point"
  ) <*>
  option auto (
    short 'O' <>
    metavar "NUM" <>
    value 0 <>
    help "Set the optimization level"
  ) <*>
  switch (
    long "strict" <>
    short 's' <>
    help "Disable implicit externs"
  ) <*>
  switch (
    long "emit-main" <>
    short 'm' <>
    help "Always emit a compiled main()"
  ) <*>
  manyStr (
    long "include" <>
    short 'I' <>
    metavar "PATH" <>
    help "Include extra LLVM/C files during linking"
  ) <*>
  switch (
    long "debug" <>
    short 'D' <>
    help "Enable debug messages in the compiled binary"
  ) <*>
  switch (
    long "interactive" <>
    help "Show execution traces interactively"
  )

optionalStr :: Mod OptionFields String -> OptParse.Parser (Maybe String)
optionalStr = optional . strOption

manyStr :: Mod OptionFields FilePath -> OptParse.Parser [FilePath]
manyStr = many . strOption

dumpOpts :: Mod OptionFields DumpOpt -> OptParse.Parser [DumpOpt]
dumpOpts desc = many (option dumpReader desc)
  where
    dumpReader = eitherReader $ \s -> do
      case s of
        "ast"    -> Right AST
        "ppr"    -> Right PPR
        "lint"   -> Right LINT
        "rename" -> Right RENAME
        "lift"   -> Right LIFT
        "tim"    -> Right TIM
        "cg"     -> Right CG
        _        -> Left ("invalid dump option " <> s)