module CLI
  ( parseCliOpts
  ) where

import Options.Applicative

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
      optional (strOption   (long "input"   <> short 'i' <> metavar "PATH" <> help "input file")) <*>
      optional (strOption   (long "output"  <> short 'o' <> metavar "PATH" <> help "output binary")) <*>
      many     (option dump (long "dump"    <> short 'd' <> metavar "DUMP" <> help "dump an internal structure")) <*>
      switch                (long "verbose" <> short 'v'                   <> help "show extra debug information")

dump :: ReadM DumpOpt
dump = eitherReader $ \s -> do
  case s of
    "ast" -> Right AST
    "ppr" -> Right PPR
    _     -> Left ("invalid dump option " <> s)