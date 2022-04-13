module CLI 
  ( CliOpts(..)
  , defaultCliOpts
  , parseCliOpts
  , DumpOpts(..)
  ) where

import Options.Applicative

----------------------------------------
-- Command-line interface
----------------------------------------

data CliOpts = CliOpts {
  optInput :: Maybe FilePath,    -- Nothing means stdin
  optOutput :: Maybe FilePath,   -- Nothing means stdout
  optDump :: [DumpOpts],      
  optVerbose :: Bool
} deriving (Show, Read, Eq, Ord)

defaultCliOpts :: CliOpts
defaultCliOpts = CliOpts {
  optInput = Nothing,
  optOutput = Nothing,
  optDump = [],
  optVerbose = False
}

----------------------------------------
-- Options parser

parseCliOpts :: IO CliOpts
parseCliOpts = do
  let cliPrefs = prefs (showHelpOnEmpty <> columns 150)
  let desc = fullDesc <> progDesc "The migthy arepa compiler" 
  customExecParser cliPrefs (info (helper <*> cliOpts) desc)
  where 
    cliOpts = CliOpts <$> 
      optional (strOption   (long "input"   <> short 'i' <> metavar "PATH" <> help "input file")) <*>
      optional (strOption   (long "output"  <> short 'o' <> metavar "PATH" <> help "output binary")) <*>
      many     (option dump (long "dump"    <> short 'd' <> metavar "DUMP" <> help "dump an internal structure")) <*>
      switch                (long "verbose" <> short 'v'                   <> help "show extra debug information")

----------------------------------------
-- Dump parser

data DumpOpts = AST | PPR
  deriving (Show, Read, Eq, Ord)

dump :: ReadM DumpOpts
dump = eitherReader $ \s -> do
  case s of
    "ast" -> Right AST
    "ppr" -> Right PPR
    _     -> Left ("invalid dump option " <> s)