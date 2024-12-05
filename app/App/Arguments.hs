module App.Arguments (execArgParser) where

import Control.Applicative
import Data.Version (showVersion)
import Data.Word
import Options.Applicative
import Paths_static_ls (version)
import StaticLS.StaticEnv.Options
import System.Environment
import System.Exit
import Text.Parsec hiding (many, option)
import Text.Read

currVersion :: String
currVersion = showVersion version

data PrgOptions = PrgOptions
  { staticEnvOpts :: StaticEnvOptions
  , showVer :: Bool
  , showHelp :: Bool
  }

-- | Run an argument parser but suppress invalid arguments (simply running the server instead)
-- Helpful for people on emacs or whose default configurations from HLS pass in
-- unsupported arguments to static-ls
execArgParser :: IO StaticEnvOptions
execArgParser =
  getArgs >>= handleParseResultWithSuppression . execParserPure defaultPrefs progParseInfo
 where
  handleParseResultWithSuppression :: ParserResult PrgOptions -> IO StaticEnvOptions
  handleParseResultWithSuppression (Success (PrgOptions {showHelp = True})) =
    -- Get the help text (optparse-applicative usually shows the help text on error)
    handleParseResult . Failure $
      parserFailure defaultPrefs progParseInfo (ShowHelpText Nothing) mempty
  handleParseResultWithSuppression (Success (PrgOptions {showVer = True})) = do
    -- Show version info
    putStrLn $ "static-ls, version " <> currVersion
    exitSuccess
  handleParseResultWithSuppression (Success a) = return a.staticEnvOpts
  -- Ignore if invalid arguments are input
  handleParseResultWithSuppression (Failure _) = return defaultStaticEnvOptions
  handleParseResultWithSuppression (CompletionInvoked compl) = do
    progn <- getProgName
    msg <- execCompletion compl progn
    putStr msg
    exitSuccess

progParseInfo :: ParserInfo PrgOptions
progParseInfo =
  info
    (argParser <**> helper)
    ( fullDesc
        <> progDesc "Run static-ls as a language server for a client to talk to"
        <> header "static-ls - a lightweight language server for haskell"
    )

argParser :: Parser PrgOptions
argParser =
  PrgOptions
    <$> staticEnvOptParser
    <*> flag
      False
      True
      ( long "version"
          <> short 'v'
          <> help "Show the program version"
      )
    <*> flag
      False
      True
      ( long "help"
          <> short 'h'
      )

staticEnvOptParser :: Parser StaticEnvOptions
staticEnvOptParser =
  StaticEnvOptions
    <$> strOption
      ( long "hiedb"
          <> metavar "TARGET"
          <> value defaultHieDb
          <> help "Path to hiedb file produced by hiedb indexing hiefiles"
          <> showDefault
      )
    <*> strOption
      ( long "hiefiles"
          <> metavar "TARGET"
          <> value defaultHieFiles
          <> help "Path to hiefiles generated by -fwrite-ide-info and specified by -hiedir in ghc"
          <> showDefault
      )
    <*> strOption
      ( long "hifiles"
          <> metavar "TARGET"
          <> value defaultHiFiles
          <> help "Path to hifiles specified by -hidir in ghc"
          <> showDefault
      )
    <*> listOption
      ( long "srcDirs"
          <> metavar "TARGET1,TARGET2,TARGET3..."
          <> value defaultSrcDirs
          <> help "Path to directories containing source code. Comma delimited strings"
          <> showDefault
      )
    <*> (flag' True (long "enableInlays" <> help "Explicitly enable inlay hints.") Options.Applicative.<|> flag False defaultStaticEnvOptions.provideInlays (long "disableInlays" <> help "Explicitly disable inlay hints."))
    <*> Control.Applicative.optional readInlayLen -- switch
 where
  -- Parse a list of comma delimited strings
  listOption = option $ eitherReader (either (Left . show) Right . runParser (sepEndBy (many alphaNum) (char ',')) () "")
  readInlayLen = (option auto $ long "maxInlayLength" <> help "Length to which inlay hints will be truncated.")
