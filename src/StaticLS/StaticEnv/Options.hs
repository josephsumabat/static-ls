{-# LANGUAGE TemplateHaskell #-}

module StaticLS.StaticEnv.Options (
  defaultStaticEnvOptions,
  StaticEnvOptions (..),
  IssueTrackerConfig (..),
)
where

import Data.Aeson.TH

data StaticEnvOptions = StaticEnvOptions
  { optionHieDbPath :: FilePath
  -- ^ Relative path to hiedb file
  -- hiedb is required for find references and go to definition to work correctly
  , optionHieDirs :: [FilePath]
  -- ^ Relative path to hie files directory
  -- hie files are required for all functionality
  , optionHiFilesPath :: FilePath
  -- ^ Relative path to hi files directory
  , optionSrcDirs :: [FilePath]
  , optionImmutableSrcDirs :: [FilePath]
  , provideInlays :: Bool
  , inlayLengthCap :: Maybe Int
  , fourmoluCommand :: Maybe FilePath
  -- ^ path to fourmolu
  , -- Include experimental features?
    experimentalFeatures :: Bool
  , issueTracker :: Maybe IssueTrackerConfig
  }
  deriving (Show, Eq)

data IssueTrackerConfig = IssueTrackerConfig
  { url :: String
  -- ^ URL template with @{org}@ and @{ticketId}@ placeholders.
  -- Supported ticket ID format: TEAM-1234 (2+ uppercase letters, hyphen, digits).
  , org :: String
  -- ^ Organization name, substituted for {org} in the URL template.
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''IssueTrackerConfig)

defaultHieDb :: FilePath
defaultHieDb = ".hiedb"

defaultHieFiles :: [FilePath]
defaultHieFiles = [".hiefiles"]

defaultSrcDirs :: [FilePath]
defaultSrcDirs = ["src/", "lib/", "app/", "test/"]

defaultImmutableSrcDirs :: [FilePath]
defaultImmutableSrcDirs = []

defaultHiFiles :: FilePath
defaultHiFiles = ".hifiles"

defaultStaticEnvOptions :: StaticEnvOptions
defaultStaticEnvOptions =
  StaticEnvOptions
    { optionHieDbPath = defaultHieDb
    , optionHieDirs = defaultHieFiles
    , optionSrcDirs = defaultSrcDirs
    , optionImmutableSrcDirs = defaultImmutableSrcDirs
    , optionHiFilesPath = defaultHiFiles
    , provideInlays = True
    , inlayLengthCap = Just 32
    , fourmoluCommand = Nothing
    , experimentalFeatures = False
    , issueTracker = Nothing
    }
