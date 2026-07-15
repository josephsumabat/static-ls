module StaticLS.StaticEnv.Options (
  defaultStaticEnvOptions,
  StaticEnvOptions (..),
)
where

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
  , restartIntervalMinutes :: Int
  -- ^ Restart (exit) the server after this many minutes so the client relaunches it.
  -- Mitigates a known memory leak that grows over long-running sessions.
  -- A value of 0 (or less) disables the timed restart.
  , -- Include experimental features?
    experimentalFeatures :: Bool
  }
  deriving (Show, Eq)

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

-- | Default restart interval in minutes (2 hours). See 'restartIntervalMinutes'.
defaultRestartIntervalMinutes :: Int
defaultRestartIntervalMinutes = 120

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
    , restartIntervalMinutes = defaultRestartIntervalMinutes
    , experimentalFeatures = False
    }
