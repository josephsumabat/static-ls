module StaticLS.StaticEnv.Options (
  defaultStaticEnvOptions,
  defaultHieDb,
  defaultHieFiles,
  defaultSrcDirs,
  defaultHiFiles,
  StaticEnvOptions (..),
)
where

data StaticEnvOptions = StaticEnvOptions
  { optionHieDbPath :: FilePath
  -- ^ Relative path to hiedb file
  -- hiedb is required for find references and go to definition to work correctly
  , optionHieFilesPath :: FilePath
  -- ^ Relative path to hie files directory
  -- hie files are required for all functionality
  , optionHiFilesPath :: FilePath
  -- ^ Relative path to hi files directory
  , optionSrcDirs :: [FilePath]
  , provideInlays :: Bool
  , inlayLengthCap :: Maybe Int
  , -- Include experimental features?
    experimentalFeatures :: Bool
  }
  deriving (Show, Eq)

defaultHieDb :: FilePath
defaultHieDb = ".hiedb"

defaultHieFiles :: FilePath
defaultHieFiles = ".hiefiles"

defaultSrcDirs :: [FilePath]
defaultSrcDirs = ["src/", "lib/", "app/", "test/"]

defaultHiFiles :: FilePath
defaultHiFiles = ".hifiles"

defaultStaticEnvOptions :: StaticEnvOptions
defaultStaticEnvOptions =
  StaticEnvOptions
    { optionHieDbPath = defaultHieDb
    , optionHieFilesPath = defaultHieFiles
    , optionSrcDirs = defaultSrcDirs
    , optionHiFilesPath = defaultHiFiles
    , provideInlays = True
    , inlayLengthCap = Just 32
    , experimentalFeatures = False
    }
