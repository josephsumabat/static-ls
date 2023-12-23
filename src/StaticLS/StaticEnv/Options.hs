module StaticLS.StaticEnv.Options (
    defaultStaticEnvOptions,
    defaultHieDb,
    defaultHieFiles,
    defaultSrcDirs,
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
    , optionSrcDirs :: [FilePath]
    }
    deriving (Show, Eq)

defaultHieDb :: FilePath
defaultHieDb = ".hiedb"

defaultHieFiles :: FilePath
defaultHieFiles = ".hiefiles"

defaultSrcDirs :: [FilePath]
defaultSrcDirs = ["src/", "lib/", "app/", "test/"]

defaultStaticEnvOptions :: StaticEnvOptions
defaultStaticEnvOptions =
    StaticEnvOptions
        { optionHieDbPath = defaultHieDb
        , optionHieFilesPath = defaultHieFiles
        , optionSrcDirs = defaultSrcDirs
        }
