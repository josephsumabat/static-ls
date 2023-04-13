module StaticLS.StaticEnv.Options (
    defaultStaticEnvOptions,
    StaticEnvOptions (..),
)
where

data StaticEnvOptions = StaticEnvOptions
    { optionHieDbPath :: Maybe FilePath
    -- ^ Relative path to hiedb file
    -- hiedb is required for find references and go to definition to work correctly
    , optionHieFilesPath :: Maybe FilePath
    -- ^ Relative path to hie files directory
    -- hie files are required for all functionality
    }

defaultStaticEnvOptions :: StaticEnvOptions
defaultStaticEnvOptions =
    StaticEnvOptions
        { optionHieDbPath = Just ".hiedb"
        , optionHieFilesPath = Just ".hiefiles"
        }
