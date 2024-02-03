{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.StaticEnv (
    initStaticEnv,
    runStaticLs,
    getStaticEnv,
    runHieDbExceptT,
    runHieDbMaybeT,
    StaticEnv (..),
    StaticLs,
    HieDbPath,
    HieFilePath,
    HiFilePath,
    HasStaticEnv,
)
where

import Control.Exception (Exception, IOException, SomeException, catch)
import Control.Monad.IO.Unlift (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Database.SQLite.Simple (SQLError)
import qualified HieDb
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import System.FilePath ((</>))

runStaticLs :: StaticEnv -> StaticLs a -> IO a
runStaticLs = flip runReaderT

type HieDbPath = FilePath
type HieFilePath = FilePath
type HiFilePath = FilePath

data HieDbException
    = HieDbIOException IOException
    | HieDbSqlException SQLError
    | HieDbNoHieDbSourceException
    | HieDbOtherException
    deriving (Show)

instance Exception HieDbException

-- | Static environment used to fetch data
data StaticEnv = StaticEnv
    { hieDbPath :: HieDbPath
    -- ^ Path to the hiedb file
    , hieFilesPath :: HieFilePath
    , hiFilesPath :: HiFilePath
    , wsRoot :: FilePath
    -- ^ workspace root
    , srcDirs :: [FilePath]
    -- ^ directories to search for source code in order of priority
    }
    deriving (Eq, Show)

type StaticLs = ReaderT StaticEnv IO

type HasStaticEnv = MonadReader StaticEnv

getStaticEnv :: (HasStaticEnv m) => m StaticEnv
getStaticEnv = ask

initStaticEnv :: FilePath -> StaticEnvOptions -> IO StaticEnv
initStaticEnv wsRoot staticEnvOptions =
    do
        let databasePath = wsRoot </> staticEnvOptions.optionHieDbPath
            hieFilesPath = wsRoot </> staticEnvOptions.optionHieFilesPath
            srcDirs = fmap (wsRoot </>) staticEnvOptions.optionSrcDirs
            hiFilesPath = wsRoot </> staticEnvOptions.optionHiFilesPath

        let serverStaticEnv =
                StaticEnv
                    { hieDbPath = databasePath
                    , hieFilesPath = hieFilesPath
                    , hiFilesPath = hiFilesPath
                    , wsRoot = wsRoot
                    , srcDirs = srcDirs
                    }

        pure serverStaticEnv

-- | Run an hiedb action in an exceptT
runHieDbExceptT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> ExceptT HieDbException m a
runHieDbExceptT hieDbFn =
    getStaticEnv
        >>= \staticEnv ->
            ( \hiedbPath ->
                ExceptT . liftIO $
                    HieDb.withHieDb hiedbPath (fmap Right . hieDbFn)
                        `catch` (pure . Left . HieDbIOException)
                        `catch` (pure . Left . HieDbSqlException)
                        `catch` (\(_ :: SomeException) -> pure . Left $ HieDbOtherException)
            )
                staticEnv.hieDbPath

-- | Run an hiedb action with the MaybeT Monad
runHieDbMaybeT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> MaybeT m a
runHieDbMaybeT = exceptToMaybeT . runHieDbExceptT
