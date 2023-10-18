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
import qualified GHC
import qualified GHC.Paths as GHC
import qualified GHC.Types.Name.Cache as GHC
import qualified HieDb
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import System.FilePath ((</>))

runStaticLs :: StaticEnv -> StaticLs a -> IO a
runStaticLs = flip runReaderT

type HieDbPath = FilePath
type HieFilePath = FilePath

data HieDbException
    = HieDbIOException IOException
    | HieDbSqlException SQLError
    | HieDbNoHieDbSourceException
    | HieDbOtherException
    deriving (Show)

instance Exception HieDbException

-- | Static environment used to fetch data
data StaticEnv = StaticEnv
    { hieDbPath :: Maybe HieDbPath
    -- ^ Path to the hiedb file
    , hieFilesPath :: HieFilePath
    , wsRoot :: FilePath
    -- ^ workspace root
    }

type StaticLs = ReaderT StaticEnv IO

type HasStaticEnv = MonadReader StaticEnv

getStaticEnv :: (HasStaticEnv m) => m StaticEnv
getStaticEnv = ask

initStaticEnv :: FilePath -> StaticEnvOptions -> IO StaticEnv
initStaticEnv wsRoot staticEnvOptions =
    do
        let databasePath = fmap (wsRoot </>) (Just staticEnvOptions.optionHieDbPath)
            hieFilesPath = wsRoot </> staticEnvOptions.optionHieFilesPath

        let serverStaticEnv =
                StaticEnv
                    { hieDbPath = databasePath
                    , hieFilesPath = hieFilesPath
                    , wsRoot = wsRoot
                    }
        pure serverStaticEnv

-- | Run an hiedb action in an exceptT
runHieDbExceptT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> ExceptT HieDbException m a
runHieDbExceptT hieDbFn =
    getStaticEnv
        >>= \staticEnv ->
            maybe
                (ExceptT . pure . Left $ HieDbNoHieDbSourceException)
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
