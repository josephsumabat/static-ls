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
    LoggerM,
    Logger,
    HasCallStack,
    logWith,
    logError,
    logInfo,
    logWarn,
)
where

import Control.Exception (Exception, IOException, SomeException, catch)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT)
import Database.SQLite.Simple (SQLError)
import qualified HieDb
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import System.FilePath ((</>))
import Data.Text (Text)
import qualified Colog.Core as Colog
import StaticLS.Logger
import qualified StaticLS.Logger as Logger
import Control.Monad.Reader

runStaticLs :: StaticEnv -> StaticLs a -> IO a
runStaticLs = flip runReaderT

type HieDbPath = FilePath
type HieFilePath = FilePath
type HiFilePath = FilePath

data HieDbException
    = HieDbIOException IOException
    | HieDbSqlException SQLError
    | HieDbNoHieDbSourceException
    | HieDbOtherException SomeException
    deriving (Show)

instance Exception HieDbException

type Logger = LoggerM StaticLs

-- | Static environment used to fetch data
data StaticEnv = StaticEnv
    { hieDbPath :: HieDbPath
    -- ^ Path to the hiedb file
    , hieFilesPath :: HieFilePath
    , hiFilesPath :: HiFilePath
    , wsRoot :: FilePath
    -- ^ workspace root
    , srcDirs :: [FilePath]
    , logger :: Logger
    -- ^ directories to search for source code in order of priority
    }

type StaticLs = ReaderT StaticEnv IO

type HasStaticEnv = MonadReader StaticEnv

getStaticEnv :: (HasStaticEnv m) => m StaticEnv
getStaticEnv = ask

initStaticEnv :: FilePath -> StaticEnvOptions -> LoggerM IO -> IO StaticEnv
initStaticEnv wsRoot staticEnvOptions logger =
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
                    , logger = Colog.liftLogIO logger
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
                        `catch` (\(e :: SomeException) -> pure . Left $ HieDbOtherException e)
            )
                staticEnv.hieDbPath

-- | Run an hiedb action with the MaybeT Monad
runHieDbMaybeT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> MaybeT m a
runHieDbMaybeT = exceptToMaybeT . runHieDbExceptT

logWith :: Colog.Severity -> Text -> Logger.CallStack -> StaticLs ()
logWith severity text stack = do
    env <- ask
    env.logger Colog.<& Logger.Msg { severity, text, stack }
    
logInfo :: HasCallStack => Text -> StaticLs ()
logInfo text = logWith Colog.Info text Logger.callStack

logError :: HasCallStack => Text -> StaticLs ()
logError text = logWith Colog.Error text Logger.callStack

logWarn :: HasCallStack => Text -> StaticLs ()
logWarn text = logWith Colog.Warning text Logger.callStack