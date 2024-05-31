{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.StaticEnv (
    initStaticEnv,
    runStaticLs,
    getStaticEnv,
    runHieDbExceptT,
    runHieDbMaybeT,
    getFileState,
    StaticEnv (..),
    StaticLs,
    HieDbPath,
    HieFilePath,
    HiFilePath,
    HasStaticEnv,
    LoggerM,
    Logger,
    HasCallStack,
    FileState(..),
    logWith,
    logError,
    logInfo,
    logWarn,
)
where

import Colog.Core qualified as Colog
import Control.Exception (Exception, IOException, SomeException, catch)
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT)
import Data.Text (Text)
import Database.SQLite.Simple (SQLError)
import HieDb qualified
import StaticLS.Logger
import StaticLS.Logger qualified as Logger
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import System.FilePath ((</>))
import qualified Data.Text.Utf16.Rope.Mixed as Rope
import qualified TreeSitter.Api as TS
import Data.Tree (Tree)
import Data.HashMap.Strict (HashMap)
import Language.LSP.Protocol.Types qualified as LSP
import Data.HashMap.Strict qualified as HashMap
import UnliftIO.IORef qualified as IORef

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

type Logger = LoggerM IO

data FileState = FileState {
  contents :: Rope.Rope,
  contentsText :: Text,
  tree :: Tree TS.Node
  }
  
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
    , logger :: Logger
    , fileStates :: IORef.IORef (HashMap LSP.NormalizedUri FileState)
    }

getFileState :: LSP.Uri -> StaticLs (Maybe FileState)
getFileState uri = do
  uri <- pure $ LSP.toNormalizedUri uri
  env <- ask
  fileStates <- IORef.readIORef env.fileStates
  let fileState = HashMap.lookup uri fileStates
  pure fileState
  
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
        fileStates <- IORef.newIORef mempty
        let serverStaticEnv =
                StaticEnv
                    { hieDbPath = databasePath
                    , hieFilesPath = hieFilesPath
                    , hiFilesPath = hiFilesPath
                    , wsRoot = wsRoot
                    , srcDirs = srcDirs
                    , logger = Colog.liftLogIO logger
                    , fileStates
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

logWith :: (HasCallStack, HasStaticEnv m, MonadIO m) => Colog.Severity -> Text -> Logger.CallStack -> m ()
logWith severity text stack = do
    env <- ask
    liftIO $ env.logger Colog.<& Logger.Msg{severity, text, stack}

logInfo :: (HasCallStack, HasStaticEnv m, MonadIO m) => Text -> m ()
logInfo text = logWith Colog.Info text Logger.callStack

logError :: (HasCallStack, HasStaticEnv m, MonadIO m) => Text -> m ()
logError text = logWith Colog.Error text Logger.callStack

logWarn :: (HasCallStack, HasStaticEnv m, MonadIO m) => Text -> m ()
logWarn text = logWith Colog.Warning text Logger.callStack
