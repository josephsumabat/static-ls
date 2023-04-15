{-# LANGUAGE ConstraintKinds #-}

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

import Control.Exception (IOException, catch)
import Control.Monad.Exception (Exception)
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
    deriving (Show)

instance Exception HieDbException

-- | Static environment used to fetch data
data StaticEnv = StaticEnv
    { hieDbPath :: Maybe HieDbPath
    -- ^ Path to the hiedb file
    , hieFilesPath :: Maybe HieFilePath
    , hscEnv :: GHC.HscEnv
    -- ^ static ghc compiler environment
    , nameCache :: GHC.NameCache
    -- ^ name cache - used for reading hie files
    , wsRoot :: FilePath
    -- ^ workspace root
    }

type StaticLs = ReaderT StaticEnv IO

type HasStaticEnv = MonadReader StaticEnv

getStaticEnv :: HasStaticEnv m => m StaticEnv
getStaticEnv = ask

initStaticEnv :: FilePath -> StaticEnvOptions -> IO StaticEnv
initStaticEnv wsRoot staticEnvOptions =
    do
        let databasePath = fmap (wsRoot </>) staticEnvOptions.optionHieDbPath
            hieFilesPath = fmap (wsRoot </>) staticEnvOptions.optionHieFilesPath
        -- TODO: find out if this is safe to do or if we should just use GhcT
        hscEnv <- GHC.runGhc (Just GHC.libdir) GHC.getSession
        -- TODO: not sure what the first parameter to name cache is - find out
        nameCache <- GHC.initNameCache 'a' []

        let serverStaticEnv =
                StaticEnv
                    { hieDbPath = databasePath
                    , hieFilesPath = hieFilesPath
                    , hscEnv = hscEnv
                    , nameCache = nameCache
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
                )
                staticEnv.hieDbPath

-- | Run an hiedb action with the MaybeT Monad
runHieDbMaybeT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> MaybeT m a
runHieDbMaybeT = exceptToMaybeT . runHieDbExceptT
