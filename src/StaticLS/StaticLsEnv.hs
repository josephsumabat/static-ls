module StaticLS.StaticLsEnv where

import AST.Haskell qualified as Haskell
import Colog.Core.IO qualified as Colog
import Control.Monad.Reader
import Data.HashMap.Strict qualified as HashMap
import Data.IORef qualified as IORef
import Data.Text (Text)
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.FileEnv
import StaticLS.Logger
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options

-- | An environment for running a language server
-- This differs from a `StaticEnv` in that it includes mutable information
-- meant for language server specific functionality
data StaticLsEnv = StaticLsEnv
    { fileEnv :: IORef.IORef FileEnv
    , staticEnv :: StaticEnv
    , logger :: Logger
    }

type StaticLsM = ReaderT StaticLsEnv IO

class (HasFileEnv m, HasLogger m, HasStaticEnv m, MonadIO m) => HasStaticLsEnv m where
    getStaticLsEnv :: m StaticLsEnv

instance HasFileEnv StaticLsM where
    getFileEnv = do
        fileEnv <- asks (.fileEnv)
        liftIO $ IORef.readIORef fileEnv

instance HasLogger StaticLsM where
    getLogger = asks (.logger)

instance HasStaticEnv StaticLsM where
    getStaticEnv = asks (.staticEnv)

initStaticLsEnv :: FilePath -> StaticEnvOptions -> Logger -> IO StaticLsEnv
initStaticLsEnv wsRoot staticEnvOptions loggerToUse = do
    staticEnv <- initStaticEnv wsRoot staticEnvOptions
    fileEnv <- IORef.newIORef mempty
    let logger = Colog.liftLogIO loggerToUse
    pure $
        StaticLsEnv
            { staticEnv = staticEnv
            , fileEnv = fileEnv
            , logger = logger
            }

runStaticLsM :: StaticLsEnv -> StaticLsM a -> IO a
runStaticLsM = flip runReaderT

getHaskell :: (HasFileEnv m, MonadIO m) => LSP.Uri -> m (Maybe Haskell.Haskell)
getHaskell uri = do
    fileState <- getFileState uri
    pure $ (.tree) <$> fileState

getSource :: (HasFileEnv m, MonadIO m) => LSP.Uri -> m (Maybe Text)
getSource uri = do
    fileState <- getFileState uri
    pure $ (.contentsText) <$> fileState

getFileState :: (HasFileEnv m, MonadIO m) => LSP.Uri -> m (Maybe FileState)
getFileState uri = do
    uri <- pure $ LSP.toNormalizedUri uri
    fileStates <- getFileEnv
    let fileState = HashMap.lookup uri fileStates
    pure fileState
