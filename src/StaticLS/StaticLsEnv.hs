module StaticLS.StaticLsEnv where

import AST.Haskell qualified as Haskell
import Colog.Core.IO qualified as Colog
import Control.Monad.Catch
import Control.Monad.Reader
import Data.HashMap.Strict qualified as HashMap
import Data.IORef qualified as IORef
import Data.Text (Text)
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.FileEnv
import StaticLS.Logger
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import StaticLS.Utils (isJustOrThrow)

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

instance SetFileEnv StaticLsM where
  setFileEnv fileEnv = do
    fileEnvRef <- asks (.fileEnv)
    liftIO $ IORef.writeIORef fileEnvRef fileEnv

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

getHaskell :: (HasFileEnv m, MonadThrow m) => LSP.Uri -> m Haskell.Haskell
getHaskell uri = do
  fileState <- getFileStateThrow uri
  pure $ fileState.tree

getSource :: (HasFileEnv m, MonadThrow m) => LSP.Uri -> m Text
getSource uri = do
  fileState <- getFileStateThrow uri
  pure $ fileState.contentsText

getFileState :: (HasFileEnv m) => LSP.Uri -> m (Maybe FileState)
getFileState uri = do
  uri <- pure $ LSP.toNormalizedUri uri
  fileStates <- getFileEnv
  let fileState = HashMap.lookup uri fileStates
  pure fileState

getFileStateThrow :: (HasFileEnv m, MonadThrow m) => LSP.Uri -> m FileState
getFileStateThrow uri = do
  fileState <- getFileState uri
  isJustOrThrow ("File not found: " ++ show uri) fileState

posToHiePos :: (HasFileEnv m, MonadThrow m) => LSP.Uri -> LSP.Position -> m LSP.Position
posToHiePos uri pos = do
  source <- getSource uri
  undefined
