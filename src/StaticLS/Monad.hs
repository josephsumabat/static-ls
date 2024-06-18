module StaticLS.Monad where

import Colog.Core.IO qualified as Colog
import Control.Monad.Reader
import Data.IORef qualified as IORef
import Data.Path (AbsPath)
import StaticLS.HIE.File (MonadHieFile (..))
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.Logger
import StaticLS.Semantic
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options

-- | An environment for running a language server
-- This differs from a `StaticEnv` in that it includes mutable information
-- meant for language server specific functionality
data Env = Env
  { fileEnv :: IORef.IORef Semantic
  , staticEnv :: StaticEnv
  , logger :: Logger
  }

type StaticLsM = ReaderT Env IO

class (HasSemantic m, HasLogger m, HasStaticEnv m, MonadIO m) => HasStaticLsEnv m where
  getEnv :: m Env

instance MonadHieFile StaticLsM where
  getHieFile = HIE.File.getHieFileFromPath

instance HasSemantic StaticLsM where
  getSemantic = do
    fileEnv <- asks (.fileEnv)
    liftIO $ IORef.readIORef fileEnv

instance SetSemantic StaticLsM where
  setSemantic fileEnv = do
    fileEnvRef <- asks (.fileEnv)
    liftIO $ IORef.writeIORef fileEnvRef fileEnv

instance HasLogger StaticLsM where
  getLogger = asks (.logger)

instance HasStaticEnv StaticLsM where
  getStaticEnv = asks (.staticEnv)

initEnv :: AbsPath -> StaticEnvOptions -> Logger -> IO Env
initEnv wsRoot staticEnvOptions loggerToUse = do
  staticEnv <- initStaticEnv wsRoot staticEnvOptions
  fileEnv <- IORef.newIORef mkSemantic
  let logger = Colog.liftLogIO loggerToUse
  pure $
    Env
      { staticEnv = staticEnv
      , fileEnv = fileEnv
      , logger = logger
      }

runStaticLsM :: Env -> StaticLsM a -> IO a
runStaticLsM = flip runReaderT
