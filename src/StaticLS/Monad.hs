module StaticLS.Monad where

import Arborist.Files
import Colog.Core.IO qualified as Colog
import Control.Monad.Reader
import Data.Path (AbsPath)
import Data.Path qualified as Path
import StaticLS.IDE.Monad
import StaticLS.IDE.Monad qualified as IDE.Monad
import StaticLS.Logger
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options

-- | An environment for running a language server
-- This differs from a `StaticEnv` in that it includes mutable information
-- meant for language server specific functionality
data Env = Env
  { ideEnv :: IdeEnv
  , staticEnv :: StaticEnv
  , logger :: Logger
  }

type StaticLsM = ReaderT Env IO

class HasEnv m where
  getEnv :: m Env

instance HasEnv StaticLsM where
  getEnv = ask

instance IDE.Monad.HasIdeEnv StaticLsM where
  getIdeEnv = do
    env <- getEnv
    pure $ env.ideEnv

instance HasLogger StaticLsM where
  getLogger = asks (.logger)

instance HasStaticEnv StaticLsM where
  getStaticEnv = asks (.staticEnv)

initEnv :: AbsPath -> StaticEnvOptions -> Logger -> IO Env
initEnv wsRoot staticEnvOptions loggerToUse = do
  staticEnv <- initStaticEnv wsRoot staticEnvOptions
  ideEnv <- IDE.Monad.newIdeEnv staticEnv.srcDirs
  let logger = Colog.liftLogIO loggerToUse
  pure $
    Env
      { staticEnv = staticEnv
      , ideEnv
      , logger = logger
      }

runStaticLsM :: Env -> StaticLsM a -> IO a
runStaticLsM = flip runReaderT
