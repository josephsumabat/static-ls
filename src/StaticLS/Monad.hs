module StaticLS.Monad where

import Colog.Core.IO qualified as Colog
import Control.Monad.Reader
import Data.ConcurrentCache (ConcurrentCache)
import Data.ConcurrentCache qualified as ConcurrentCache
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Path (AbsPath)
import StaticLS.IDE.Monad
import StaticLS.IDE.Monad qualified as IDE.Monad
import StaticLS.Logger
import StaticLS.Semantic
import StaticLS.Semantic qualified as Semantic
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import UnliftIO.IORef qualified as IORef

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

-- instance TouchCachesParallel StaticLsM where
--   touchCachesParallel = touchCachesParallelImpl

-- instance HasHieCache StaticLsM where
--   getHieCacheMap = do
--     hieCacheRef <- asks (.hieCache)
--     hieCache <- liftIO $ IORef.readIORef hieCacheRef
--     pure hieCache

-- instance SetHieCache StaticLsM where
--   setHieCacheMap !hieCache = do
--     hieCacheRef <- asks (.hieCache)
--     liftIO $ IORef.writeIORef hieCacheRef hieCache

-- instance HasDiffCacheRef StaticLsM where
--   getDiffCacheRef = asks (.diffCache)

-- instance RemovePath StaticLsM where
--   removePath = removePathImpl

-- instance GetDiffCache StaticLsM where
--   getDiffCache = getDiffCacheImpl

-- instance MonadHieFile StaticLsM where
--   getHieCache = getHieCacheImpl

-- instance GetFileState StaticLsM where
--   getFileState path = do
--     env <- ask
--     ConcurrentCache.insert
--       path
--       ( do
--           fileState <- getFileStateResult path
--           case fileState of
--             Just fileState' -> pure fileState'
--             Nothing -> pure Semantic.emptyFileState
--       )
--       env.fileStateCache

-- instance HasSemantic StaticLsM where
--   getSemantic = do
--     fileEnv <- asks (.fileEnv)
--     liftIO $ IORef.readIORef fileEnv

-- instance SetSemantic StaticLsM where
--   setSemantic !fileEnv = do
--     fileEnvRef <- asks (.fileEnv)
--     liftIO $ IORef.writeIORef fileEnvRef fileEnv

instance HasLogger StaticLsM where
  getLogger = asks (.logger)

instance HasStaticEnv StaticLsM where
  getStaticEnv = asks (.staticEnv)

initEnv :: AbsPath -> StaticEnvOptions -> Logger -> IO Env
initEnv wsRoot staticEnvOptions loggerToUse = do
  staticEnv <- initStaticEnv wsRoot staticEnvOptions
  ideEnv <- IDE.Monad.newIdeEnv
  let logger = Colog.liftLogIO loggerToUse
  pure $
    Env
      { staticEnv = staticEnv
      , ideEnv
      , logger = logger
      }

runStaticLsM :: Env -> StaticLsM a -> IO a
runStaticLsM = flip runReaderT
