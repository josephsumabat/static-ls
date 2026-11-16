{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.StaticEnv (
  initStaticEnv,
  getStaticEnv,
  runHieDbExceptT,
  runHieDbMaybeT,
  StaticEnv (..),
  HieDbPath,
  HieFilePath,
  HiFilePath,
  HasStaticEnv,
  HasCallStack,
  runStaticEnv,
)
where

import Control.Concurrent (getNumCapabilities)
import Control.Exception (Exception, IOException, SomeException, catch, mask, onException)
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Database.SQLite.Simple (SQLError)
import Database.SQLite.Simple qualified as SQL
import HieDb qualified
import StaticLS.Logger
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import System.FilePath qualified as FilePath

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

-- | Imuttable references to "static sources" of language information. Should
--     be low overhead and should be sources for language information only.
--
-- Functions that make use of this should ensure that they are robust against
-- exceptions i.e. that the language server does not crash if something goes
-- wrong with fetching information from a static source
data StaticEnv = StaticEnv
  { hieDbPath :: AbsPath
  -- ^ Path to the hiedb file
  , hieDirs :: [AbsPath]
  , hiFilesPath :: AbsPath
  , ghciErrorFile :: AbsPath
  , wsRoot :: AbsPath
  , modelsFilesDir :: AbsPath
  -- ^ workspace root
  , mutableSrcDirs :: [AbsPath]
  -- ^ directories to search for source code in order of priority (mutable directories)
  , immutableSrcDirs :: [AbsPath]
  -- ^ directories to search for source code in order of priority (immutable directories)
  , allSrcDirs :: [AbsPath]
  -- ^ directories to search for source code in order of priority (muttable and imuttable directories)
  , fourmoluCommand :: Maybe FilePath
  -- ^ path to fourmolu binary
  , hieDbPool :: HieDbPool
  }

data HieDbPool = HieDbPool
  { idle :: IORef [HieDb.HieDb]
  , maxIdle :: Int
  }

newHieDbPool :: IO HieDbPool
newHieDbPool = do
  idle <- newIORef []
  maxIdle <- getNumCapabilities
  pure HieDbPool {idle, maxIdle}

withPooledHieDb :: HieDbPool -> FilePath -> (HieDb.HieDb -> IO a) -> IO a
withPooledHieDb pool path f = mask \restore -> do
  db <- acquire
  res <- restore (f db) `onException` SQL.close (HieDb.getConn db)
  release db
  pure res
 where
  acquire = do
    existing <- atomicModifyIORef' pool.idle \case
      [] -> ([], Nothing)
      db : dbs -> (dbs, Just db)
    maybe open pure existing
  open = do
    conn <- SQL.open path
    HieDb.checkVersion pure (HieDb.HieDb conn) `onException` SQL.close conn
  release db = do
    kept <- atomicModifyIORef' pool.idle \dbs ->
      if length dbs < pool.maxIdle then (db : dbs, True) else (dbs, False)
    unless kept $ SQL.close (HieDb.getConn db)

class (Monad m) => HasStaticEnv m where
  getStaticEnv :: m StaticEnv

instance (Monad m) => HasStaticEnv (ReaderT StaticEnv m) where
  getStaticEnv = ask

instance (HasStaticEnv m) => HasStaticEnv (MaybeT m) where
  getStaticEnv = lift getStaticEnv

instance (HasStaticEnv m) => HasStaticEnv (ExceptT e m) where
  getStaticEnv = lift getStaticEnv

runStaticEnv :: StaticEnv -> ReaderT StaticEnv IO a -> IO a
runStaticEnv = flip runReaderT

initStaticEnv :: AbsPath -> StaticEnvOptions -> IO StaticEnv
initStaticEnv wsRoot staticEnvOptions = do
  let databasePath = wsRoot Path.</> (Path.filePathToRel staticEnvOptions.optionHieDbPath)
      hieDirs = fmap ((wsRoot Path.</>) . Path.filePathToRel) (staticEnvOptions.optionHieDirs)
      mutableSrcDirs = fmap ((wsRoot Path.</>) . Path.filePathToRel) (staticEnvOptions.optionSrcDirs)
      immutableSrcDirs = fmap ((wsRoot Path.</>) . Path.filePathToRel) (staticEnvOptions.optionImmutableSrcDirs)
      allSrcDirs = mutableSrcDirs <> immutableSrcDirs
      hiFilesPath = wsRoot Path.</> Path.filePathToRel staticEnvOptions.optionHiFilesPath
      ghciErrorFile = wsRoot Path.</> Path.filePathToRel (FilePath.normalise staticEnvOptions.optionGhciErrorFile)
  hieDbPool <- newHieDbPool
  let serverStaticEnv =
        StaticEnv
          { hieDbPath = databasePath
          , hieDirs = hieDirs
          , hiFilesPath = hiFilesPath
          , ghciErrorFile = ghciErrorFile
          , wsRoot = wsRoot
          , modelsFilesDir = wsRoot Path.</> "config" Path.</> "modelsFiles"
          , mutableSrcDirs = mutableSrcDirs
          , immutableSrcDirs = immutableSrcDirs
          , allSrcDirs = allSrcDirs
          , fourmoluCommand = staticEnvOptions.fourmoluCommand
          , hieDbPool
          }
  pure serverStaticEnv

-- | Run an hiedb action in an exceptT
runHieDbExceptT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> ExceptT HieDbException m a
runHieDbExceptT hieDbFn =
  getStaticEnv
    >>= \staticEnv ->
      ( \hiedbPath ->
          ExceptT . liftIO $
            withPooledHieDb staticEnv.hieDbPool (Path.toFilePath hiedbPath) (fmap Right . hieDbFn)
              `catch` (pure . Left . HieDbIOException)
              `catch` (pure . Left . HieDbSqlException)
              `catch` (\(e :: SomeException) -> pure . Left $ HieDbOtherException e)
      )
        staticEnv.hieDbPath

-- | Run an hiedb action with the MaybeT Monad
runHieDbMaybeT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> MaybeT m a
runHieDbMaybeT = exceptToMaybeT . runHieDbExceptT
