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

import Control.Exception (Exception, IOException, SomeException, catch)
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Database.SQLite.Simple (SQLError)
import HieDb qualified
import StaticLS.Logger
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))

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
  }

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
      hiFilesPath = wsRoot Path.</> (Path.filePathToRel staticEnvOptions.optionHiFilesPath)
  let serverStaticEnv =
        StaticEnv
          { hieDbPath = databasePath
          , hieDirs = hieDirs
          , hiFilesPath = hiFilesPath
          , wsRoot = wsRoot
          , modelsFilesDir = wsRoot Path.</> "config" Path.</> "modelsFiles"
          , mutableSrcDirs = mutableSrcDirs
          , immutableSrcDirs = immutableSrcDirs
          , allSrcDirs = allSrcDirs
          , fourmoluCommand = staticEnvOptions.fourmoluCommand
          }
  pure serverStaticEnv

-- | Run an hiedb action in an exceptT
runHieDbExceptT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> ExceptT HieDbException m a
runHieDbExceptT hieDbFn =
  getStaticEnv
    >>= \staticEnv ->
      ( \hiedbPath ->
          ExceptT . liftIO $
            HieDb.withHieDb (Path.toFilePath hiedbPath) (fmap Right . hieDbFn)
              `catch` (pure . Left . HieDbIOException)
              `catch` (pure . Left . HieDbSqlException)
              `catch` (\(e :: SomeException) -> pure . Left $ HieDbOtherException e)
      )
        staticEnv.hieDbPath

-- | Run an hiedb action with the MaybeT Monad
runHieDbMaybeT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> MaybeT m a
runHieDbMaybeT = exceptToMaybeT . runHieDbExceptT
