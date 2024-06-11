module StaticLS.StaticLsEnv where

import AST.Haskell qualified as Haskell
import Colog.Core.IO qualified as Colog
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict qualified as HashMap
import Data.IORef qualified as IORef
import Data.Path (AbsPath)
import Data.Pos (LineCol, Pos)
import Data.Pos qualified as Position
import Data.Rope (Rope)
import Data.Text (Text)
import StaticLS.FileEnv
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import StaticLS.Utils (isJustOrThrowS)

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

initStaticLsEnv :: AbsPath -> StaticEnvOptions -> Logger -> IO StaticLsEnv
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

getHaskell :: (HasFileEnv m, MonadThrow m) => AbsPath -> m Haskell.Haskell
getHaskell uri = do
  fileState <- getFileStateThrow uri
  pure fileState.tree

getSourceRope :: (HasFileEnv m, MonadThrow m) => AbsPath -> m Rope
getSourceRope uri = do
  fileState <- getFileStateThrow uri
  pure $ fileState.contentsRope

getSource :: (HasFileEnv m, MonadThrow m) => AbsPath -> m Text
getSource uri = do
  fileState <- getFileStateThrow uri
  pure fileState.contentsText

getFileState :: (HasFileEnv m) => AbsPath -> m (Maybe FileState)
getFileState path = do
  fileStates <- getFileEnv
  let fileState = HashMap.lookup path fileStates
  pure fileState

getFileStateThrow :: (HasFileEnv m, MonadThrow m) => AbsPath -> m FileState
getFileStateThrow uri = do
  fileState <- getFileState uri
  isJustOrThrowS ("File not found: " ++ show uri) fileState

posToHiePos :: (MonadIO m, HasStaticEnv m, HasFileEnv m, MonadThrow m) => AbsPath -> Text -> Pos -> MaybeT m Pos
posToHiePos uri hieSource pos = do
  source <- lift $ getSource uri
  let diff = PositionDiff.diffText source hieSource
  let pos' = PositionDiff.updatePositionUsingDiff pos diff
  pure pos'

hiePosToPos :: (MonadIO m, HasStaticEnv m, HasFileEnv m, MonadThrow m) => AbsPath -> Text -> Pos -> MaybeT m Pos
hiePosToPos path hieSource hiePos = do
  source <- lift $ getSource path
  let diff = PositionDiff.diffText hieSource source
  let pos' = PositionDiff.updatePositionUsingDiff hiePos diff
  pure pos'

hieLineColToLineCol :: (MonadIO m, HasStaticEnv m, HasFileEnv m, MonadThrow m) => AbsPath -> Text -> LineCol -> MaybeT m LineCol
hieLineColToLineCol path hieSource lineCol = do
  source <- lift $ getSource path
  let pos = Position.lineColToPos hieSource lineCol
  pos' <- hiePosToPos path hieSource pos
  let lineCol' = Position.posToLineCol source pos'
  pure lineCol'

lineColToHieLineCol :: (MonadIO m, HasStaticEnv m, HasFileEnv m, MonadThrow m) => AbsPath -> Text -> LineCol -> MaybeT m LineCol
lineColToHieLineCol path hieSource lineCol = do
  source <- lift $ getSource path
  let pos = Position.lineColToPos source lineCol
  pos' <- posToHiePos path hieSource pos
  let lineCol' = Position.posToLineCol hieSource pos'
  pure lineCol'
