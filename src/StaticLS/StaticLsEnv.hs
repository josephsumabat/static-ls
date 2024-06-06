module StaticLS.StaticLsEnv where

import AST.Haskell qualified as Haskell
import Colog.Core.IO qualified as Colog
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict qualified as HashMap
import Data.IORef qualified as IORef
import Data.Text (Text)
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.FileEnv
import StaticLS.Logger
import Data.Pos (LineCol, Pos)
import Data.Pos qualified as Position
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import StaticLS.Utils (isJustOrThrow)
import Data.Path (AbsPath)

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

posToHiePos :: (MonadIO m, HasStaticEnv m, HasFileEnv m, MonadThrow m) => LSP.Uri -> Text -> Pos -> MaybeT m Pos
posToHiePos uri hieSource pos = do
  source <- lift $ getSource uri
  let diff = PositionDiff.diffText source hieSource
  let pos' = PositionDiff.updatePositionUsingDiff pos diff
  pure pos'

lineColToHieLineCol :: (MonadIO m, HasStaticEnv m, HasFileEnv m, MonadThrow m) => LSP.Uri -> Text -> LineCol -> MaybeT m LineCol
lineColToHieLineCol uri hieSource lineCol = do
  source <- lift $ getSource uri
  let pos = Position.lineColToPos source lineCol
  pos' <- posToHiePos uri hieSource pos
  let lineCol' = Position.posToLineCol hieSource pos'
  pure lineCol'
