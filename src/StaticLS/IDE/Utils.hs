module StaticLS.IDE.Utils where

import AST.Haskell qualified as Haskell
import Colog.Core.IO qualified as Colog
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict qualified as HashMap
import Data.IORef qualified as IORef
import Data.LineColRange
import Data.Path (AbsPath, toFilePath)
import Data.Pos (LineCol, Pos)
import Data.Pos qualified as Position
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import Data.Text.IO qualified as T
import GHC.Iface.Ext.Types qualified as GHC
import StaticLS.FileEnv
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.HIE.File qualified as Hie
import StaticLS.HIE.File (MonadHieFile (..))
import StaticLS.IDE.FileWith
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.Semantic
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import StaticLS.Utils (isJustOrThrowS)
import System.IO

getHaskell :: (HasSemantic m, MonadThrow m) => AbsPath -> m Haskell.Haskell
getHaskell uri = do
  fileState <- getFileStateThrow uri
  pure fileState.tree

getSourceRope :: (MonadIO m, HasSemantic m, MonadThrow m) => AbsPath -> m Rope
getSourceRope uri = do
  mFileState <- getFileState uri
  case mFileState of
    Just fileState -> pure $ Rope.fromText fileState.contentsText
    Nothing -> fmap Rope.fromText $ liftIO $ T.readFile $ toFilePath uri

getSource :: (HasSemantic m, MonadIO m) => AbsPath -> m Text
getSource uri = do
  mFileState <- getFileState uri
  case mFileState of
    Just fileState -> pure fileState.contentsText
    Nothing -> liftIO $ T.readFile $ toFilePath uri

getHieSource :: (HasStaticEnv m, HasSemantic m, MonadThrow m, MonadIO m) => AbsPath -> MaybeT m (Text)
getHieSource path = do
  hieFile <- HIE.File.getHieFileFromPath path
  let hieSource = T.Encoding.decodeUtf8 $ GHC.hie_hs_src hieFile
  pure $ hieSource

getHieSourceRope :: (HasStaticEnv m, HasSemantic m, MonadThrow m, MonadIO m) => AbsPath -> MaybeT m Rope
getHieSourceRope path = Rope.fromText <$> getHieSource path

getHieToSrcDiffMap :: (HasStaticEnv m, HasSemantic m, MonadThrow m, MonadIO m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getHieToSrcDiffMap path = do
  hieSource <- getHieSource path
  source <- getSource path
  pure $ PositionDiff.getDiffMap hieSource source

getSrcToHieDiffMap :: (HasStaticEnv m, HasSemantic m, MonadThrow m, MonadIO m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getSrcToHieDiffMap path = do
  hieSource <- getHieSource path
  source <- getSource path
  pure $ PositionDiff.getDiffMap source hieSource

getFileState :: (HasSemantic m) => AbsPath -> m (Maybe FileState)
getFileState path = do
  sema <- getSemantic
  let fileState = HashMap.lookup path sema.fileStates
  pure fileState

getFileStateThrow :: (HasSemantic m, MonadThrow m) => AbsPath -> m FileState
getFileStateThrow uri = do
  fileState <- getFileState uri
  isJustOrThrowS ("File not found in virtual file state: " ++ show uri) fileState
