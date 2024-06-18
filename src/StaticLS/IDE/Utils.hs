module StaticLS.IDE.Utils where

import AST.Haskell qualified as Haskell
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict qualified as HashMap
import Data.Path (AbsPath, toFilePath)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text.IO qualified as T
import StaticLS.HIE.File (HieFile (..), MonadHieFile (..))
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.Semantic
import StaticLS.Semantic qualified as Semantic
import StaticLS.StaticEnv

type MonadIde m =
  ( MonadThrow m,
    MonadHieFile m,
    HasStaticEnv m,
    HasSemantic m,
    SetSemantic m,
    HasLogger m
  )

getHaskell :: (MonadIde m, MonadIO m) => AbsPath -> m Haskell.Haskell
getHaskell uri = do
  fileState <- getFileStateThrow uri
  pure fileState.tree

getSourceRope :: (MonadIde m, MonadIO m) => AbsPath -> m Rope
getSourceRope uri = do
  mFileState <- getFileStateThrow uri
  pure mFileState.contentsRope

getSource :: (MonadIde m, MonadIO m) => AbsPath -> m Text
getSource uri = do
  mFileState <- getFileStateThrow uri
  pure mFileState.contentsText

getHieSource :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m (Text)
getHieSource path = do
  hieFile <- getHieFile path
  pure hieFile.src

getHieSourceRope :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m Rope
getHieSourceRope path = Rope.fromText <$> getHieSource path

getHieToSrcDiffMap :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getHieToSrcDiffMap path = do
  hieSource <- getHieSource path
  source <- getSource path
  pure $ PositionDiff.getDiffMap hieSource source

getSrcToHieDiffMap :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getSrcToHieDiffMap path = do
  hieSource <- getHieSource path
  source <- getSource path
  pure $ PositionDiff.getDiffMap source hieSource

getFileStateThrow :: (MonadIde m, MonadIO m) => AbsPath -> m FileState
getFileStateThrow path = do
  sema <- getSemantic
  let fileState = HashMap.lookup path sema.fileStates
  case fileState of
    Just fileState -> pure $ fileState
    Nothing -> do
      contents <- liftIO $ T.readFile $ toFilePath path
      let contentsRope = Rope.fromText contents
      let fileState = Semantic.mkFileState contents contentsRope
      Semantic.setFileState path fileState
      pure fileState
