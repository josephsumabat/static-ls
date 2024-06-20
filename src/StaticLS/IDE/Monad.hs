module StaticLS.IDE.Monad (
  MonadIde,
  getHaskell,
  getSourceRope,
  getSource,
  getHieToSrcDiffMap,
  getSrcToHieDiffMap,
  getFileStateThrow,
  getHieSourceRope,
  getHieSource,
  getHieFile,
  getHieCacheImpl,
  CachedHieFile (..),
  MonadHieFile (..),
  SetHieCache (..),
  HasHieCache (..),
  DiffCache (..),
  HasDiffCacheRef (..),
  GetDiffCache (..),
  getDiffCacheImpl,
  getHieToSource,
  getSourceToHie,
  removeDiffCache,
  removePath,
  removeHieFromSourcePath,
  onNewSource,
  getHieTokenMap,
  getTokenMap,
)
where

import AST.Haskell qualified as Haskell
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict qualified as HashMap
import Data.Path (AbsPath, toFilePath)
import Data.RangeMap (RangeMap)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.Semantic qualified as Semantic
import StaticLS.StaticEnv
import UnliftIO.IORef qualified as IORef

type MonadIde m =
  ( MonadThrow m
  , MonadHieFile m
  , HasStaticEnv m
  , Semantic.HasSemantic m
  , Semantic.SetSemantic m
  , HasLogger m
  , GetDiffCache m
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

getFileStateThrow :: (MonadIde m, MonadIO m) => AbsPath -> m Semantic.FileState
getFileStateThrow path = do
  sema <- Semantic.getSemantic
  let fileState = HashMap.lookup path sema.fileStates
  case fileState of
    Just fileState -> pure fileState
    Nothing -> do
      contents <- liftIO $ T.readFile $ toFilePath path
      let contentsRope = Rope.fromText contents
      let fileState = Semantic.mkFileState contents contentsRope
      Semantic.setFileState path fileState
      pure fileState

type HieCacheMap = HashMap.HashMap AbsPath CachedHieFile

-- keep these fields lazy
data CachedHieFile = CachedHieFile
  { hieSource :: Text
  , hieSourceRope :: Rope
  , file :: HIE.File.HieFile
  , hieTokenMap :: RangeMap PositionDiff.Token
  }

class HasHieCache m where
  getHieCacheMap :: m HieCacheMap

instance (Monad m, HasHieCache m) => HasHieCache (MaybeT m) where
  getHieCacheMap = lift getHieCacheMap

class SetHieCache m where
  setHieCacheMap :: HieCacheMap -> m ()

instance (Monad m, SetHieCache m) => SetHieCache (MaybeT m) where
  setHieCacheMap c = lift $ setHieCacheMap c

class (Monad m) => MonadHieFile m where
  getHieCache :: AbsPath -> MaybeT m CachedHieFile

getHieCacheImpl :: (HasHieCache m, SetHieCache m, MonadIde m, MonadIO m) => AbsPath -> MaybeT m CachedHieFile
getHieCacheImpl path = do
  hieCacheMap <- getHieCacheMap
  case HashMap.lookup path hieCacheMap of
    Just hieFile -> do
      MaybeT $ pure $ Just hieFile
    Nothing -> do
      file <- HIE.File.getHieFileFromPath path
      let hieSource = HIE.File.getHieSource file
      let tokens = PositionDiff.lex $ T.unpack hieSource
      let hieFile =
            CachedHieFile
              { hieSource
              , hieSourceRope = Rope.fromText hieSource
              , file = file
              , hieTokenMap = PositionDiff.tokensToRangeMap tokens
              }
      setHieCacheMap $ HashMap.insert path hieFile hieCacheMap
      pure hieFile

instance (MonadHieFile m, Monad m) => MonadHieFile (MaybeT m) where
  getHieCache = lift . getHieCache

getTokenMap :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m (RangeMap PositionDiff.Token)
getTokenMap path = do
  fileState <- getFileStateThrow path
  pure fileState.tokenMap

getHieTokenMap :: (MonadHieFile m) => AbsPath -> MaybeT m (RangeMap PositionDiff.Token)
getHieTokenMap path = do
  hieCache <- getHieCache path
  pure $ hieCache.hieTokenMap

getHieFile :: (MonadHieFile m) => AbsPath -> MaybeT m HIE.File.HieFile
getHieFile path = do
  hieCache <- getHieCache path
  pure $ hieCache.file

getHieSource :: (MonadHieFile m) => AbsPath -> MaybeT m Text
getHieSource path = do
  hieCache <- getHieCache path
  pure $ hieCache.hieSource

getHieSourceRope :: (MonadHieFile m) => AbsPath -> MaybeT m Rope
getHieSourceRope path = do
  hieCache <- getHieCache path
  pure $ hieCache.hieSourceRope

getSourceToHie :: (Monad m, GetDiffCache m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getSourceToHie path = do
  hieCache <- getDiffCache path
  pure $ hieCache.sourceToHie

getHieToSource :: (Monad m, GetDiffCache m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getHieToSource path = do
  hieCache <- getDiffCache path
  pure $ hieCache.hieToSource

data DiffCache = DiffCache
  { hieToSource :: PositionDiff.DiffMap
  , sourceToHie :: PositionDiff.DiffMap
  }

class HasDiffCacheRef m where
  getDiffCacheRef :: m (IORef.IORef (HashMap.HashMap AbsPath DiffCache))

instance (Monad m, HasDiffCacheRef m) => HasDiffCacheRef (MaybeT m) where
  getDiffCacheRef = lift getDiffCacheRef

class GetDiffCache m where
  getDiffCache :: AbsPath -> MaybeT m DiffCache

instance (Monad m, GetDiffCache m) => GetDiffCache (MaybeT m) where
  getDiffCache = lift . getDiffCache

getDiffCacheImpl :: (MonadIde m, HasDiffCacheRef m, MonadIO m) => AbsPath -> MaybeT m DiffCache
getDiffCacheImpl path = do
  diffCacheRef <- getDiffCacheRef
  diffCacheMap <- IORef.readIORef diffCacheRef
  case HashMap.lookup path diffCacheMap of
    Just diffCache -> MaybeT $ pure $ Just diffCache
    Nothing -> do
      hieToSource <- getHieToSrcDiffMap path
      sourceToHie <- getSrcToHieDiffMap path
      let diffCache =
            DiffCache
              { hieToSource
              , sourceToHie
              }
      IORef.writeIORef diffCacheRef $ HashMap.insert path diffCache diffCacheMap
      pure diffCache

onNewSource :: (MonadIde m, HasDiffCacheRef m, MonadIO m) => AbsPath -> Rope.Rope -> m ()
onNewSource path source = do
  Semantic.updateSemantic path source
  removeDiffCache path

removeDiffCache :: (MonadIde m, HasDiffCacheRef m, MonadIO m) => AbsPath -> m ()
removeDiffCache path = do
  diffCacheRef <- getDiffCacheRef
  diffCacheMap <- IORef.readIORef diffCacheRef
  IORef.writeIORef diffCacheRef $ HashMap.delete path diffCacheMap

removePath :: (MonadIde m, HasDiffCacheRef m, MonadIO m) => AbsPath -> m ()
removePath path = do
  Semantic.removePath path
  removeDiffCache path

removeHieFromSourcePath :: (MonadIde m, HasHieCache m, HasDiffCacheRef m, SetHieCache m, MonadIO m) => AbsPath -> m ()
removeHieFromSourcePath path = do
  cache <- getHieCacheMap
  setHieCacheMap $ HashMap.delete path cache
  removeDiffCache path
