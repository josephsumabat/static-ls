module StaticLS.IDE.Monad (
  MonadIde,
  getHaskell,
  getSourceRope,
  getSource,
  getHieToSrcDiffMap,
  getSrcToHieDiffMap,
  getFileState,
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
  TouchCachesParallel (..),
  touchCachesParallelImpl,
  getDiffCacheImpl,
  getHieToSource,
  getSourceToHie,
  removeDiffCache,
  RemovePath (..),
  removePathImpl,
  removeHieFromSourcePath,
  onNewSource,
  getHieTokenMap,
  getTokenMap,
  getHir,
  getHieView,
)
where

import AST.Haskell qualified as Haskell
import Control.Monad ((<$!>))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath, toFilePath)
import Data.Path qualified as Path
import Data.RangeMap (RangeMap)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.HieView qualified as HieView
import StaticLS.Hir qualified as Hir
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.Semantic qualified as Semantic
import StaticLS.StaticEnv
import System.Directory (doesFileExist)
import UnliftIO (MonadUnliftIO, pooledForConcurrently)
import UnliftIO.Async (pooledMapConcurrently)
import UnliftIO.Exception qualified as Exception
import UnliftIO.IORef qualified as IORef

type MonadIde m =
  ( MonadThrow m
  , MonadHieFile m
  , HasStaticEnv m
  , Semantic.HasSemantic m
  , Semantic.SetSemantic m
  , HasLogger m
  , GetDiffCache m
  , RemovePath m
  , TouchCachesParallel m
  )

getHaskell :: (MonadIde m, MonadIO m) => AbsPath -> m Haskell.Haskell
getHaskell uri = do
  fileState <- getFileState uri
  pure fileState.tree

getHir :: (MonadIde m, MonadIO m) => AbsPath -> m Hir.Program
getHir hir = do
  fileState <- getFileState hir
  pure fileState.hir

getSourceRope :: (MonadIde m, MonadIO m) => AbsPath -> m Rope
getSourceRope uri = do
  mFileState <- getFileState uri
  pure mFileState.contentsRope

getSource :: (MonadIde m, MonadIO m) => AbsPath -> m Text
getSource uri = do
  mFileState <- getFileState uri
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

-- While hie operations return MaybeT, because we expect those operations to fail and we want to recover from them
-- However, if we fail to get the file state, we're probably screwed anyway
-- So we catch the exception and return an empty file state, so the exception doesn't ruin other stuff
-- For example, sometimes the hiefiles are stale and refer to some file that doesn't exist anymore
-- We don't want one non existing file to ruin an entire goto references for example
getFileState :: (MonadIde m, MonadIO m) => AbsPath -> m Semantic.FileState
getFileState path = do
  sema <- Semantic.getSemantic
  let fileState = HashMap.lookup path sema.fileStates
  case fileState of
    Just fileState -> pure fileState
    Nothing -> do
      -- use the double liftIO here to avoid the MonadUnliftIO constraint
      -- we really just want unliftio to handle not catching async exceptions
      doesExist <- liftIO $ doesFileExist (Path.toFilePath path)
      if doesExist
        then do
          contents <-
            liftIO $
              Exception.tryAny
                (liftIO $ T.readFile $ toFilePath path)
          case contents of
            Left e -> do
              logError $ "Failed to read file: " <> T.pack (show e)
              removePath path
              pure Semantic.emptyFileState
            Right contents -> do
              let contentsRope = Rope.fromText contents
              let fileState = Semantic.mkFileState contents contentsRope
              Semantic.setFileState path fileState
              pure fileState
        else do
          logInfo $ "File does not exist: " <> T.pack (show path)
          pure Semantic.emptyFileState

type HieCacheMap = HashMap.HashMap AbsPath CachedHieFile

-- keep these fields lazy
data CachedHieFile = CachedHieFile
  { hieSource :: Text
  , hieSourceRope :: Rope
  , file :: HIE.File.HieFile
  , fileView :: HieView.File
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

class (Monad m) => TouchCachesParallel m where
  touchCachesParallel :: [AbsPath] -> m ()

instance (Monad m, TouchCachesParallel m) => TouchCachesParallel (MaybeT m) where
  touchCachesParallel paths = lift $ touchCachesParallel paths

getHieCacheImpl :: (HasHieCache m, SetHieCache m, MonadIde m, MonadIO m) => AbsPath -> MaybeT m CachedHieFile
getHieCacheImpl path = do
  hieCacheMap <- getHieCacheMap
  case HashMap.lookup path hieCacheMap of
    Just hieFile -> do
      MaybeT $ pure $ Just hieFile
    Nothing -> do
      file <- HIE.File.getHieFileFromPath path
      let fileView = HieView.viewHieFile file
      let hieSource = fileView.source
      let tokens = PositionDiff.lex $ T.unpack hieSource
      let hieFile =
            CachedHieFile
              { hieSource
              , hieSourceRope = Rope.fromText hieSource
              , file = file
              , fileView
              , hieTokenMap = PositionDiff.tokensToRangeMap tokens
              }
      setHieCacheMap $ HashMap.insert path hieFile hieCacheMap
      pure hieFile

forceCachedHieFile :: CachedHieFile -> CachedHieFile
forceCachedHieFile
  CachedHieFile
    { hieSource = !hieSource
    , hieSourceRope = !hieSourceRope
    , file = !file
    , fileView = !fileView
    , hieTokenMap = hieTokenMap
    } =
    CachedHieFile
      { hieSource
      , hieSourceRope
      , file
      , fileView
      , hieTokenMap
      }

getHieCacheWithMap :: (MonadIO m, HasStaticEnv m) => AbsPath -> HieCacheMap -> MaybeT m CachedHieFile
getHieCacheWithMap path hieCacheMap =
  case HashMap.lookup path hieCacheMap of
    Just hieFile -> do
      MaybeT $ pure $ Just hieFile
    Nothing -> do
      file <- HIE.File.getHieFileFromPath path
      let fileView = HieView.viewHieFile file
      let hieSource = fileView.source
      let tokens = PositionDiff.lex $ T.unpack hieSource
      let hieFile =
            CachedHieFile
              { hieSource
              , hieSourceRope = Rope.fromText hieSource
              , file = file
              , fileView
              , hieTokenMap = PositionDiff.tokensToRangeMap tokens
              }
      pure hieFile

instance (MonadHieFile m, Monad m) => MonadHieFile (MaybeT m) where
  getHieCache = lift . getHieCache

getTokenMap :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m (RangeMap PositionDiff.Token)
getTokenMap path = do
  fileState <- getFileState path
  pure fileState.tokenMap

getHieTokenMap :: (MonadHieFile m) => AbsPath -> MaybeT m (RangeMap PositionDiff.Token)
getHieTokenMap path = do
  hieCache <- getHieCache path
  pure $ hieCache.hieTokenMap

getHieFile :: (MonadHieFile m) => AbsPath -> MaybeT m HIE.File.HieFile
getHieFile path = do
  hieCache <- getHieCache path
  pure $ hieCache.file

getHieView :: (MonadHieFile m) => AbsPath -> MaybeT m HieView.File
getHieView path = do
  hieCache <- getHieCache path
  pure $ hieCache.fileView

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
  deriving (Show, Eq)

class HasDiffCacheRef m where
  getDiffCacheRef :: m (IORef.IORef (HashMap.HashMap AbsPath DiffCache))

instance (Monad m, HasDiffCacheRef m) => HasDiffCacheRef (MaybeT m) where
  getDiffCacheRef = lift getDiffCacheRef

class GetDiffCache m where
  getDiffCache :: AbsPath -> MaybeT m DiffCache

instance (Monad m, GetDiffCache m) => GetDiffCache (MaybeT m) where
  getDiffCache = lift . getDiffCache

class RemovePath m where
  removePath :: AbsPath -> m ()

instance (Monad m, RemovePath m) => RemovePath (MaybeT m) where
  removePath = lift . removePath

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

removePathImpl :: (MonadIde m, HasDiffCacheRef m, MonadIO m) => AbsPath -> m ()
removePathImpl path = do
  Semantic.removePath path
  removeDiffCache path

removeHieFromSourcePath :: (MonadIde m, HasHieCache m, HasDiffCacheRef m, SetHieCache m, MonadIO m) => AbsPath -> m ()
removeHieFromSourcePath path = do
  cache <- getHieCacheMap
  setHieCacheMap $ HashMap.delete path cache
  removeDiffCache path

touchCachesParallelImpl ::
  ( MonadIO m
  , HasStaticEnv m
  , HasHieCache m
  , SetHieCache m
  , MonadUnliftIO m
  , HasDiffCacheRef m
  ) =>
  [AbsPath] ->
  m ()
touchCachesParallelImpl paths = do
  map <- getHieCacheMap
  res <- pooledForConcurrently paths \path -> runMaybeT do
    _ <- MaybeT $ case HashMap.lookup path map of
      Just _ -> pure Nothing
      Nothing -> pure $ Just ()
    hieCache <- getHieCacheWithMap path map
    !hieCache <- pure $! forceCachedHieFile hieCache
    pure $ (path, hieCache)
  res <- pure $ Maybe.catMaybes res
  let map' = HashMap.union map (HashMap.fromList res)
  setHieCacheMap map'
  -- diffCacheRef <- getDiffCacheRef
  -- diffCache <- IORef.readIORef diffCacheRef
  pure ()
