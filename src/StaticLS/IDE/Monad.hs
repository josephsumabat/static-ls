module StaticLS.IDE.Monad
where

import AST.Haskell qualified as Haskell
import AST.Traversal qualified as AST
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict qualified as HashMap
import Data.LineCol (LineCol)
import Data.Maybe
import Data.Path (AbsPath, toFilePath)
import Data.Path qualified as Path
import Data.Pos (Pos)
import Data.Range qualified as Range
import Data.RangeMap (RangeMap)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import StaticLS.FilePath
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.HieView.View qualified as HieView
import StaticLS.Hir.Types qualified as Hir
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.Semantic
import StaticLS.Semantic qualified as Semantic
import StaticLS.StaticEnv
import System.Directory (doesFileExist)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as Exception
import UnliftIO.MVar (MVar)
import UnliftIO.MVar qualified as MVar

data CurrentFileCache = CurrentFileCache
  { path :: AbsPath
  , fileState :: MVar (Maybe FileState)
  , hieCache :: MVar (Maybe CachedHieFile)
  , diffCache :: MVar (Maybe DiffCache)
  }

data IdeEnv = IdeEnv
  { currentFile :: MVar (Maybe CurrentFileCache)
  }

newIdeEnv :: IO IdeEnv
newIdeEnv = do
  currentFile <- MVar.newMVar Nothing
  pure $ IdeEnv {currentFile}

class HasIdeEnv m where
  getIdeEnv :: m IdeEnv

instance (HasIdeEnv m, Monad m) => HasIdeEnv (MaybeT m) where
  getIdeEnv = lift getIdeEnv

type MonadIde m =
  ( HasIdeEnv m
  , MonadIO m
  , HasStaticEnv m
  , HasLogger m
  , MonadUnliftIO m
  )

removePath :: (MonadIde m) => AbsPath -> m ()
removePath path = do
  env <- getIdeEnv
  maybeCache <- liftIO $ MVar.readMVar env.currentFile
  case maybeCache of
    Just cache | cache.path == path -> do
      liftIO $ MVar.modifyMVar_ env.currentFile (\_ -> pure Nothing)
    _ -> pure ()

-- when switching to a new file, create a cache
switchToFile :: (MonadIde m) => AbsPath -> m ()
switchToFile path = do
  env <- getIdeEnv
  fileStateMVar <- liftIO $ MVar.newMVar Nothing
  hieCacheMVar <- liftIO $ MVar.newMVar Nothing
  diffCacheMVar <- liftIO $ MVar.newMVar Nothing
  
  let newCache = CurrentFileCache
        { path = path
        , fileState = fileStateMVar
        , hieCache = hieCacheMVar
        , diffCache = diffCacheMVar
        }
  
  liftIO $ MVar.modifyMVar_ env.currentFile (\_ -> pure (Just newCache))

-- get the current file cache
getCurrentFileCache :: (MonadIde m) => AbsPath -> MaybeT m CurrentFileCache
getCurrentFileCache path = do
  env <- getIdeEnv
  maybeCache <- liftIO $ MVar.readMVar env.currentFile
  case maybeCache of
    Just cache | cache.path == path -> pure cache
    _ -> MaybeT $ pure Nothing

getFileState :: (MonadIde m) => AbsPath -> m FileState
getFileState path = do
  maybeCache <- runMaybeT $ getCurrentFileCache path
  case maybeCache of
    Nothing -> do
      switchToFile path
      getFileState path
    Just cache -> do
      maybeCached <- liftIO $ MVar.readMVar cache.fileState
      case maybeCached of
        Just fs -> pure fs
        Nothing -> do
          res <- getFileStateResult path
          let fileState = fromMaybe Semantic.emptyFileState res
          liftIO $ MVar.modifyMVar_ cache.fileState (\_ -> pure (Just fileState))
          pure fileState

getHaskell :: (MonadIde m) => AbsPath -> m Haskell.Haskell
getHaskell path = do
  fileState <- getFileState path
  pure fileState.tree

getHir :: (MonadIde m) => AbsPath -> m Hir.Program
getHir path = do
  fileState <- getFileState path
  pure fileState.hir

getSourceRope :: (MonadIde m) => AbsPath -> m Rope
getSourceRope path = do
  fileState <- getFileState path
  pure fileState.contentsRope

getSource :: (MonadIde m) => AbsPath -> m Text
getSource path = do
  fileState <- getFileState path
  pure fileState.contentsText

getHieToSrcDiffMap :: (MonadIde m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getHieToSrcDiffMap path = do
  hieSource <- getHieSource path
  source <- lift $ getSource path
  pure $ PositionDiff.getDiffMap hieSource source

getSrcToHieDiffMap :: (MonadIde m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getSrcToHieDiffMap path = do
  hieSource <- getHieSource path
  source <- lift $ getSource path
  pure $ PositionDiff.getDiffMap source hieSource

getFileStateResult :: (HasLogger m, MonadIO m) => AbsPath -> m (Maybe Semantic.FileState)
getFileStateResult path = do
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
          pure Nothing
        Right contents -> do
          let contentsRope = Rope.fromText contents
          let fileState = Semantic.mkFileState contents contentsRope
          pure $ Just fileState
    else do
      logInfo $ "File does not exist: " <> T.pack (show path)
      pure Nothing

type HieCacheMap = HashMap.HashMap AbsPath CachedHieFile

data CachedHieFile = CachedHieFile
  { hieSource :: Text
  , hieSourceRope :: Rope
  , file :: HIE.File.HieFile
  , fileView :: HieView.File
  , hieTokenMap :: RangeMap PositionDiff.Token
  , modifiedAt :: UTCTime
  }

getHieCacheResult :: (MonadIde m) => AbsPath -> MaybeT m CachedHieFile
getHieCacheResult path = do
  file <- HIE.File.getHieFileFromPath path
  modifiedAt <- getFileModifiedAt path
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
          , modifiedAt = modifiedAt
          }
  pure hieFile

getHieCache :: (MonadIde m) => AbsPath -> MaybeT m CachedHieFile
getHieCache path = do
  cache <- getCurrentFileCache path
  maybeCached <- liftIO $ MVar.readMVar cache.hieCache
  case maybeCached of
    Just hie -> pure hie
    Nothing -> do
      hieFile <- getHieCacheResult path
      liftIO $ MVar.modifyMVar_ cache.hieCache (\_ -> pure (Just hieFile))
      pure hieFile

getTokenMap :: (MonadIde m) => AbsPath -> m (RangeMap PositionDiff.Token)
getTokenMap path = do
  fileState <- getFileState path
  pure fileState.tokenMap

getHieTokenMap :: (MonadIde m) => AbsPath -> MaybeT m (RangeMap PositionDiff.Token)
getHieTokenMap path = do
  hieCache <- getHieCache path
  pure $ hieCache.hieTokenMap

getHieFile :: (MonadIde m) => AbsPath -> MaybeT m HIE.File.HieFile
getHieFile path = do
  hieCache <- getHieCache path
  pure $ hieCache.file

getHieView :: (MonadIde m) => AbsPath -> MaybeT m HieView.File
getHieView path = do
  hieCache <- getHieCache path
  pure $ hieCache.fileView

getHieSource :: (MonadIde m) => AbsPath -> MaybeT m Text
getHieSource path = do
  hieCache <- getHieCache path
  pure $ hieCache.hieSource

getHieSourceRope :: (MonadIde m) => AbsPath -> MaybeT m Rope
getHieSourceRope path = do
  hieCache <- getHieCache path
  pure $ hieCache.hieSourceRope

getSourceToHie :: (MonadIde m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getSourceToHie path = do
  diffCache <- getDiffCache path
  pure $ diffCache.sourceToHie

getHieToSource :: (MonadIde m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getHieToSource path = do
  diffCache <- getDiffCache path
  pure $ diffCache.hieToSource

lineColToPos :: (MonadIde m) => AbsPath -> LineCol -> m Pos
lineColToPos path lineCol = do
  sourceRope <- getSourceRope path
  pure $ Rope.lineColToPos sourceRope lineCol

data DiffCache = DiffCache
  { hieToSource :: PositionDiff.DiffMap
  , sourceToHie :: PositionDiff.DiffMap
  }
  deriving (Show, Eq)

getDiffCache :: (MonadIde m) => AbsPath -> MaybeT m DiffCache
getDiffCache path = do
  cache <- getCurrentFileCache path
  maybeCached <- liftIO $ MVar.readMVar cache.diffCache
  case maybeCached of
    Just diff -> pure diff
    Nothing -> do
      diffCache <- getDiffCacheResult path
      liftIO $ MVar.modifyMVar_ cache.diffCache (\_ -> pure (Just diffCache))
      pure diffCache

getDiffCacheResult :: (MonadIde m) => AbsPath -> MaybeT m DiffCache
getDiffCacheResult path = do
  hieToSource <- getHieToSrcDiffMap path
  sourceToHie <- getSrcToHieDiffMap path
  let diffCache =
        DiffCache
          { hieToSource
          , sourceToHie
          }
  pure diffCache

onNewSource :: (MonadIde m) => AbsPath -> Rope.Rope -> m ()
onNewSource path source = do
  env <- getIdeEnv
  fileStateMVar <- liftIO $ MVar.newMVar Nothing
  hieCacheMVar <- liftIO $ MVar.newMVar Nothing
  diffCacheMVar <- liftIO $ MVar.newMVar Nothing
  
  let newCache = CurrentFileCache
        { path = path
        , fileState = fileStateMVar
        , hieCache = hieCacheMVar
        , diffCache = diffCacheMVar
        }
  
  -- swap in the new cache
  liftIO $ MVar.modifyMVar_ env.currentFile (\_ -> pure (Just newCache))
  let fileState = Semantic.mkFileState (Rope.toText source) source
  liftIO $ MVar.modifyMVar_ fileStateMVar (\_ -> pure (Just fileState))
  pure ()

removeHieFromSourcePath :: (MonadIde m) => AbsPath -> m ()
removeHieFromSourcePath path = do
  maybeCache <- runMaybeT $ getCurrentFileCache path
  case maybeCache of
    Just cache -> do
      liftIO $ MVar.modifyMVar_ cache.hieCache (\_ -> pure Nothing)
      liftIO $ MVar.modifyMVar_ cache.diffCache (\_ -> pure Nothing)
    Nothing -> pure ()

throwIfInThSplice :: (HasCallStack, MonadIde m) => String -> AbsPath -> Pos -> m ()
throwIfInThSplice msg path pos = do
  haskell <- getHaskell path
  let range = (Range.point pos)
      splice = AST.getDeepestContaining @Hir.ThSplice range haskell.dynNode
  case splice of
    Nothing -> pure ()
    Just _ -> do
      Exception.throwString $ "Cannot perform action in splice: " ++ msg