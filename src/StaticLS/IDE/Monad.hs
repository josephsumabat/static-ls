module StaticLS.IDE.Monad
where

import AST.Haskell qualified as Haskell
import AST.Traversal qualified as AST
import Arborist.Files
import Arborist.ProgramIndex
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Cache.LRU.IO.Internal (AtomicLRU, toList)
import Data.Cache.LRU.IO.Internal qualified as AtomicLRU
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
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
import Hir qualified as Hir
import Hir.Types qualified as Hir
import StaticLS.FilePath
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.HieView qualified as HieView
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.Semantic
import StaticLS.Semantic qualified as Semantic
import StaticLS.StaticEnv
import System.Directory (doesFileExist)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as Exception
import Debug.Trace

data FileCacheEntry = FileCacheEntry
  { fileState :: Maybe FileState
  , hieCache :: Maybe CachedHieFile
  , diffCache :: Maybe DiffCache
  }

-- empty 
emptyEntry :: FileCacheEntry
emptyEntry = FileCacheEntry Nothing Nothing Nothing

data IdeEnv = IdeEnv
  {
    modFileMap :: ModFileMap
  , prgIndex :: MVar ProgramIndex
  , cache :: AtomicLRU AbsPath FileCacheEntry
  }

newIdeEnv :: [AbsPath] -> IO IdeEnv
newIdeEnv srcDirs = do
  cache <- AtomicLRU.newAtomicLRU (Just 3)
  prgIndex <- newMVar HashMap.empty
  modFileMap <- buildModuleFileMap (Path.toFilePath <$> srcDirs)
  pure $ IdeEnv {modFileMap, prgIndex, cache}

class HasIdeEnv m where
  getIdeEnv :: m IdeEnv

instance (HasIdeEnv m, Monad m) => HasIdeEnv (MaybeT m) where
  getIdeEnv = lift getIdeEnv

getModFileMap :: (MonadIde m) => m ModFileMap
getModFileMap = do
  ideEnv <- getIdeEnv
  pure ideEnv.modFileMap

getPrgIndex :: (MonadIde m, MonadIO m) => m ProgramIndex
getPrgIndex = do
  ideEnv <- getIdeEnv
  liftIO $ readMVar ideEnv.prgIndex

tryWritePrgIndex :: (MonadIde m, MonadIO m) => (ProgramIndex -> ProgramIndex) -> m ()
tryWritePrgIndex modify = do
  ideEnv <- getIdeEnv
  mCurrPrgIndex <- liftIO $ tryTakeMVar ideEnv.prgIndex
  case mCurrPrgIndex of
    Nothing -> pure ()
    Just old -> liftIO $ putMVar ideEnv.prgIndex (modify old)

invalidatePrgIndexMod :: (MonadIde m, MonadIO m) => Hir.ModuleText -> m ()
invalidatePrgIndexMod mod = do
  ideEnv <- getIdeEnv
  currPrgIndex <- liftIO $ takeMVar ideEnv.prgIndex
  liftIO $ putMVar ideEnv.prgIndex (HashMap.delete mod currPrgIndex)

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
  _ <- liftIO $ AtomicLRU.delete path env.cache
  pure ()

getFileState :: (MonadIde m) => AbsPath -> m FileState
getFileState path = do
  env <- getIdeEnv
  myList <- liftIO $ toList env.cache
  traceShowM $ fmap fst myList
  entry <- liftIO $ AtomicLRU.lookup path env.cache
  case entry >>= (.fileState) of
    Just fs -> pure fs
    Nothing -> do
      res <- getFileStateResult path
      let fs = fromMaybe Semantic.emptyFileState res
      let currentEntry = fromMaybe emptyEntry entry
      liftIO $ AtomicLRU.insert path (currentEntry { fileState = Just fs }) env.cache
      pure fs

getHaskell :: (MonadIde m, MonadIO m) => AbsPath -> m Haskell.HaskellP
getHaskell path = do
  fileState <- getFileState path
  pure fileState.tree

getHir :: (MonadIde m, MonadIO m) => AbsPath -> m Hir.Program
getHir path = do
  fileState <- getFileState path
  pure fileState.hir

getSourceRope :: (MonadIde m, MonadIO m) => AbsPath -> m Rope
getSourceRope path = do
  fileState <- getFileState path
  pure fileState.contentsRope

getSource :: (MonadIde m, MonadIO m) => AbsPath -> m Text
getSource path = do
  fileState <- getFileState path
  pure fileState.contentsText

getHieToSrcDiffMap :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getHieToSrcDiffMap path = do
  hieSource <- getHieSource path
  source <- lift $ getSource path
  pure $ PositionDiff.getDiffMap hieSource source

getSrcToHieDiffMap :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m PositionDiff.DiffMap
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

getHieCacheResult :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m CachedHieFile
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

invalidateStaleHieCacheFile :: (MonadIde m, MonadIO m) => AbsPath -> m ()
invalidateStaleHieCacheFile path = do
  fmap (fromMaybe ()) $ runMaybeT $ do
    env <- getIdeEnv
    latestHieModifiedAt <- getFileModifiedAt path
    entry <- MaybeT $ liftIO $ AtomicLRU.lookup path env.cache
    case entry.hieCache of
      Just hieFile -> do
        when (hieFile.modifiedAt < latestHieModifiedAt) $ do
          liftIO $ AtomicLRU.insert path (entry { hieCache = Nothing, diffCache = Nothing }) env.cache
      Nothing -> pure ()

getHieCache :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m CachedHieFile
getHieCache path = do
  env <- getIdeEnv
  _ <- lift $ invalidateStaleHieCacheFile path
  entry <- liftIO $ AtomicLRU.lookup path env.cache
  case entry >>= (.hieCache) of
    Just hie -> pure hie
    Nothing -> do
      hie <- getHieCacheResult path
      let currentEntry = fromMaybe emptyEntry entry
      liftIO $ AtomicLRU.insert path (currentEntry { hieCache = Just hie }) env.cache
      pure hie

getTokenMap :: (MonadIde m, MonadIO m) => AbsPath -> m (RangeMap PositionDiff.Token)
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

lineColToPos :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m Pos
lineColToPos path lineCol = do
  sourceRope <- getSourceRope path
  pure $ Rope.lineColToPos sourceRope lineCol

data DiffCache = DiffCache
  { hieToSource :: PositionDiff.DiffMap
  , sourceToHie :: PositionDiff.DiffMap
  }
  deriving (Show, Eq)

getDiffCache :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m DiffCache
getDiffCache path = do
  env <- getIdeEnv
  entry <- liftIO $ AtomicLRU.lookup path env.cache
  case entry >>= (.diffCache) of
    Just diff -> pure diff
    Nothing -> do
      diff <- getDiffCacheResult path
      let currentEntry = fromMaybe emptyEntry entry
      liftIO $ AtomicLRU.insert path (currentEntry { diffCache = Just diff }) env.cache
      pure diff

getDiffCacheResult :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m DiffCache
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
  let fileState = Semantic.mkFileState (Rope.toText source) source
  liftIO $ AtomicLRU.insert path (FileCacheEntry (Just fileState) Nothing Nothing) env.cache

removeHieFromSourcePath :: (MonadIde m) => AbsPath -> m ()
removeHieFromSourcePath path = do
  env <- getIdeEnv
  entry <- liftIO $ AtomicLRU.lookup path env.cache
  case entry of
    Just e -> liftIO $ AtomicLRU.insert path (e { hieCache = Nothing, diffCache = Nothing }) env.cache
    Nothing -> pure ()

forceCachedHieFile :: CachedHieFile -> CachedHieFile
forceCachedHieFile hie = hie

getHieCacheWithMap :: (MonadIO m, HasStaticEnv m, HasLogger m) => AbsPath -> HieCacheMap -> MaybeT m CachedHieFile
getHieCacheWithMap path hieCacheMap =
  case HashMap.lookup path hieCacheMap of
    Just hieFile -> do
      MaybeT $ pure $ Just hieFile
    Nothing -> do
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

throwIfInThSplice :: (HasCallStack, MonadIde m, MonadIO m) => String -> AbsPath -> Pos -> m ()
throwIfInThSplice msg path pos = do
  haskell <- getHaskell path
  let range = (Range.point pos)
      splice = AST.getDeepestContaining @Hir.ThSplice range haskell.dynNode
  case splice of
    Nothing -> pure ()
    Just _ -> do
      Exception.throwString $ "Cannot perform action in splice: " ++ msg