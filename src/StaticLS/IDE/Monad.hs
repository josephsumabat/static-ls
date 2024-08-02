module StaticLS.IDE.Monad where

import AST.Haskell qualified as Haskell
import AST.Traversal qualified as AST
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ConcurrentCache (ConcurrentCache)
import Data.ConcurrentCache qualified as ConcurrentCache
import Data.HashMap.Strict qualified as HashMap
import Data.LineCol (LineCol)
import Data.Path (AbsPath, toFilePath)
import Data.Path qualified as Path
import Data.Range qualified as Range
import Data.RangeMap (RangeMap)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.HieView qualified as HieView
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.Symbol (Symbol (..))
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.Semantic
import StaticLS.Semantic qualified as Semantic
import StaticLS.StaticEnv
import System.Directory (doesFileExist)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as Exception

data IdeEnv = IdeEnv
  { fileStateCache :: ConcurrentCache AbsPath FileState
  , hieCache :: ConcurrentCache AbsPath (Maybe CachedHieFile)
  , diffCache :: ConcurrentCache AbsPath (Maybe DiffCache)
  , workspaceSymbolCache :: ConcurrentCache () [Symbol]
  }

newIdeEnv :: IO IdeEnv
newIdeEnv = do
  fileStateCache <- ConcurrentCache.new
  hieCache <- ConcurrentCache.new
  diffCache <- ConcurrentCache.new
  workspaceSymbolCache <- ConcurrentCache.new
  pure $
    IdeEnv
      { fileStateCache
      , hieCache
      , diffCache
      , workspaceSymbolCache
      }

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
  , MonadThrow m
  )

removePath :: (MonadIde m) => AbsPath -> m ()
removePath path = do
  env <- getIdeEnv
  ConcurrentCache.remove path env.fileStateCache

getFileState :: (MonadIde m) => AbsPath -> m FileState
getFileState path = do
  env <- getIdeEnv
  ConcurrentCache.insert
    path
    ( do
        res <- getFileStateResult path
        case res of
          Just fileState -> pure fileState
          Nothing -> pure Semantic.emptyFileState
    )
    env.fileStateCache

getHaskell :: (MonadIde m, MonadIO m) => AbsPath -> m Haskell.Haskell
getHaskell path = do
  fileState <- getFileState path
  pure fileState.tree

getHir :: (MonadIde m, MonadIO m) => AbsPath -> m Hir.Program
getHir hir = do
  fileState <- getFileState hir
  pure fileState.hir

getSourceRope :: (MonadIde m, MonadIO m) => AbsPath -> m Rope
getSourceRope path = do
  mFileState <- getFileState path
  pure mFileState.contentsRope

getSource :: (MonadIde m, MonadIO m) => AbsPath -> m Text
getSource path = do
  mFileState <- getFileState path
  pure mFileState.contentsText

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
          pure Nothing
        Right contents -> do
          let contentsRope = Rope.fromText contents
          let fileState = Semantic.mkFileState contents contentsRope
          pure $ Just fileState
    else do
      logInfo $ "File does not exist: " <> T.pack (show path)
      pure Nothing

type HieCacheMap = HashMap.HashMap AbsPath CachedHieFile

-- keep these fields lazy
data CachedHieFile = CachedHieFile
  { hieSource :: Text
  , hieSourceRope :: Rope
  , file :: HIE.File.HieFile
  , fileView :: HieView.File
  , hieTokenMap :: RangeMap PositionDiff.Token
  }

getHieCacheResult :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m CachedHieFile
getHieCacheResult path = do
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

getHieCache :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m CachedHieFile
getHieCache path = do
  env <- getIdeEnv
  MaybeT $
    ConcurrentCache.insert
      path
      (runMaybeT $ getHieCacheResult path)
      env.hieCache

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
  hieCache <- getDiffCache path
  pure $ hieCache.sourceToHie

getHieToSource :: (MonadIde m) => AbsPath -> MaybeT m PositionDiff.DiffMap
getHieToSource path = do
  hieCache <- getDiffCache path
  pure $ hieCache.hieToSource

data DiffCache = DiffCache
  { hieToSource :: PositionDiff.DiffMap
  , sourceToHie :: PositionDiff.DiffMap
  }
  deriving (Show, Eq)

getDiffCache :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m DiffCache
getDiffCache path = do
  env <- getIdeEnv
  MaybeT $
    ConcurrentCache.insert
      path
      (runMaybeT $ getDiffCacheResult path)
      env.diffCache

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

-- this function is not thread safe
onNewSource :: (MonadIde m) => AbsPath -> Rope.Rope -> m ()
onNewSource path source = do
  env <- getIdeEnv
  ConcurrentCache.remove path env.fileStateCache
  _ <- ConcurrentCache.insert path (pure (Semantic.mkFileState (Rope.toText source) source)) env.fileStateCache
  ConcurrentCache.remove path env.diffCache
  pure ()

removeHieFromSourcePath :: (MonadIde m) => AbsPath -> m ()
removeHieFromSourcePath path = do
  env <- getIdeEnv
  ConcurrentCache.remove path env.hieCache
  ConcurrentCache.remove path env.diffCache
  ConcurrentCache.remove () env.workspaceSymbolCache
  pure ()

getThSplice :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m (Maybe Hir.ThSplice)
getThSplice path lineCol = do
  srcRope <- getSourceRope path
  haskell <- getHaskell path
  let pos = Rope.lineColToPos srcRope lineCol
      range = (Range.point pos)
      splice = AST.getDeepestContaining @Hir.ThSplice range haskell.dynNode
  pure splice
