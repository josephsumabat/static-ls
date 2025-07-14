module StaticLS.IDE.Monad
where

-- (
--   MonadIde,
--   IdeEnv (..),
--   HasIdeEnv (..),
--   getHaskell,
--   getSourceRope,
--   getSource,
--   getHieToSrcDiffMap,
--   getSrcToHieDiffMap,
--   getHieSourceRope,
--   getHieSource,
--   getHieFile,
--   getHieCacheImpl,
--   HieFileInfo (..),
--   MonadHieFile (..),
--   SetHieCache (..),
--   HasHieCache (..),
--   DiffCache (..),
--   HasDiffCacheRef (..),
--   GetDiffCache (..),
--   TouchCachesParallel (..),
--   touchCachesParallelImpl,
--   getDiffCacheImpl,
--   getHieToSource,
--   getSourceToHie,
--   removeDiffCache,
--   removePathImpl,
--   removeHieFromSourcePath,
--   onNewSource,
--   getHieTokenMap,
--   getTokenMap,
--   getHir,
--   getHieView,
--   getThSplice,
--   getFileStateResult,
-- )

import AST.Haskell qualified as Haskell
import AST.Traversal qualified as AST
import Arborist.Files
import Arborist.ProgramIndex
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ConcurrentCache (ConcurrentCache)
import Data.ConcurrentCache qualified as ConcurrentCache
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

data IdeEnv = IdeEnv
  { fileStateCache :: ConcurrentCache AbsPath FileState
  , diffCache :: ConcurrentCache AbsPath (Maybe DiffCache)
  , modFileMap :: ModFileMap
  , prgIndex :: MVar ProgramIndex
  }

newIdeEnv :: [AbsPath] -> IO IdeEnv
newIdeEnv srcDirs = do
  fileStateCache <- ConcurrentCache.new
  diffCache <- ConcurrentCache.new
  modFileMap <- buildModuleFileMap (Path.toFilePath <$> srcDirs)
  prgIndex <- newMVar HashMap.empty
  pure $ IdeEnv {fileStateCache, diffCache, modFileMap, prgIndex}

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
  prevHir <- getHir path
  case prevHir.mod of
    Nothing -> pure ()
    Just m -> invalidatePrgIndexMod m
  ConcurrentCache.remove path env.fileStateCache

-- setFileState :: (Monad m, HasSemantic m, SetSemantic m) => AbsPath -> FileState -> m ()
-- setFileState path fileState = do
--   sema <- getSemantic
--   setSemantic $
--     sema
--       { fileStates = HashMap.insert path fileState sema.fileStates
--       }
--   pure ()

-- updateSemantic :: (Monad m, HasSemantic m, SetSemantic m) => AbsPath -> Rope.Rope -> m ()
-- updateSemantic path contentsRope = do
--   let contentsText = Rope.toText contentsRope
--   let fileState = mkFileState contentsText contentsRope
--   setFileState path fileState

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

getHaskell :: (MonadIde m, MonadIO m) => AbsPath -> m Haskell.HaskellP
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
          let fileState = Semantic.mkFileState (T.pack $ toFilePath path) contents contentsRope
          pure $ Just fileState
    else do
      logInfo $ "File does not exist: " <> T.pack (show path)
      pure Nothing

type HieCacheMap = HashMap.HashMap AbsPath HieFileInfo

-- keep these fields lazy
data HieFileInfo = HieFileInfo
  { hieSource :: Text
  , hieSourceRope :: Rope
  , file :: HIE.File.HieFile
  , fileView :: HieView.File
  , hieTokenMap :: RangeMap PositionDiff.Token
  , modifiedAt :: UTCTime
  }

getHieCacheResult :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m HieFileInfo
getHieCacheResult path = do
  file <- HIE.File.getHieFileFromPath path
  modifiedAt <- getFileModifiedAt path
  let fileView = HieView.viewHieFile file
  let hieSource = fileView.source
  let tokens = PositionDiff.lex $ T.unpack hieSource
  let hieFile =
        HieFileInfo
          { hieSource
          , hieSourceRope = Rope.fromText hieSource
          , file = file
          , fileView
          , hieTokenMap = PositionDiff.tokensToRangeMap tokens
          , modifiedAt = modifiedAt
          }
  pure hieFile

getHieCache :: (MonadIde m, MonadIO m) => AbsPath -> MaybeT m HieFileInfo
getHieCache path = do
  env <- getIdeEnv
  getHieCacheResult path

forceHieFileInfo :: HieFileInfo -> HieFileInfo
forceHieFileInfo
  HieFileInfo
    { hieSource = !hieSource
    , hieSourceRope = !hieSourceRope
    , file = !file
    , fileView = !fileView
    , hieTokenMap = hieTokenMap
    , modifiedAt = !modifiedAt
    } =
    HieFileInfo
      { hieSource
      , hieSourceRope
      , file
      , fileView
      , hieTokenMap
      , modifiedAt
      }

getHieCacheWithMap :: (MonadIO m, HasStaticEnv m, HasLogger m) => AbsPath -> HieCacheMap -> MaybeT m HieFileInfo
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
            HieFileInfo
              { hieSource
              , hieSourceRope = Rope.fromText hieSource
              , file = file
              , fileView
              , hieTokenMap = PositionDiff.tokensToRangeMap tokens
              , modifiedAt = modifiedAt
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
  _ <- ConcurrentCache.insert path (pure (Semantic.mkFileState (T.pack $ toFilePath path) (Rope.toText source) source)) env.fileStateCache
  ConcurrentCache.remove path env.diffCache
  pure ()

removeHieFromSourcePath :: (MonadIde m) => AbsPath -> m ()
removeHieFromSourcePath path = do
  env <- getIdeEnv
  ConcurrentCache.remove path env.diffCache
  pure ()

throwIfInThSplice :: (HasCallStack, MonadIde m, MonadIO m) => String -> AbsPath -> Pos -> m ()
throwIfInThSplice msg path pos = do
  haskell <- getHaskell path
  let range = (Range.point pos)
      splice = AST.getDeepestContaining @Hir.ThSplice range haskell.dynNode
  case splice of
    Nothing -> pure ()
    Just _ -> do
      Exception.throwString $ "Cannot perform action in splice: " ++ msg
