module StaticLS.IDE.HiePos where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.LineCol (LineCol (..))
import Data.LineColRange
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Pos (Pos (..))
import Data.RangeMap qualified as RangeMap
import Data.Rope qualified as Rope
import Data.Text qualified as T
import Data.Traversable (for)
import StaticLS.IDE.FileWith
import StaticLS.IDE.Monad
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff

posToHiePos :: (MonadIde m, MonadIO m) => AbsPath -> Pos -> MaybeT m Pos
posToHiePos path pos = do
  sourceToHie <- getSourceToHie path
  pure $ PositionDiff.diffPos pos sourceToHie

hiePosToPos :: (MonadIde m, MonadIO m) => AbsPath -> Pos -> MaybeT m Pos
hiePosToPos path hiePos = do
  hieToSource <- getHieToSource path
  pure $ PositionDiff.diffPos hiePos hieToSource

lineColToPos :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m Pos
lineColToPos path lineCol = do
  sourceRope <- getSourceRope path
  pure $ Rope.lineColToPos sourceRope lineCol

hieLineColToPos :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> MaybeT m Pos
hieLineColToPos path lineCol = do
  hieSourceRope <- getHieSourceRope path
  let pos = Rope.lineColToPos hieSourceRope lineCol
  pure pos

hieLineColToLineCol :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> MaybeT m LineCol
hieLineColToLineCol path lineCol = do
  sourceRope <- lift $ getSourceRope path
  hieSourceRope <- getHieSourceRope path
  let pos = Rope.lineColToPos hieSourceRope lineCol
  pos' <- hiePosToPos path pos
  let lineCol' = Rope.posToLineCol sourceRope pos'
  pure lineCol'

lineColToHieLineCol :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> MaybeT m LineCol
lineColToHieLineCol path lineCol = do
  sourceRope <- lift $ getSourceRope path
  hieSourceRope <- getHieSourceRope path
  let pos = Rope.lineColToPos sourceRope lineCol
  pos' <- posToHiePos path pos
  let lineCol' = Rope.posToLineCol hieSourceRope pos'
  pure lineCol'

isHiePosValid :: (MonadIde m, MonadIO m) => AbsPath -> Pos -> Pos -> m Bool
isHiePosValid path pos hiePos = do
  res <- runMaybeT do
    hieTokenMap <- getHieTokenMap path
    tokenMap <- getTokenMap path
    pure $ RangeMap.lookup hiePos hieTokenMap == RangeMap.lookup pos tokenMap
  pure $ Maybe.fromMaybe False res

hieLineColRangeToSrc :: (MonadIde m, MonadIO m) => AbsPath -> LineColRange -> MaybeT m LineColRange
hieLineColRangeToSrc path lineColRange = do
  start <- hieLineColToLineCol path lineColRange.start
  end <- hieLineColToLineCol path lineColRange.end
  pure $ LineColRange start end

fileRangeToLc :: (MonadIde m, MonadIO m) => FileRange -> m FileLcRange
fileRangeToLc FileWith {path, loc} = do
  source <- getSourceRope path
  let range = Rope.rangeToLineColRange source loc
  pure $ FileWith {path, loc = range}

fileLcRangeToRange :: (MonadIde m, MonadIO m) => FileLcRange -> m FileRange
fileLcRangeToRange FileWith {path, loc} = do
  source <- getSourceRope path
  let range = Rope.lineColRangeToRange source loc
  pure $ FileWith {path, loc = range}

hieFileLcToFileLc :: (MonadIde m, MonadIO m) => FileLcRange -> MaybeT m FileLcRange
hieFileLcToFileLc fileLineCol = do
  let path = fileLineCol.path
      posStart = fileLineCol.loc.start
      posEnd = fileLineCol.loc.end
  -- TODO: Pass source in instead of source file path to avoid file read twice
  lcStart <- hieLineColToLineCol path posStart
  lcEnd <- hieLineColToLineCol path posEnd
  let lcRange = LineColRange lcStart lcEnd
  pure $ FileWith path lcRange

hieFileLcToFileLcParallel :: (MonadIde m, MonadIO m) => [FileLcRange] -> m [FileLcRange]
hieFileLcToFileLcParallel fileLcs = do
  let len = (length @[] fileLcs)
  if len > 3000
    then do
      logInfo "too many conversions, skipping"
      pure fileLcs
    else do
      logInfo $ T.pack $ "number of conversions: " ++ show len
      logInfo "touching caches"
      touchCachesParallel $ (.path) <$> fileLcs
      logInfo "finished touching caches"
      logInfo "converting positions"
      newRes <- for fileLcs \fileLcRange -> do
        new <- runMaybeT $ hieFileLcToFileLc fileLcRange
        pure $ Maybe.fromMaybe fileLcRange new
      logInfo "finished converting positions"
      pure newRes
