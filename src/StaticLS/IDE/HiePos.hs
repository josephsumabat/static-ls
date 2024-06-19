module StaticLS.IDE.HiePos where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.LineColRange
import Data.Path (AbsPath)
import Data.Pos (LineCol, Pos)
import Data.Rope qualified as Rope
import StaticLS.IDE.FileWith
import StaticLS.IDE.Monad
import StaticLS.PositionDiff qualified as PositionDiff

posToHiePos :: (MonadIde m, MonadIO m) => AbsPath -> Pos -> MaybeT m Pos
posToHiePos path pos = do
  sourceToHie <- getSourceToHie path
  pure $ PositionDiff.diffPos pos sourceToHie

hiePosToPos :: (MonadIde m, MonadIO m) => AbsPath -> Pos -> MaybeT m Pos
hiePosToPos path hiePos = do
  source <- lift $ getSource path
  hieSource <- getHieSource path
  let diff = PositionDiff.diffText hieSource source
  let pos' = PositionDiff.updatePositionUsingDiff diff hiePos
  pure pos'

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
