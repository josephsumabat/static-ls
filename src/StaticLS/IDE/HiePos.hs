module StaticLS.IDE.HiePos where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.LineColRange
import Data.Path (AbsPath)
import Data.Pos (LineCol, Pos)
import Data.Pos qualified as Position
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import StaticLS.IDE.FileWith
import StaticLS.IDE.Monad
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.Semantic
import StaticLS.StaticEnv

posToHiePos :: (MonadIde m, MonadIO m) => AbsPath -> Text -> Pos -> MaybeT m Pos
posToHiePos uri hieSource pos = do
  source <- lift $ getSource uri
  let diff = PositionDiff.diffText source hieSource
  let (ts, errs) = PositionDiff.lexWithErrors (T.unpack source)
  let (ts', errs) = PositionDiff.lexWithErrors (T.unpack hieSource)
  -- diff' = (fmap . fmap) PositionDiff.concatTokens $ Diff.diffMerged ts ts'
  -- liftIO $ hPutStrLn stderr $ "diff': " ++ PositionDiff.printDiffSummary diff'
  -- liftIO $ hPutStrLn stderr $ "diff: " ++ PositionDiff.printDiffSummary diff
  -- liftIO $ hPutStrLn stderr $ "ts: " ++ show ts
  -- liftIO $ hPutStrLn stderr $ "ts': " ++ show ts'
  -- liftIO $ hPutStrLn stderr $ "errs: " ++ show errs
  let pos' = PositionDiff.updatePositionUsingDiff diff pos
  pure pos'

hiePosToPos :: (MonadIde m, MonadIO m) => AbsPath -> Pos -> MaybeT m Pos
hiePosToPos path hiePos = do
  source <- lift $ getSource path
  hieSource <- getHieSource path
  let diff = PositionDiff.diffText hieSource source
  let pos' = PositionDiff.updatePositionUsingDiff diff hiePos
  pure pos'

hieLineColToLineCol :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> MaybeT m LineCol
hieLineColToLineCol path lineCol = do
  source <- lift $ getSource path
  hieSource <- getHieSource path
  let pos = Position.lineColToPos hieSource lineCol
  pos' <- hiePosToPos path pos
  let lineCol' = Position.posToLineCol source pos'
  pure lineCol'

lineColToHieLineCol :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> MaybeT m LineCol
lineColToHieLineCol path lineCol = do
  source <- lift $ getSource path
  hieSource <- getHieSource path
  let pos = Position.lineColToPos source lineCol
  pos' <- posToHiePos path hieSource pos
  let lineCol' = Position.posToLineCol hieSource pos'
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
