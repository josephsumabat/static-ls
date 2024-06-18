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
import StaticLS.HIE.File qualified as Hie
import StaticLS.IDE.FileWith
import StaticLS.IDE.Utils
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.Semantic
import StaticLS.StaticEnv

posToHiePos :: (MonadIO m, HasStaticEnv m, HasSemantic m, MonadThrow m) => AbsPath -> Text -> Pos -> MaybeT m Pos
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

hiePosToPos :: (MonadIO m, HasStaticEnv m, HasSemantic m, MonadThrow m) => AbsPath -> Text -> Pos -> MaybeT m Pos
hiePosToPos path hieSource hiePos = do
  source <- lift $ getSource path
  let diff = PositionDiff.diffText hieSource source
  let pos' = PositionDiff.updatePositionUsingDiff diff hiePos
  pure pos'

hieLineColToLineCol :: (MonadIO m, HasStaticEnv m, HasSemantic m, MonadThrow m) => AbsPath -> Text -> LineCol -> MaybeT m LineCol
hieLineColToLineCol path hieSource lineCol = do
  source <- lift $ getSource path
  let pos = Position.lineColToPos hieSource lineCol
  pos' <- hiePosToPos path hieSource pos
  let lineCol' = Position.posToLineCol source pos'
  pure lineCol'

lineColToHieLineCol :: (MonadIO m, HasStaticEnv m, HasSemantic m, MonadThrow m) => AbsPath -> Text -> LineCol -> MaybeT m LineCol
lineColToHieLineCol path hieSource lineCol = do
  source <- lift $ getSource path
  let pos = Position.lineColToPos source lineCol
  pos' <- posToHiePos path hieSource pos
  let lineCol' = Position.posToLineCol hieSource pos'
  pure lineCol'

hieLineColRangeToSrc :: (MonadIO m, HasStaticEnv m, HasSemantic m, MonadThrow m) => AbsPath -> Text -> LineColRange -> MaybeT m LineColRange
hieLineColRangeToSrc path hieSource lineColRange = do
  start <- hieLineColToLineCol path hieSource lineColRange.start
  end <- hieLineColToLineCol path hieSource lineColRange.end
  pure $ LineColRange start end

fileRangeToLc :: (MonadIO m, HasStaticEnv m, HasSemantic m, MonadThrow m) => FileRange -> m FileLcRange
fileRangeToLc FileWith {path, loc} = do
  source <- getSourceRope path
  let range = Rope.rangeToLineColRange source loc
  pure $ FileWith {path, loc = range}

hieFileLcToFileLc :: (MonadIO m, HasStaticEnv m, HasSemantic m, MonadThrow m) => FileLcRange -> MaybeT m FileLcRange
hieFileLcToFileLc fileLineCol = do
  let path = fileLineCol.path
      posStart = fileLineCol.loc.start
      posEnd = fileLineCol.loc.end
  hieFile <- Hie.getHieFileFromPath path
  let hieSource = Hie.getHieSource hieFile
  -- TODO: Pass source in instead of source file path to avoid file read twice
  lcStart <- hieLineColToLineCol path hieSource posStart
  lcEnd <- hieLineColToLineCol path hieSource posEnd
  let lcRange = LineColRange lcStart lcEnd
  pure $ FileWith path lcRange
