module StaticLS.IDE.References (
  findRefs,
  findRefsPos,
)
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.LineColRange qualified as LineColRange
import Data.Maybe (catMaybes, fromMaybe)
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Traversable (for)
import HieDb qualified
import StaticLS.HIE.File hiding (getHieSource)
import StaticLS.HIE.Position
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.View qualified as HieView
import StaticLS.IDE.FileWith (FileLcRange, FileRange, FileWith' (..))
import StaticLS.IDE.HiePos
import StaticLS.IDE.Monad
import StaticLS.StaticEnv

findRefsPos :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m [FileRange]
findRefsPos path lineCol = do
  refs <- findRefs path lineCol
  traverse fileLcRangeToRange refs

findRefs :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m [FileLcRange]
findRefs path lineCol = do
  mLocList <- runMaybeT $ do
    hieView <- getHieView path
    lineCol' <- lineColToHieLineCol path lineCol
    let names = HieView.Query.fileNamesAtRangeList (Just (LineColRange.empty lineCol')) hieView
    let nameDefRanges = Maybe.mapMaybe HieView.Name.getRange names
    let localDefNames = concatMap (\range -> HieView.Query.fileLocalBindsAtRangeList (Just range) hieView) nameDefRanges
    case localDefNames of
      [] -> do
        refRows <- traverse hieDbFindReferences names
        refRows <- pure $ concat refRows
        lift $ catMaybes <$> mapM (runMaybeT . refRowToLocation) refRows
      _ -> do
        let localRefs = HieView.Query.fileNamesWithDefRange nameDefRanges hieView
        let localRanges = Maybe.mapMaybe HieView.Name.getRange localRefs
        let localFileRanges = fmap (\loc -> FileWith {path, loc}) localRanges
        pure localFileRanges
  let res = fromMaybe [] mLocList
  newRes <- for res \fileLcRange -> do
    new <- runMaybeT $ hieFileLcToFileLc fileLcRange
    pure $ fromMaybe fileLcRange new
  pure newRes

hieDbFindReferences :: (HasStaticEnv m, MonadIO m) => HieView.Name -> MaybeT m [HieDb.Res HieDb.RefRow]
hieDbFindReferences name =
  runHieDbMaybeT \hieDb ->
    HieDb.findReferences
      hieDb
      False
      (HieView.Name.toGHCOccName name)
      (fmap HieView.Name.toGHCModuleName (HieView.Name.getModuleName name))
      Nothing
      []

refRowToLocation :: (HasStaticEnv m, MonadIO m) => HieDb.Res HieDb.RefRow -> MaybeT m FileLcRange
refRowToLocation (refRow HieDb.:. _) = do
  let start = hiedbCoordsToLineCol (refRow.refSLine, refRow.refSCol)
      end = hiedbCoordsToLineCol (refRow.refELine, refRow.refECol)
      range = LineColRange start end
      hieFilePath = refRow.refSrc
  hieFilePath <- Path.filePathToAbs hieFilePath
  file <- hieFilePathToSrcFilePath hieFilePath
  pure $ FileWith file range
