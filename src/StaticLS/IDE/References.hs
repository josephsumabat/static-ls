module StaticLS.IDE.References (
  findRefs,
  findRefsPos,
)
where

import AST qualified
import Control.Monad qualified as Monad
import Control.Monad.Extra (mapMaybeM)
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
import Data.Range qualified as Range
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQL
import HieDb qualified
import StaticLS.HIE.File hiding (getHieSource)
import StaticLS.HIE.Position
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.FileWith (FileLcRange, FileRange, FileWith' (..))
import StaticLS.IDE.HiePos
import StaticLS.IDE.Monad
import StaticLS.Logger
import StaticLS.StaticEnv

findRefsPos :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m [FileRange]
findRefsPos path lineCol = do
  refs <- findRefs path lineCol
  traverse fileLcRangeToRange refs

findRefs :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m [FileLcRange]
findRefs path lineCol = do
  pos <- lineColToPos path lineCol
  throwIfInThSplice "findRefs" path pos
  hs <- getHaskell path
  let qual = Hir.getQualifiedAtPoint (Range.point pos) hs
  nameRes <- runMaybeT do
    hieView <- getHieView path
    hieLineCol <- lineColToHieLineCol path lineCol
    hiePos <- hieLineColToPos path hieLineCol
    valid <- lift $ isHiePosValid path pos hiePos
    Monad.guard valid
    let names = HieView.Query.fileNamesAtRangeList (Just (LineColRange.point hieLineCol)) hieView
    logInfo $ T.pack $ "findRefs: names: " <> show names
    let nameDefRanges = Maybe.mapMaybe HieView.Name.getRange names
    logInfo $ T.pack $ "findRefs: nameDefRanges: " <> show nameDefRanges
    let localDefNames = concatMap (\range -> HieView.Query.fileLocalBindsAtRangeList (Just range) hieView) nameDefRanges
    logInfo $ T.pack $ "findRefs: localDefNames: " <> show localDefNames
    pure (hieView, names, nameDefRanges, localDefNames)
  case nameRes of
    Nothing
      | Right (Just qual) <- qual -> do
          logInfo "fallback logic for refs"
          findRefsString qual.name.node.nodeText
      | otherwise -> do
          pure []
    Just (hieView, names, nameDefRanges, localDefNames) -> do
      mLocList <- runMaybeT $ do
        case localDefNames of
          [] -> do
            refRows <- traverse hieDbFindRefs names
            refRows <- pure $ concat refRows
            lift $ catMaybes <$> mapM (runMaybeT . refRowToLocation) refRows
          _ -> do
            let localRefs = HieView.Query.fileRefsWithDefRanges nameDefRanges hieView
            logInfo $ T.pack $ "findRefs: localRefs: " <> show localRefs
            let localFileRanges = fmap (\loc -> FileWith {path, loc}) localRefs
            pure localFileRanges
      let res = fromMaybe [] mLocList
      newRes <- hieFileLcToFileLcParallel res
      pure newRes

refRowToLocation :: (HasStaticEnv m, HasLogger m, MonadIO m) => HieDb.RefRow -> MaybeT m FileLcRange
refRowToLocation refRow = do
  let start = hiedbCoordsToLineCol (refRow.refSLine, refRow.refSCol)
      end = hiedbCoordsToLineCol (refRow.refELine, refRow.refECol)
      range = LineColRange start end
      hieFilePath = refRow.refSrc
  hieFilePath <- Path.filePathToAbs hieFilePath
  file <- hieFilePathToSrcFilePath hieFilePath
  pure $ FileWith file range

hieDbFindRefs :: (HasStaticEnv m, MonadIO m) => HieView.Name.Name -> MaybeT m [HieDb.RefRow]
hieDbFindRefs name =
  runHieDbMaybeT \hieDb ->
    SQL.queryNamed
      (HieDb.getConn hieDb)
      "SELECT refs.* \
      \FROM refs \
      \WHERE occ = :occ AND (:mod IS NULL OR mod = :mod)"
      [ ":occ" SQL.:= HieView.Name.toGHCOccName name
      , ":mod" SQL.:= HieView.Name.getModule name
      ]

findRefsString :: (MonadIde m, MonadIO m) => T.Text -> m [FileLcRange]
findRefsString ref = do
  res <- runMaybeT do
    res <- runHieDbMaybeT \hieDb ->
      SQL.queryNamed
        (HieDb.getConn hieDb)
        "SELECT refs.* \
        \FROM refs \
        \WHERE occ LIKE :occ"
        [ ":occ" SQL.:= "_:" <> ref
        ]
    mapMaybeM (runMaybeT . refRowToLocation) res
  pure $ Maybe.fromMaybe [] res
