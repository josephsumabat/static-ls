module StaticLS.IDE.References (findRefs) where

import Control.Monad (join)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.HashMap.Strict qualified as HashMap
import Data.LineColRange (LineColRange (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Text qualified as T
import Data.Pos (LineCol (..))
import StaticLS.Logger
import Data.Rope qualified as Rope
import Data.Text.Encoding qualified as T.Encoding
import Data.Traversable (for)
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins qualified as GHC
import HieDb qualified
import StaticLS.FileEnv
import StaticLS.HIE
import StaticLS.HIE.File hiding (getHieSource)
import StaticLS.IDE.FileWith (FileLcRange, FileWith (..))
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.StaticEnv
import StaticLS.StaticLsEnv

hieFileLcRangesToSrc :: (MonadThrow m, HasStaticEnv m, HasFileEnv m, MonadIO m) => [FileLcRange] -> m [FileLcRange]
hieFileLcRangesToSrc ranges = do
  let rangesForFile =
        HashMap.fromListWith (++) $
          map (\FileWith {path, loc} -> (path, [loc])) ranges
  infoForFile <- for (HashMap.keys rangesForFile) \path -> runMaybeT do
    source <- getSource path
    sourceRope <- getSourceRope path
    hieSource <- getHieSource path
    let hieSourceRope = Rope.fromText hieSource
    diffMap <- pure $ PositionDiff.getDiffMap hieSource source
    pure (path, (hieSource, hieSourceRope, diffMap, source, sourceRope))
  infoForFile <- pure $ HashMap.fromList $ catMaybes infoForFile
  srcRanges <- for ranges \fr@FileWith {path, loc = hieLineCol} ->
    fromMaybe fr <$> runMaybeT do
      (_hieSource, hieSourceRope, diffMap, _source, sourceRope) <- hoistMaybe $ HashMap.lookup path infoForFile
      let srcLineCol = PositionDiff.diffLineColRange hieSourceRope diffMap sourceRope hieLineCol
      pure FileWith {path, loc = srcLineCol}
  pure srcRanges

findRefs :: (HasLogger m, HasStaticEnv m, MonadThrow m, HasFileEnv m, MonadIO m) => AbsPath -> LineCol -> m [FileLcRange]
findRefs path lineCol = do
  mLocList <- runMaybeT $ do
    hieFile <- getHieFileFromPath path
    let hieSource = T.Encoding.decodeUtf8 $ GHC.hie_hs_src hieFile
    lineCol' <- lineColToHieLineCol path hieSource lineCol
    let hiedbPosition = lspPositionToHieDbCoords (ProtoLSP.lineColToProto lineCol')
        names = namesAtPoint hieFile hiedbPosition
        occNamesAndModNamesAtPoint =
          (\name -> (GHC.occName name, fmap GHC.moduleName . GHC.nameModule_maybe $ name))
            <$> names
    refResRows <-
      lift $ fmap (fromMaybe []) $ runMaybeT $ runHieDbMaybeT $ \hieDb -> do
        join
          <$> mapM
            ( \(occ, mModName) -> do
                HieDb.findReferences hieDb False occ mModName Nothing []
            )
            occNamesAndModNamesAtPoint
    lift $ catMaybes <$> mapM (runMaybeT . refRowToLocation) refResRows
  res <- pure $ fromMaybe [] mLocList
  logInfo $ T.pack $ "res: " <> show res
  newRes <- hieFileLcRangesToSrc res
  logInfo $ T.pack $ "newRes: " <> show newRes
  pure newRes

-- TODO: we converted positions to hie positions to run the references,
-- but we still need to -- convert hie positions to current positions
refRowToLocation :: (HasStaticEnv m, MonadIO m) => HieDb.Res HieDb.RefRow -> MaybeT m FileLcRange
refRowToLocation (refRow HieDb.:. _) = do
  let start = hiedbCoordsToLineCol (refRow.refSLine, refRow.refSCol)
      end = hiedbCoordsToLineCol (refRow.refELine, refRow.refECol)
      range = LineColRange start end
      hieFilePath = refRow.refSrc
  hieFilePath <- Path.filePathToAbs hieFilePath
  file <- hieFilePathToSrcFilePath hieFilePath
  pure $ FileWith file range
