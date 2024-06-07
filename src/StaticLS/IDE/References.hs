module StaticLS.IDE.References (findRefs) where

import StaticLS.StaticLsEnv

import Control.Monad (join)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT, runMaybeT)
import Data.LineColRange (LineColRange (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (LineCol (..))
import Data.Text.Encoding qualified as T.Encoding
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins qualified as GHC
import HieDb qualified
import StaticLS.FileEnv
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.IDE.FileWith (FileLcRange, FileWith (..))
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.StaticEnv

findRefs :: (HasStaticEnv m, MonadThrow m, HasFileEnv m, MonadIO m) => AbsPath -> LineCol -> m [FileLcRange]
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
  pure $ fromMaybe [] mLocList

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
