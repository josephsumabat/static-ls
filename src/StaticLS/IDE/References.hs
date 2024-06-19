module StaticLS.IDE.References (findRefs) where

import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.LineColRange (LineColRange (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (LineCol (..))
import Data.Text qualified as T
import Data.Traversable (for)
import GHC.Plugins qualified as GHC
import HieDb qualified
import StaticLS.HIE.File hiding (getHieSource)
import StaticLS.HIE.Position
import StaticLS.HIE.Queries
import StaticLS.IDE.FileWith (FileLcRange, FileWith (..))
import StaticLS.IDE.HiePos
import StaticLS.IDE.Monad
import StaticLS.Logger
import StaticLS.StaticEnv

findRefs :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m [FileLcRange]
findRefs path lineCol = do
  mLocList <- runMaybeT $ do
    hieFile <- getHieFile path
    lineCol' <- lineColToHieLineCol path lineCol
    let hiedbPosition = lineColToHieDbCoords lineCol'
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
  let res = fromMaybe [] mLocList
  logInfo $ T.pack $ "res: " <> show res
  newRes <- for res \fileLcRange -> do
    new <- runMaybeT $ hieFileLcToFileLc fileLcRange
    pure $ fromMaybe fileLcRange new
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
