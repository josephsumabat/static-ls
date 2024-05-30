module StaticLS.IDE.References (findRefs) where

import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (catMaybes, fromMaybe)
import GHC.Plugins qualified as GHC
import HieDb qualified
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.Except
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.Maybe
import StaticLS.StaticEnv

findRefs :: (HasStaticEnv m, MonadIO m) => LSP.TextDocumentIdentifier -> LSP.Position -> m [LSP.Location]
findRefs tdi position = do
    mLocList <- runMaybeT $ do
        hieFile <- getHieFileFromTdi tdi
        let hiedbPosition = lspPositionToHieDbCoords position
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

refRowToLocation :: (HasStaticEnv m, MonadIO m) => HieDb.Res HieDb.RefRow -> MaybeT m LSP.Location
refRowToLocation (refRow HieDb.:. _) = do
    let start = exceptToMaybe $ hiedbCoordsToLspPosition (refRow.refSLine, refRow.refSCol)
        end = exceptToMaybe $ hiedbCoordsToLspPosition (refRow.refELine, refRow.refECol)
        range = LSP.Range <$> start <*> end
        hieFilePath = refRow.refSrc
    file <- hieFilePathToSrcFilePath hieFilePath
    let lspUri = LSP.fromNormalizedUri . LSP.normalizedFilePathToUri . LSP.toNormalizedFilePath $ file
    toAlt $ LSP.Location lspUri <$> range
