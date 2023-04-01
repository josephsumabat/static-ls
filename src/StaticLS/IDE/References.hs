module StaticLS.IDE.References where

import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT, runMaybeT)
import qualified Data.Map as Map
import Data.Maybe
import GHC.Data.Maybe (liftMaybeT)
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Unit.Types as GHC
import HieDb
import qualified HieDb
import qualified Language.LSP.Types as LSP
import StaticLS.Except
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.StaticEnv
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

findRefs :: (HasStaticEnv m, MonadIO m) => LSP.TextDocumentIdentifier -> LSP.Position -> m [LSP.Location]
findRefs tdi position = do
    staticEnv <- getStaticEnv
    let databasePath = staticEnv.hieDbPath
    mLocList <- runMaybeT $ do
        hieFile <- exceptToMaybeT $ getHieFileFromTdi tdi
        let moduleName = GHC.moduleName $ GHC.hie_module hieFile
            identifiersAtPoint =
                join
                    ( HieDb.pointCommand
                        hieFile
                        (lspPositionToHieDbCoords position)
                        Nothing
                        hieAstNodeToIdentifiers
                    )
            namesAtPoint = identifiersToNames identifiersAtPoint
            occNamesAndModNamesAtPoint =
                (\name -> (GHC.occName name, fmap GHC.moduleName . GHC.nameModule_maybe $ name))
                    <$> namesAtPoint
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
refRowToLocation (refRow HieDb.:. modInfo) = do
    staticEnv <- getStaticEnv
    let start = exceptToMaybe $ hiedbCoordsToLspPosition (refRow.refSLine, refRow.refSCol)
        end = exceptToMaybe $ hiedbCoordsToLspPosition (refRow.refELine, refRow.refECol)
        range = LSP.Range <$> start <*> end
        hieFilePath = refRow.refSrc
    file <- hieFilePathToSrcFilePath hieFilePath
    let lspUri = LSP.fromNormalizedUri . LSP.normalizedFilePathToUri . LSP.toNormalizedFilePath $ file
    MaybeT . pure $ LSP.Location lspUri <$> range
