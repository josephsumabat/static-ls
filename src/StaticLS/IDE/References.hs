module StaticLS.IDE.References (findRefs) where

import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (catMaybes, fromMaybe)
import qualified GHC.Plugins as GHC
import qualified HieDb
import qualified Language.LSP.Types as LSP
import StaticLS.Except
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.HIE.File.Except
import StaticLS.Maybe
import StaticLS.StaticEnv

findRefs :: (HasStaticEnv m, MonadIO m) => LSP.TextDocumentIdentifier -> LSP.Position -> m (Either HieFileReadException [LSP.Location])
findRefs tdi position = do
    runExceptT $ do
        mHieFile <- runMaybeT $ getHieFileFromTdiE tdi
        mLocList <- runMaybeT $ do
            hieFile <- toAlt mHieFile
            let identifiersAtPoint =
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
refRowToLocation (refRow HieDb.:. _) = do
    let start = exceptToMaybe $ hiedbCoordsToLspPosition (refRow.refSLine, refRow.refSCol)
        end = exceptToMaybe $ hiedbCoordsToLspPosition (refRow.refELine, refRow.refECol)
        range = LSP.Range <$> start <*> end
        hieFilePath = refRow.refSrc
    file <- hieFilePathToSrcFilePath hieFilePath
    let lspUri = LSP.fromNormalizedUri . LSP.normalizedFilePathToUri . LSP.toNormalizedFilePath $ file
    toAlt $ LSP.Location lspUri <$> range
