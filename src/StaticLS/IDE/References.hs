module StaticLS.IDE.References where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.Map as Map
import Data.Maybe
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Unit.Types as GHC
import HieDb
import qualified HieDb
import qualified Language.LSP.Types as LSP
import StaticLS.HIE
import StaticLS.Monad
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

findRefs :: HasStaticEnv m => LSP.TextDocumentIdentifier -> LSP.Position -> m [LSP.Location]
findRefs tdi position = do
    staticEnv <- getStaticEnv
    let databasePath = staticEnv.hieDbPath
    mLocList <- runMaybeT $ do
        hieInfo <- MaybeT $ getHieInfo tdi
        let moduleName = GHC.moduleName $ GHC.hie_module $ hieInfo.hieFile
            identifiers =
                join
                    ( HieDb.pointCommand
                        hieInfo.hieFile
                        (lspPositionToHieDbCoords position)
                        Nothing
                        hieAstNodeToIdentifiers
                    )
            names = identifiersToNames identifiers
            occNames = GHC.occName <$> names
        refResRows <-
            lift $ runHieDb $ \hieDb -> do
                join
                    <$> mapM
                        ( \occ -> do
                            HieDb.findReferences hieDb False occ Nothing Nothing []
                        )
                        occNames
        let refRows = (\(mRefrow HieDb.:. _) -> mRefrow) <$> refResRows
        lift $ catMaybes <$> mapM refRowToLocation refRows
    pure $ fromMaybe [] mLocList

refRowToLocation :: HasStaticEnv m => HieDb.RefRow -> m (Maybe LSP.Location)
refRowToLocation refRow = do
    staticEnv <- getStaticEnv
    let start = hiedbCoordsToLspPosition (refRow.refSLine, refRow.refSCol)
        end = hiedbCoordsToLspPosition (refRow.refELine, refRow.refECol)
        range = LSP.Range <$> start <*> end
    hieFilePath <- liftIO (makeAbsolute $ staticEnv.wsRoot </> refRow.refSrc)
    file <- hieFilePathToSrcFilePath hieFilePath
    let lspUri = LSP.filePathToUri <$> file
    pure $ LSP.Location <$> lspUri <*> range
