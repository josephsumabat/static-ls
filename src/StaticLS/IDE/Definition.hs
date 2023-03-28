module StaticLS.IDE.Definition where

import GHC.Plugins
import qualified HieDb
import qualified Language.LSP.Types as LSP
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.Monad
import System.FilePath ((</>))

-- nameToLocation :: StaticLsM m => Name -> m (Maybe [Location])
-- nameToLocation name =
--  runMaybeT $ do

defRowToLocation :: HasStaticEnv m => HieDb.DefRow -> m (Maybe LSP.Location)
defRowToLocation defRow = do
    staticEnv <- getStaticEnv
    let start = hiedbCoordsToLspPosition (defRow.defSLine, defRow.defSCol)
        end = hiedbCoordsToLspPosition (defRow.defELine, defRow.defECol)
        range = LSP.Range <$> start <*> end
        hieFilePath = defRow.defSrc
    file <- hieFilePathToSrcFilePath hieFilePath
    let lspUri = LSP.filePathToUri <$> file
    pure $ LSP.Location <$> lspUri <*> range
