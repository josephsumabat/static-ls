module StaticLS.IDE.Hover (
    retrieveHover,
)
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe
import Data.Text (Text, intercalate)
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins as GHC
import HieDb (pointCommand)
import Language.LSP.Protocol.Types (
    Hover (..),
    MarkupContent (..),
    MarkupKind (..),
    Position,
    Range (..),
    TextDocumentIdentifier,
    sectionSeparator,
    type (|?) (..),
 )
import StaticLS.HI
import StaticLS.HI.File
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.IDE.Hover.Info
import StaticLS.Maybe
import StaticLS.StaticEnv

-- | Retrieve hover information.
retrieveHover :: (HasCallStack, HasStaticEnv m, MonadIO m) => TextDocumentIdentifier -> Position -> m (Maybe Hover)
retrieveHover identifier position = do
    runMaybeT $ do
        hieFile <- getHieFileFromTdi identifier
        docs <- docsAtPoint hieFile position
        let info =
                listToMaybe $
                    pointCommand
                        hieFile
                        (lspPositionToHieDbCoords position)
                        Nothing
                        (hoverInfo (GHC.hie_types hieFile) docs)
        toAlt $ hoverInfoToHover <$> info
  where
    hoverInfoToHover :: (Maybe Range, [Text]) -> Hover
    hoverInfoToHover (mRange, contents) =
        Hover
            { _range = mRange
            , _contents = InL $ MarkupContent MarkupKind_Markdown $ intercalate sectionSeparator contents
            }

docsAtPoint :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.HieFile -> Position -> m [NameDocs]
docsAtPoint hieFile position = do
    let names = namesAtPoint hieFile (lspPositionToHieDbCoords position)
        modNames = fmap GHC.moduleName . mapMaybe GHC.nameModule_maybe $ names
    modIfaceFiles <- fromMaybe [] <$> runMaybeT (mapM modToHiFile modNames)
    modIfaces <- catMaybes <$> mapM (runMaybeT . readHiFile) modIfaceFiles
    let docs = getDocsBatch names =<< modIfaces
    pure docs
