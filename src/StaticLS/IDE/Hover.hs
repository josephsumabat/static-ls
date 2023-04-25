module StaticLS.IDE.Hover (
    retrieveHover,
)
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (listToMaybe)
import Data.Text (Text, intercalate)
import qualified GHC.Iface.Ext.Types as GHC
import GHC.Stack (HasCallStack)
import HieDb (pointCommand)
import Language.LSP.Types (
    Hover (..),
    HoverContents (..),
    MarkupContent (..),
    MarkupKind (..),
    Position,
    Range (..),
    TextDocumentIdentifier,
    sectionSeparator,
 )
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.HIE.File.Except
import StaticLS.IDE.Hover.Info
import StaticLS.Maybe
import StaticLS.StaticEnv

-- | Retrive hover information. Incomplete
retrieveHover ::
    (HasCallStack, HasStaticEnv m, MonadIO m) =>
    TextDocumentIdentifier ->
    Position ->
    m (Either HieFileReadException (Maybe Hover))
retrieveHover identifier position = do
    runExceptT $ do
        mHieFile <- runMaybeT $ getHieFileFromTdiE identifier
        lift $ runMaybeT $ do
            hieFile <- toAlt mHieFile
            let info =
                    listToMaybe $
                        pointCommand
                            hieFile
                            (lspPositionToHieDbCoords position)
                            Nothing
                            (hoverInfo (GHC.hie_types hieFile))
            toAlt $ hoverInfoToHover <$> info
  where
    hoverInfoToHover :: (Maybe Range, [Text]) -> Hover
    hoverInfoToHover (mRange, contents) =
        Hover
            { _range = mRange
            , _contents = HoverContents $ MarkupContent MkMarkdown $ intercalate sectionSeparator contents
            }
