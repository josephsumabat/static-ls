module StaticLS.IDE.Hover where

import Data.Maybe (listToMaybe)
import Data.Text (intercalate)
import Development.IDE.Spans.Common
import Development.IDE.Spans.Documentation
import qualified GHC.Iface.Ext.Types as GHC
import Language.LSP.Types (
    Hover (..),
    HoverContents (..),
    MarkupContent (..),
    MarkupKind (..),
    Position,
    TextDocumentIdentifier,
    sectionSeparator,
 )
import StaticLS.IDE.Hover.Info

import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Driver.Ppr as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Utils.Outputable as GHC
import HieDb (pointCommand)
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.StaticEnv

-- | Retrive hover information. Incomplete
retrieveHover :: (HasStaticEnv m, MonadIO m) => TextDocumentIdentifier -> Position -> m (Maybe Hover)
retrieveHover identifier position = do
    runMaybeT $ do
        hieFile <- MaybeT $ getHieFileFromTdi identifier
        let identifiers =
                join
                    ( pointCommand
                        hieFile
                        (lspPositionToHieDbCoords position)
                        Nothing
                        hieAstNodeToIdentifiers
                    )
        let info =
                listToMaybe $
                    pointCommand
                        hieFile
                        (lspPositionToHieDbCoords position)
                        Nothing
                        (hoverInfo (GHC.hie_types hieFile))
        MaybeT $ pure $ hoverInfoToHover <$> info
  where
    -- hoverInfoToHover :: (Maybe Range,
    hoverInfoToHover (mRange, contents) =
        Hover
            { _range = mRange
            , _contents = HoverContents $ MarkupContent MkMarkdown $ intercalate sectionSeparator contents
            }

showGhc :: (HasStaticEnv m, GHC.Outputable o) => o -> m String
showGhc outputable = do
    staticEnv <- getStaticEnv
    let dynFlags = GHC.extractDynFlags staticEnv.hscEnv
    pure $ GHC.showSDoc dynFlags (GHC.ppr outputable)
