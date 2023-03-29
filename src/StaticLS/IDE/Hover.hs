module StaticLS.IDE.Hover -- (retrieveHover)
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (intercalate)
import Development.IDE.Spans.Common
import Development.IDE.Spans.Documentation
import Language.LSP.Types (
    Hover (..),
    HoverContents (..),
    MarkupContent (..),
    MarkupKind (..),
    Position,
    TextDocumentIdentifier,
    sectionSeparator,
 )

import Control.Monad (join)
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
import StaticLS.Monad

-- | Retrive hover information. Incomplete
retrieveHover :: (HasStaticEnv m) => TextDocumentIdentifier -> Position -> m (Maybe Hover)
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
        case identifiers of
            ((Left moduleName) : xs) -> lift $ hoverFromModule moduleName
            ((Right name) : xs) -> MaybeT $ hoverFromName name
            _ -> MaybeT $ pure Nothing

hoverFromModule :: (HasStaticEnv m) => GHC.ModuleName -> m Hover
hoverFromModule modName = do
    output <- showGhc modName
    pure $
        Hover
            { _range = Nothing
            , _contents = HoverContents $ MarkupContent MkMarkdown $ T.pack output
            }

hoverFromName :: (HasStaticEnv m) => GHC.Name -> m (Maybe Hover)
hoverFromName name = do
    staticEnv <- getStaticEnv
    spanDoc <- liftIO $ getDocumentationTryGhc staticEnv.hscEnv name
    output <- showGhc name
    let
        contents = spanDocToMarkdown spanDoc
        hoverInfo =
            Hover
                { _range = Nothing
                , -- , _contents = HoverContents $ MarkupContent MkMarkdown $ intercalate sectionSeparator contents
                  _contents = HoverContents $ MarkupContent MkMarkdown $ T.pack output
                }
    pure $ Just hoverInfo

showGhc :: (HasStaticEnv m, GHC.Outputable o) => o -> m String
showGhc outputable = do
    staticEnv <- getStaticEnv
    let dynFlags = GHC.extractDynFlags staticEnv.hscEnv
    pure $ GHC.showSDoc dynFlags (GHC.ppr outputable)
