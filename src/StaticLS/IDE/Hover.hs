module StaticLS.IDE.Hover
  ( retrieveHover,
  )
where

import Data.Text.Lazy qualified as TL
import Text.Pretty.Simple
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe
import Data.Text (Text, intercalate)
import Data.Text.Encoding qualified as T.Encoding
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins as GHC hiding ((<>))
import HieDb (pointCommand)
import Language.LSP.Protocol.Types
  ( Hover (..),
    MarkupContent (..),
    MarkupKind (..),
    Position,
    Range (..),
    TextDocumentIdentifier (..),
    sectionSeparator,
    type (|?) (..),
  )
import StaticLS.FileEnv
import StaticLS.HI
import StaticLS.HI.File
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.IDE.Hover.Info
import StaticLS.Maybe
import StaticLS.Position qualified as Position
import StaticLS.PositionDiff
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.StaticEnv
import StaticLS.StaticLsEnv
import StaticLS.Utils (isJustOrThrow)
import qualified Data.Text as T
import StaticLS.Logger (logInfo, HasLogger)
import Control.Monad.RWS

-- | Retrieve hover information.
retrieveHover :: (HasCallStack, HasLogger m, HasStaticEnv m, MonadIO m, HasFileEnv m) => TextDocumentIdentifier -> Position -> m (Maybe Hover)
retrieveHover identifier position = do
  let uri = identifier._uri
  source <- getSource uri
  source <- isJustOrThrow "No source found" source
  let lineCol = ProtoLSP.lineColFromProto position
  let pos = Position.lineColToPos source lineCol
  runMaybeT $ do
    hieFile <- getHieFileFromTdi identifier
    let hieSource = T.Encoding.decodeUtf8 $ GHC.hie_hs_src hieFile
    let diff = diffText source hieSource
    let pos' = updatePositionUsingDiff pos diff
    let lineCol' = Position.posToLineCol hieSource pos'
    -- lift $ logInfo $ TL.toStrict $ "diff: " <> pShowNoColor diff
    -- lift $ logInfo $ TL.toStrict $ "lineCol: " <> pShowNoColor lineCol
    -- lift $ logInfo $ TL.toStrict $ "pos: " <> pShowNoColor pos
    -- lift $ logInfo $ TL.toStrict $ "lineCol': " <> pShowNoColor lineCol'
    -- lift $ logInfo $ TL.toStrict $ "pos': " <> pShowNoColor pos'
    lift $ logInfo $ T.pack $ "diff: " <> show diff
    lift $ logInfo $ T.pack $ "lineCol: " <> show lineCol
    lift $ logInfo $ T.pack $ "pos: " <> show pos
    lift $ logInfo $ T.pack $ "lineCol': " <> show lineCol'
    lift $ logInfo $ T.pack $ "pos': " <> show pos'
    docs <- docsAtPoint hieFile (ProtoLSP.lineColToProto lineCol')
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
        { _range = mRange,
          _contents = InL $ MarkupContent MarkupKind_Markdown $ intercalate sectionSeparator contents
        }

docsAtPoint :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.HieFile -> Position -> m [NameDocs]
docsAtPoint hieFile position = do
  let names = namesAtPoint hieFile (lspPositionToHieDbCoords position)
      modNames = fmap GHC.moduleName . mapMaybe GHC.nameModule_maybe $ names
  modIfaceFiles <- fromMaybe [] <$> runMaybeT (mapM modToHiFile modNames)
  modIfaces <- catMaybes <$> mapM (runMaybeT . readHiFile) modIfaceFiles
  let docs = getDocsBatch names =<< modIfaces
  pure docs
