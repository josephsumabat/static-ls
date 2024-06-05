module StaticLS.IDE.Hover
  ( retrieveHover,
  )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe
import Data.Text (Text, intercalate)
import Data.Text qualified as T
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
import StaticLS.Logger (HasLogger, logInfo)
import StaticLS.Maybe
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.StaticEnv
import StaticLS.StaticLsEnv

-- | Retrieve hover information.
retrieveHover ::
  ( HasLogger m,
    HasStaticEnv m,
    MonadIO m,
    HasFileEnv m,
    MonadThrow m
  ) =>
  TextDocumentIdentifier ->
  Position ->
  m (Maybe Hover)
retrieveHover identifier position = do
  let uri = identifier._uri
  let lineCol = ProtoLSP.lineColFromProto position
  runMaybeT $ do
    hieFile <- getHieFileFromUri uri
    let hieSource = T.Encoding.decodeUtf8 $ GHC.hie_hs_src hieFile
    lineCol' <- lineColToHieLineCol uri hieSource lineCol
    lift $ logInfo $ T.pack $ "lineCol: " <> show lineCol
    lift $ logInfo $ T.pack $ "lineCol': " <> show lineCol'
    docs <- docsAtPoint hieFile (ProtoLSP.lineColToProto lineCol')
    let info =
          listToMaybe $
            pointCommand
              hieFile
              (lspPositionToHieDbCoords (ProtoLSP.lineColToProto lineCol'))
              Nothing
              (hoverInfo (GHC.hie_types hieFile) docs)
    toAlt $ hoverInfoToHover <$> info
  where
    -- TODO: use the original range in the hover
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
