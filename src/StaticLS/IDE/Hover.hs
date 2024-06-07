module StaticLS.IDE.Hover (
  retrieveHover,
)
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.LineColRange
import Data.Maybe
import Data.Path qualified as Path
import Data.Text (Text, intercalate)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins as GHC hiding ((<>))
import HieDb (pointCommand)
import Language.LSP.Protocol.Types (
  Hover (..),
  MarkupContent (..),
  MarkupKind (..),
  Position,
  Range (..),
  sectionSeparator,
  type (|?) (..),
 )
import Language.LSP.Protocol.Types qualified as LSP
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
import Data.Path (AbsPath)
import Data.Pos (LineCol)

-- | Retrieve hover information.
retrieveHover ::
  forall m.
  ( HasLogger m
  , HasStaticEnv m
  , MonadIO m
  , HasFileEnv m
  , MonadThrow m
  ) =>
  AbsPath ->
  LineCol ->
  m (Maybe Hover)
retrieveHover path lineCol = do
  runMaybeT $ do
    hieFile <- getHieFileFromPath path
    let hieSource = T.Encoding.decodeUtf8 $ GHC.hie_hs_src hieFile
    lineCol' <- lineColToHieLineCol path hieSource lineCol
    lift $ logInfo $ T.pack $ "lineCol: " <> show lineCol
    lift $ logInfo $ T.pack $ "lineCol': " <> show lineCol'
    docs <- docsAtPoint hieFile (ProtoLSP.lineColToProto lineCol')
    let mHieInfo =
          listToMaybe $
            pointCommand
              hieFile
              (lspPositionToHieDbCoords (ProtoLSP.lineColToProto lineCol'))
              Nothing
              (hoverInfo (GHC.hie_types hieFile) docs)
    -- Convert the location from the hie file back to an original src location
    srcInfo <-
      MaybeT $
        maybe
          (pure Nothing)
          ( \(mRange, contents) -> do
              mSrcRange <- runMaybeT $ hieRangeToSrcRange uri hieSource mRange
              pure $ Just (mSrcRange, contents)
          )
          mHieInfo

    pure $ hoverInfoToHover srcInfo
 where
  hoverInfoToHover :: (Maybe Range, [Text]) -> Hover
  hoverInfoToHover (mRange, contents) =
    Hover
      { _range = mRange
      , _contents = InL $ MarkupContent MarkupKind_Markdown $ intercalate sectionSeparator contents
      }

  hieRangeToSrcRange :: LSP.Uri -> Text -> Maybe Range -> MaybeT m Range
  hieRangeToSrcRange uri hieSource mHieRange = do
    hieRange <- toAlt mHieRange
    let lineColRange = ProtoLSP.lineColRangeFromProto hieRange
    srcStart <- hieLineColToLineCol uri hieSource lineColRange.start
    srcEnd <- hieLineColToLineCol uri hieSource lineColRange.end
    pure $ ProtoLSP.lineColRangeToProto (LineColRange srcStart srcEnd)

docsAtPoint :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.HieFile -> Position -> m [NameDocs]
docsAtPoint hieFile position = do
  let names = namesAtPoint hieFile (lspPositionToHieDbCoords position)
      modNames = fmap GHC.moduleName . mapMaybe GHC.nameModule_maybe $ names
  modIfaceFiles <- fromMaybe [] <$> runMaybeT (mapM modToHiFile modNames)
  modIfaces <- catMaybes <$> mapM (runMaybeT . readHiFile . Path.toFilePath) modIfaceFiles
  let docs = getDocsBatch names =<< modIfaces
  pure docs
