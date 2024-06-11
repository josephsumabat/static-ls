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
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (LineCol)
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
  Range (..),
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
    docs <- docsAtPoint hieFile lineCol'
    let mHieInfo =
          listToMaybe $
            pointCommand
              hieFile
              (lineColToHieDbCoords lineCol')
              Nothing
              (hoverInfo (GHC.hie_types hieFile) docs)
    -- Convert the location from the hie file back to an original src location
    srcInfo <-
      MaybeT $
        maybe
          (pure Nothing)
          ( \(mRange, contents) -> do
              mSrcRange <- runMaybeT $ hieRangeToSrcRange path hieSource mRange
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

  hieRangeToSrcRange :: AbsPath -> Text -> Maybe LineColRange -> MaybeT m Range
  hieRangeToSrcRange path hieSource mLineColRange = do
    lineColRange <- toAlt mLineColRange
    srcStart <- hieLineColToLineCol path hieSource lineColRange.start
    srcEnd <- hieLineColToLineCol path hieSource lineColRange.end
    pure $ ProtoLSP.lineColRangeToProto (LineColRange srcStart srcEnd)

docsAtPoint :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.HieFile -> LineCol -> m [NameDocs]
docsAtPoint hieFile position = do
  let names = namesAtPoint hieFile (lineColToHieDbCoords position)
      modNames = fmap GHC.moduleName . mapMaybe GHC.nameModule_maybe $ names
  modIfaceFiles <- fromMaybe [] <$> runMaybeT (mapM modToHiFile modNames)
  modIfaces <- catMaybes <$> mapM (runMaybeT . readHiFile . Path.toFilePath) modIfaceFiles
  let docs = getDocsBatch names =<< modIfaces
  pure docs
