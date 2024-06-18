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
import StaticLS.HI
import StaticLS.HI.File

import StaticLS.HIE.File
import StaticLS.HIE.Position
import StaticLS.HIE.Queries
import StaticLS.IDE.HiePos
import StaticLS.IDE.Hover.Info
import StaticLS.Logger (HasLogger, logInfo)
import StaticLS.Maybe
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.Semantic (HasSemantic)
import StaticLS.StaticEnv
import StaticLS.IDE.Utils

-- | Retrieve hover information.
retrieveHover ::
  forall m.
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m (Maybe Hover)
retrieveHover path lineCol = do
  runMaybeT $ do
    hieFile <- getHieFile path
    lineCol' <- lineColToHieLineCol path lineCol
    lift $ logInfo $ T.pack $ "lineCol: " <> show lineCol
    lift $ logInfo $ T.pack $ "lineCol': " <> show lineCol'
    docs <- docsAtPoint hieFile.file lineCol'
    let mHieInfo =
          listToMaybe $
            pointCommand
              hieFile.file
              (lineColToHieDbCoords lineCol')
              Nothing
              (hoverInfo (GHC.hie_types hieFile.file) docs)
    -- Convert the location from the hie file back to an original src location
    srcInfo <-
      MaybeT $
        maybe
          (pure Nothing)
          ( \(mRange, contents) -> do
              mSrcRange <- runMaybeT $ hieRangeToSrcRange path mRange
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

  hieRangeToSrcRange :: AbsPath -> Maybe LineColRange -> MaybeT m Range
  hieRangeToSrcRange path mLineColRange = do
    lineColRange <- toAlt mLineColRange
    srcStart <- hieLineColToLineCol path lineColRange.start
    srcEnd <- hieLineColToLineCol path lineColRange.end
    pure $ ProtoLSP.lineColRangeToProto (LineColRange srcStart srcEnd)

docsAtPoint :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.HieFile -> LineCol -> m [NameDocs]
docsAtPoint hieFile position = do
  let names = namesAtPoint hieFile (lineColToHieDbCoords position)
      modNames = fmap GHC.moduleName . mapMaybe GHC.nameModule_maybe $ names
  modIfaceFiles <- fromMaybe [] <$> runMaybeT (mapM modToHiFile modNames)
  modIfaces <- catMaybes <$> mapM (runMaybeT . readHiFile . Path.toFilePath) modIfaceFiles
  let docs = getDocsBatch names =<< modIfaces
  pure docs
