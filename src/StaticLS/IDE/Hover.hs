module StaticLS.IDE.Hover (
  retrieveHover,
)
where

import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.LineCol (LineCol (..))
import Data.LineColRange
import Data.Maybe
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Text (Text, intercalate)
import Data.Text qualified as T
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins as GHC hiding ((<>))
import HieDb (pointCommand)
import Language.LSP.Protocol.Types (
  Hover (..),
  MarkupContent (..),
  MarkupKind (..),
  sectionSeparator,
  type (|?) (..),
 )
import StaticLS.HI
import StaticLS.HI.File

import AST qualified
import AST.Haskell qualified as H
import Control.Monad qualified as Monad
import Data.LineColRange qualified as LineColRange
import Data.Maybe qualified as Maybe
import Data.Pos (Pos)
import Data.Range (Range)
import Data.Range qualified as Range
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.HIE.Position
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.View qualified as HieView
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.HiePos
import StaticLS.IDE.Hover.Info
import StaticLS.IDE.Monad
import StaticLS.Logger (logInfo)
import StaticLS.Maybe
import StaticLS.ProtoLSP qualified as ProtoLSP

-- | Retrieve hover information.
retrieveHover ::
  forall m.
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m (Maybe Hover)
retrieveHover path lineCol = do
  pos <- lineColToPos path lineCol
  throwIfInThSplice "retriveHover" path pos
  runMaybeT $ do
    hieFile <- getHieFile path
    hieView <- getHieView path
    lineCol' <- lineColToHieLineCol path lineCol
    lift $ logInfo $ T.pack $ "lineCol: " <> show lineCol
    lift $ logInfo $ T.pack $ "lineCol': " <> show lineCol'
    pos <- lift $ lineColToPos path lineCol
    hieLineCol <- lineColToHieLineCol path lineCol
    hiePos <- hieLineColToPos path hieLineCol
    valid <- lift $ isHiePosValid path pos hiePos
    _ <- Monad.guard valid
    docs <- lift $ docsAtPoint path hieView pos lineCol'
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
              mSrcRange <- runMaybeT $ hieRangeToSrcRange path mRange
              pure $ Just (mSrcRange, contents)
          )
          mHieInfo
    pure $ hoverInfoToHover srcInfo
 where
  hoverInfoToHover :: (Maybe LSP.Range, [Text]) -> Hover
  hoverInfoToHover (mRange, contents) =
    Hover
      { _range = mRange
      , _contents = InL $ MarkupContent MarkupKind_Markdown $ intercalate sectionSeparator contents
      }

  hieRangeToSrcRange :: AbsPath -> Maybe LineColRange -> MaybeT m LSP.Range
  hieRangeToSrcRange path mLineColRange = do
    lineColRange <- toAlt mLineColRange
    srcStart <- hieLineColToLineCol path lineColRange.start
    srcEnd <- hieLineColToLineCol path lineColRange.end
    pure $ ProtoLSP.lineColRangeToProto (LineColRange srcStart srcEnd)

isInHoverName :: (MonadIde m) => AbsPath -> Range -> m Bool
isInHoverName path range = do
  hs <- getHaskell path
  let node = AST.getDeepestContaining @(H.Module AST.:+ Hir.ParseQualifiedTypes) range hs.dynNode
  pure $ Maybe.isJust node

maxNames :: Int
maxNames = 20

docsAtPoint :: (MonadIde m) => AbsPath -> HieView.File -> Pos -> LineCol -> m [NameDocs]
docsAtPoint path hieView pos position = do
  -- make sure that we are on a name
  -- there are no docs in a subexpression
  -- this way we won't try to get all the possible names in some subexpression which could be huge
  inHover <- isInHoverName path (Range.point pos)
  if inHover
    then do
      let
        -- don't take too many here so we don't hang
        names = take maxNames $ fmap HieView.Name.toGHCName $ HieView.Query.fileNamesAtRangeList (Just (LineColRange.point position)) hieView
        modNames = fmap GHC.moduleName . mapMaybe GHC.nameModule_maybe $ names
      modIfaceFiles <- fromMaybe [] <$> runMaybeT (mapM modToHiFile modNames)
      modIfaces <- catMaybes <$> mapM (runMaybeT . readHiFile . Path.toFilePath) modIfaceFiles
      let docs = getDocsBatch names =<< modIfaces
      pure docs
    else do
      pure []
