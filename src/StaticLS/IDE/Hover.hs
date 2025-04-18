module StaticLS.IDE.Hover (
  retrieveHover,
  time,
)
where

import TreeSitter.Api
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
import AST.Haskell.Generated qualified as H
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
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import StaticLS.IDE.HiePos
import StaticLS.IDE.Hover.Info
import StaticLS.IDE.Monad
import StaticLS.Logger (logInfo)
import StaticLS.Maybe
import StaticLS.ProtoLSP qualified as ProtoLSP
import Arborist.Renamer
import Arborist.ModGraph
import Arborist.Scope.Types
import StaticLS.StaticEnv
import Control.Applicative
import Data.Time
import Debug.Trace

time :: MonadIO m => [Char] -> m a -> m a
time label fn = do
  start <- liftIO getCurrentTime
  res <- fn
  end <- liftIO getCurrentTime
  liftIO $ traceShowM $ "Time to run " <> label <> ": " ++ show (diffUTCTime end start)
  pure res

getResolvedVar :: MonadIO m => Hir.Program -> LineCol -> [FilePath] -> m (Maybe (H.Variable RenamePhase))
getResolvedVar target lc srcFiles = do
  time "resolvedVar" $ do
    (requiredPrograms, exportIdx) <- liftIO $ time "gather" $ gatherScopeDeps target srcFiles
    let renameTree = renamePrg requiredPrograms exportIdx target
    pure $ (AST.getDeepestContainingLineCol @(H.Variable RenamePhase) (point lc)) . (.dynNode) =<< renameTree

varToHover :: (H.Variable RenamePhase) -> Hover
varToHover varNode =
  let mResolvedVar = varNode.ext
      range = ProtoLSP.lineColRangeToProto varNode.dynNode.nodeLineColRange
      contents = fromMaybe "" (resolvedVarToContents =<< mResolvedVar)
    in
    Hover
      { _range = Just range
      , _contents = InL $ MarkupContent MarkupKind_Markdown contents
      }

resolvedVarToContents :: ResolvedVariable -> Maybe Text
resolvedVarToContents resolvedVar =
  case resolvedVar of
    ResolvedVariable (ResolvedGlobal glblVarInfo) ->
      Just $ renderGlblVarInfo glblVarInfo
    _ -> Nothing

renderGlblVarInfo :: GlblVarInfo -> Text
renderGlblVarInfo glblVarInfo =
  wrapHaskell tySig <>
  "\nimported from: " <> glblVarInfo.importedFrom.text
  <> "\noriginates from: " <> glblVarInfo.originatingMod.text
    where
      tySig =
          maybe "" (.node.dynNode.nodeText) glblVarInfo.sig
      wrapHaskell x = "\n```haskell\n" <> x <> "\n```\n"


-- | Retrieve hover information.
retrieveHover ::
  forall m.
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m (Maybe Hover)
retrieveHover path lineCol = do
  pos <- lineColToPos path lineCol
  throwIfInThSplice "retrieveHover" path pos
  prg <- getHir path
  srcFiles <- (.srcDirs) <$> getStaticEnv
  mVarNode <- getResolvedVar prg lineCol (Path.toFilePath <$> srcFiles)
  let astResult = varToHover <$> mVarNode
  hieResult <- runMaybeT $ do
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
              (hoverInfo mVarNode (GHC.hie_types hieFile) docs)
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
  pure $ hieResult <|> astResult
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
  let node = AST.getDeepestContaining @(H.ModuleP AST.:+ Hir.ParseQualifiedTypes) range hs.dynNode
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
