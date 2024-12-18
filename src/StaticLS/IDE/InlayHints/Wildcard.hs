module StaticLS.IDE.InlayHints.Wildcard (getInlayHints) where

import AST.Cast
import AST.Haskell.Generated qualified as Haskell
import AST.Node
import AST.Traversal
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Char
import Data.LineCol
import Data.LineColRange
import Data.LineColRange qualified as LineColRange
import Data.List (find, nub)
import Data.Maybe
import Data.Path
import Data.Path qualified as Path
import Data.Pos as Pos
import Data.Range as Range
import Data.Rope as Rope (lineColToPos, posToLineCol)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins as GHC hiding ((<>))
import HieDb (pointCommand)
import StaticLS.HI
import StaticLS.HI.File
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.IDE.FileWith
import StaticLS.IDE.HiePos
import StaticLS.IDE.Implementation
import StaticLS.IDE.InlayHints.Common
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad hiding (lineColToPos)
import StaticLS.IDE.Monad qualified as IDE.Monad
import StaticLS.Monad

import AST qualified
import AST.Haskell qualified as H
import Control.Monad qualified as Monad
import Data.Maybe qualified as Maybe
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.HIE.Position
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.View qualified as HieView
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.Hover.Info
import StaticLS.Logger (logInfo)
import StaticLS.Maybe
import StaticLS.ProtoLSP qualified as ProtoLSP

getInlayHints :: AbsPath -> StaticLsM [InlayHint]
getInlayHints absPath = do
  getWildcardAnns absPath

getWildcardAnns :: AbsPath -> StaticLsM [InlayHint]
getWildcardAnns absPath = do
  haskell <- getHaskell absPath
  rope <- getSourceRope absPath
  let dynNodesToType = selectNodesToAnn haskell
  inlayHints <- catMaybes <$> traverse (mkInlayHint absPath haskell rope) dynNodesToType
  pure inlayHints

selectNodesToAnn :: Haskell.Haskell -> [(Haskell.Wildcard, ASTLoc)]
selectNodesToAnn haskell = do
  let astLocs = leaves $ rootToASTLoc $ getDynNode haskell
  [ (wc, par)
    | astLoc <- astLocs
    , Just wc <- [cast @Haskell.Wildcard (nodeAtLoc astLoc)]
    , Just par <- [parent astLoc]
    ]

mkInlayHint absPath haskell rope (wcn, parents) = do
  let lcr = nodeToRange wcn
  h <- retrieveHover absPath $ posToLineCol rope lcr.start
  case h of
    Nothing -> pure Nothing
    Just x -> pure $ Just $ mkInlayText (posToLineCol rope lcr.end) x

wcRecord :: AbsPath -> Rope.Rope -> ASTLoc -> StaticLsM (Maybe [Text])
wcRecord absPath rope parent = do
  let mrecord = findAncestor (isRecord . nodeAtLoc) parent
  case mrecord of
    Nothing -> pure $ Just ["James"]
    Just record -> do
      impl <- getImplementation absPath $ posToLineCol rope (nodeToRange $ nodeAtLoc record).start
      recordInfo <- case impl of
        [] -> pure ["Henry"]
        lcr : _ -> do
          let file = lcr.path
          let lineCol = lcr.loc.start
          getRecordInfo file lineCol
      pure $ Just recordInfo

getRecordInfo :: AbsPath -> LineCol -> StaticLsM [Text]
getRecordInfo absPath lineCol = do
  haskell <- getHaskell absPath
  rope <- getSourceRope absPath
  let maybeRecord = getDeepestContaining @Haskell.Record (Range.point $ Rope.lineColToPos rope lineCol) (getDynNode haskell)
  case maybeRecord of
    Nothing -> pure []
    Just record -> do
      let nameNodes = analyzeRecord record
      let names = fromMaybe [] $ (fmap . fmap) (Rope.toText . fromMaybe Rope.empty . Rope.indexRange rope . nodeRange) nameNodes
      pure names

analyzeRecord :: Haskell.Record -> Maybe [DynNode]
analyzeRecord record = do
  _ : fields : _ <- pure (getDynNode record).nodeChildren
  _ <- cast @Haskell.Fields fields
  let fieldInfos = concat $ mapMaybe toFieldInfo (getDynNode fields).nodeChildren
  pure fieldInfos

toFieldInfo :: DynNode -> Maybe [DynNode]
toFieldInfo node = do
  _ <- cast @Haskell.Field node
  ty : names <- pure node.nodeChildren
  Just names --  [(name, ty) | name <- names] -- feoij

isWildcard :: DynNode -> Bool
isWildcard = isJust . cast @Haskell.Wildcard

isRecord :: DynNode -> Bool
isRecord = isJust . cast @Haskell.Record

-- | Retrieve hover information.
retrieveHover ::
  forall m.
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m (Maybe Text)
retrieveHover path lineCol = do
  pos <- IDE.Monad.lineColToPos path lineCol
  throwIfInThSplice "retriveHover" path pos
  runMaybeT $ do
    hieFile <- getHieFile path
    hieView <- getHieView path
    lineCol' <- lineColToHieLineCol path lineCol
    lift $ logInfo $ T.pack $ "lineCol: " <> show lineCol
    lift $ logInfo $ T.pack $ "lineCol': " <> show lineCol'
    pos <- lift $ IDE.Monad.lineColToPos path lineCol
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
    pure . Text.intercalate ", " . filter (/= mempty) . nub . fmap clean $ snd srcInfo
 where
  clean = Text.takeWhile (not . isSpace) . Text.drop 7 . Text.filter (\x -> isAlphaNum x || elem x (" :." :: String))

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
        names = fmap HieView.Name.toGHCName $ HieView.Query.fileNamesAtRangeList (Just (LineColRange.point position)) hieView
        modNames = fmap GHC.moduleName . mapMaybe GHC.nameModule_maybe $ names
      modIfaceFiles <- fromMaybe [] <$> runMaybeT (mapM modToHiFile modNames)
      modIfaces <- catMaybes <$> mapM (runMaybeT . readHiFile . Path.toFilePath) modIfaceFiles
      let docs = getDocsBatch names =<< modIfaces
      pure docs
    else do
      pure []
