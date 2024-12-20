{-# LANGUAGE LambdaCase #-}

module StaticLS.IDE.InlayHints.TypeAnnotations (getInlayHints) where

import AST.Cast
import AST.Haskell.Generated qualified as Haskell
import AST.Node
import Control.Applicative as Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.LineCol
import Data.LineColRange qualified as LineColRange
import Data.Maybe
import Data.Path
import Data.Range
import Data.Rope (posToLineCol)
import Data.Text (Text)
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.Type qualified as HieView.Type
import StaticLS.IDE.HiePos
import StaticLS.IDE.InlayHints.Common
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad
import StaticLS.Monad
import StaticLS.StaticEnv.Options

getInlayHints :: StaticEnvOptions -> AbsPath -> StaticLsM [InlayHint]
getInlayHints options absPath = do
  hieView' <- runMaybeT $ getHieView absPath
  case hieView' of
    Nothing -> pure []
    Just hieView -> do
      let getTypes lineCol = do
            let tys = HieView.Query.fileTysAtRangeList hieView (LineColRange.point lineCol)
            fmap HieView.Type.printType tys
      getTypedefInlays options absPath getTypes

getTypedefInlays :: StaticEnvOptions -> AbsPath -> (LineCol -> [Text]) -> StaticLsM [InlayHint]
getTypedefInlays options absPath getTypes = do
  haskell <- getHaskell absPath
  rope <- getSourceRope absPath
  let maxLen = options.inlayLengthCap
  let targetNodes = selectNodesToType options (getDynNode haskell)
  let ranges = nodeToRange <$> targetNodes
  let srcLineCols' = posToLineCol rope . (.start) <$> ranges
  hieLineCols' <- traverse (runMaybeT . lineColToHieLineCol absPath) srcLineCols'
  let hieLineCols = catMaybes hieLineCols'
  let endPosns = posToLineCol rope . (.end) <$> ranges
  let typeStrs = getTypes <$> hieLineCols
  let inlayData = zip endPosns (fmtTypeStr . fromMaybe "" . lastSafe <$> typeStrs)
  let inlayHints = uncurry (mkTypedefInlay maxLen) <$> inlayData
  pure inlayHints

fmtTypeStr :: Text -> Text
fmtTypeStr text
  | text == "" = ""
  | otherwise = ":: " <> text

nodeIsVarAtBinding :: ASTLoc -> Bool
nodeIsVarAtBinding astLoc = isJust $ do
  let curNode = nodeAtLoc astLoc
  let nameCorrect = maybe False (`elem` ["name", "pattern", "element", "left_operand", "right_operand"]) curNode.nodeFieldName
  let typeCorrect = isJust (cast @Haskell.Variable curNode) || isJust (cast @Haskell.Function curNode)
  let headNodeGood = nameCorrect && typeCorrect
  let criterion = nthChildOf 0 (\y -> isBind y || isAlt y || isFunction y)
  guard headNodeGood
  bindSite <- parent =<< findAncestor criterion astLoc
  bindSiteP <- parent bindSite
  _ <- findAncestor ((\p -> isLet p || isBind p || isFunction p) . nodeAtLoc) bindSiteP
  pure True

nodeIsRecordVar :: ASTLoc -> Bool
nodeIsRecordVar astLoc = isJust $ do
  let curNode = nodeAtLoc astLoc
  _ <- cast @Haskell.Variable curNode
  let name = curNode.nodeFieldName
  let isBound = maybe False (`elem` ["pattern", "element", "left_operand", "right_operand"]) name
  fpParent <- findAncestor (isJust . cast @Haskell.FieldPattern . nodeAtLoc) astLoc
  let fpChildren = children fpParent
  case length fpChildren of
    1 -> Applicative.empty
    _ -> guard isBound

nodeIsUpdatedField :: ASTLoc -> Bool
nodeIsUpdatedField astLoc = isJust $ do
  let curNode = nodeAtLoc astLoc
  _ <- cast @Haskell.Variable curNode
  -- let name = curNode.nodeFieldName
  -- let isBound = maybe False (`elem` ["pattern", "element", "left_operand", "right_operand"]) name
  -- let isPun = name == Nothing
  fieldNode <- findAncestor (isJust . cast @Haskell.FieldName . nodeAtLoc) astLoc
  guard $ childIndex fieldNode == Just 0
  _ <- findAncestor (isJust . cast @Haskell.FieldUpdate . nodeAtLoc) fieldNode
  pure ()

isAlt :: DynNode -> Bool
isAlt = isJust . cast @Haskell.Alternative

isFunction :: DynNode -> Bool
isFunction = isJust . cast @Haskell.Function

isBind :: DynNode -> Bool
isBind = isJust . cast @Haskell.Bind

isLet :: DynNode -> Bool
isLet = isJust . cast @Haskell.Let

selectNodesToType :: StaticEnvOptions -> DynNode -> [DynNode]
selectNodesToType options root = do
  let leafNodes = leaves (rootToASTLoc root)
  let checks = [nodeIsVarAtBinding, nodeIsUpdatedField] <> [nodeIsRecordVar | options.experimentalFeatures]
  let selectedLeafNodes = filter (or . sequenceA checks) leafNodes
  fmap nodeAtLoc selectedLeafNodes

lastSafe :: [a] -> Maybe a
lastSafe = listToMaybe . reverse
