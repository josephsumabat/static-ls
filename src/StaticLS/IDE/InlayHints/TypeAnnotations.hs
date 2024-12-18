{-# LANGUAGE LambdaCase #-}

module StaticLS.IDE.InlayHints.TypeAnnotations (getInlayHints) where

import AST.Cast
import AST.Haskell.Generated qualified as Haskell
import AST.Node
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

getInlayHints :: AbsPath -> Maybe Int -> StaticLsM [InlayHint]
getInlayHints path maxLen = getTypedefInlays_ path maxLen

getTypedefInlays :: AbsPath -> (LineCol -> [Text]) -> Maybe Int -> StaticLsM [InlayHint]
getTypedefInlays absPath getTypes maxLen = do
  haskell <- getHaskell absPath
  rope <- getSourceRope absPath
  let targetNodes = selectNodesToType (getDynNode haskell)
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

getTypedefInlays_ :: AbsPath -> Maybe Int -> StaticLsM [InlayHint]
getTypedefInlays_ absPath maxLen = do
  hieView' <- runMaybeT $ getHieView absPath
  case hieView' of
    Nothing -> pure []
    Just hieView -> do
      let getTypes lineCol = do
            let tys = HieView.Query.fileTysAtRangeList hieView (LineColRange.point lineCol)
            fmap HieView.Type.printType tys
      getTypedefInlays absPath getTypes maxLen

nodeIsVarAtBinding astLoc = isJust $ do
  let headNode = nodeAtLoc astLoc
  let nameCorrect = maybe False (`elem` ["name", "pattern", "element", "left_operand", "right_operand"]) headNode.nodeFieldName
  let typeCorrect = isJust (cast @Haskell.Variable headNode) || isJust (cast @Haskell.Function headNode)
  let headNodeGood = nameCorrect && typeCorrect
  let criterion = nthChildOf 0 (\y -> isJust (cast @Haskell.Bind y) || isJust (cast @Haskell.Alternative y) || isJust (cast @Haskell.Function y))
  guard headNodeGood
  bindSite <- findAncestor criterion astLoc
  bindSiteP <- parent =<< parent bindSite
  outerBindSite <- findAncestor ((\p -> isLet p || isBind p) . nodeAtLoc) bindSiteP
  pure True

nodeIsVarBoundInLambda :: ASTLoc -> Bool
nodeIsVarBoundInLambda astLoc = isVar (nodeAtLoc astLoc) && isJust (findAncestor (isJust . cast @Haskell.Patterns . nodeAtLoc) astLoc)

isFunction :: DynNode -> Bool
isFunction = isJust . cast @Haskell.Function

isBind :: DynNode -> Bool
isBind = isJust . cast @Haskell.Bind

isLet :: DynNode -> Bool
isLet = isJust . cast @Haskell.Let

isLam :: DynNode -> Bool
isLam = isJust . cast @Haskell.Lambda

isVar :: DynNode -> Bool
isVar = isJust . cast @Haskell.Variable

selectNodesToType :: DynNode -> [DynNode]
selectNodesToType root = do
  let leafNodes = leaves (rootToASTLoc root)
  let selectedLeafNodes = filter nodeIsVarAtBinding leafNodes
  fmap nodeAtLoc selectedLeafNodes

lastSafe :: [a] -> Maybe a
lastSafe = listToMaybe . reverse
