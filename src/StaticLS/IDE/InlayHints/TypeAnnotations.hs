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
import Data.Pos
import Data.Range
import Data.Rope (posToLineCol)
import Data.Text (Text)
import Data.Text qualified as Text
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.Type qualified as HieView.Type
import StaticLS.IDE.HiePos
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad
import StaticLS.Monad

getInlayHints :: AbsPath -> Maybe Int -> StaticLsM [InlayHint]
getInlayHints path maxLen = getTypedefInlays_ path maxLen

defaultInlayHint :: InlayHint
defaultInlayHint = InlayHint {position = LineCol (Pos 0) (Pos 0), kind = Nothing, label = Left "", textEdits = Nothing, paddingLeft = Nothing, paddingRight = Nothing}

mkInlayText :: LineCol -> Text -> InlayHint
mkInlayText lineCol text = defaultInlayHint {position = lineCol, label = Left text}

mkTypedefInlay :: Maybe Int -> LineCol -> Text -> InlayHint
mkTypedefInlay maxLen lineCol text = (mkInlayText lineCol (truncateInlay maxLen (formatInlayText text))) {kind = Just InlayHintKind_Type, paddingLeft = Just True}

formatInlayText :: Text -> Text
formatInlayText = normalizeWhitespace
 where
  normalizeWhitespace = Text.unwords . Text.words

truncateInlay :: Maybe Int -> Text -> Text
truncateInlay Nothing text = text
truncateInlay (Just maxLen) text
  | Text.length text <= maxLen = text
  | otherwise = Text.take maxLen text <> "\x2026"

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

nodeIsVarAtBinding :: [(Int, DynNode)] -> Bool
nodeIsVarAtBinding path = do
  let checkHeadNode n =
        (isJust (cast @Haskell.Variable n) || isJust (cast @Haskell.Function n))
          && (maybe False (`elem` ["name", "pattern", "element", "left_operand", "right_operand"]) n.nodeFieldName)
  let headNodeGood = maybe False (checkHeadNode . snd) (listToMaybe path)
  let bindLhsP ((0, _) : (_, y) : _) = isJust (cast @Haskell.Bind y) || isJust (cast @Haskell.Alternative y) || isJust (cast @Haskell.Function y)
      bindLhsP _ = False
  (&&) headNodeGood $ isJust $ do
    bindLhs <- stripUntil bindLhsP path
    let d1 = drop 1 bindLhs
    guard $ isNonTopLevel (snd <$> d1)
    pure ()

_nodeIsVarBoundInLambda :: [(Int, DynNode)] -> Bool
_nodeIsVarBoundInLambda path = do
  let headIsVar = maybe False (isVar . snd) (listToMaybe path)

  headIsVar && any (isJust . cast @Haskell.Patterns . snd) path

-- logic for manipulating AST to find where to show inlay hints

data IndexedTree a = IndexedTree {root :: (Int, a), children :: [IndexedTree a]}

nodeToIndexedTree :: DynNode -> IndexedTree DynNode
nodeToIndexedTree t = go ((-1), t)
 where
  go x@(_, node) = IndexedTree x $ go <$> zip [0 ..] node.nodeChildren

indexedTreeTraversalGeneric :: ((Int, DynNode) -> st -> st) -> st -> IndexedTree DynNode -> [st]
indexedTreeTraversalGeneric fn = go
 where
  go st tree = do
    let newSt = fn tree.root st
    newSt : (go newSt =<< tree.children)

stripUntil :: ([a] -> Bool) -> [a] -> Maybe [a]
stripUntil f = go
 where
  go xs | f xs = Just xs
  go [] = Nothing
  go (_ : ys) = stripUntil f ys

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

isNonTopLevel :: [DynNode] -> Bool
isNonTopLevel parents = do
  let stripped = drop 1 $ dropWhile (\x -> not (isLet x || isBind x || isLam x)) parents
  any (\x -> isFunction x || isBind x) stripped

selectNodesToType :: DynNode -> [DynNode]
selectNodesToType root = do
  let tree = nodeToIndexedTree root
  let nodePaths = indexedTreeTraversalGeneric (:) [] tree
  let filteredNodePaths = filter (\x -> nodeIsVarAtBinding x) nodePaths
  let dynNodesToShow = snd . head <$> filteredNodePaths
  dynNodesToShow

lastSafe :: [a] -> Maybe a
lastSafe = listToMaybe . reverse
