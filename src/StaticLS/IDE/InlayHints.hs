{-# LANGUAGE LambdaCase#-} 
module StaticLS.IDE.InlayHints (getInlayHints, InlayHint (..), InlayHintKind(..), InlayHintLabelPart (..), MarkupContent (..), Command (..)) where

import AST.Cast
import AST.Haskell.Generated qualified as Haskell
import AST.Node
import Control.Monad.Trans.Maybe
import Data.Change
import Data.LineCol
import Data.LineColRange qualified as LineColRange
import Data.Maybe
import Data.Path
import Data.Range
import Data.Rope qualified as Rope
import Data.Rope (Rope, posToLineCol)
import Data.Text (Text)
import Data.Text qualified as Text
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.Type qualified as HieView.Type
import StaticLS.IDE.Monad
import StaticLS.Monad
import Data.List
import StaticLS.IDE.HiePos
import StaticLS.IDE.FileWith
import Control.Monad
import Control.Applicative
import Data.Pos

getInlayHints :: AbsPath -> StaticLsM [InlayHint]
getInlayHints path = getTypedefInlays_ path


-- everything below this line could potentially be moved to a different file

-- data type and constructors
data InlayHint = InlayHint
  { position :: LineCol
  , label :: Either Text [InlayHintLabelPart]
  , kind :: Maybe InlayHintKind
  , -- TODO add kind kind :: ??
    textEdits :: Maybe (Rope, [Change]) -- we need the rope to convert ranges in changes to lineColRanges
    -- TODO add tooltip
  , paddingLeft :: Maybe Bool 
  , paddingRight :: Maybe Bool 
  }

data InlayHintLabelPart = InlayHintLabelPart {
  value :: Text,
  tooltip :: Maybe (Either Text MarkupContent),
  location :: Maybe FileLcRange,
  command :: Maybe Command
}


-- data types to be implemented later
data Command

data MarkupContent

defaultInlayHint = InlayHint {position = LineCol (Pos 0) (Pos 0), kind=Nothing, label = Left "", textEdits = Nothing, paddingLeft = Nothing, paddingRight = Nothing}


mkInlayText :: LineCol -> Text -> InlayHint
mkInlayText lineCol text = InlayHint {position = lineCol, kind=Nothing, label = Left text, textEdits = Nothing, paddingLeft = Nothing, paddingRight = Nothing}

mkTypedefInlay :: LineCol -> Text -> InlayHint
mkTypedefInlay lineCol text = (mkTruncatedInlayText lineCol text){kind = Just InlayHintKind_Type}
--
--
mkTruncatedInlayText :: LineCol -> Text -> InlayHint
mkTruncatedInlayText lineCol text = defaultInlayHint {position = lineCol, kind=Nothing, label = Right [InlayHintLabelPart {value = truncate text, tooltip = Just (Left text), location = Nothing, command = Nothing}]}
  where truncate text | Text.length text <= maxLen = text 
                      | otherwise = Text.take (maxLen - 1) text <> "\x2026"
        maxLen = 32


data InlayHintKind = InlayHintKind_Type | InlayHintKind_Parameter


getTypedefInlays :: AbsPath -> (LineCol -> [Text]) -> StaticLsM [InlayHint]
getTypedefInlays absPath getTypes = do
  haskell <- getHaskell absPath
  rope <- getSourceRope absPath
  let targetNodes = selectNodesToType (getDynNode haskell)
  let ranges = nodeToRange <$> targetNodes
  let srcLineCols' = posToLineCol rope . (.start) <$> ranges
  hieLineCols' <- traverse (runMaybeT . lineColToHieLineCol absPath) srcLineCols'
  let (srcLineCols, hieLineCols) = unzip $ mapMaybe sequenceA $ zip srcLineCols' hieLineCols'
  let endPosns = posToLineCol rope . (.end) <$> ranges
  let typeStrs = getTypes <$> hieLineCols
  let inlayData = zip endPosns (fmtTypeStr . fromMaybe "" . lastSafe <$> typeStrs)
  let inlayHints = uncurry mkTypedefInlay <$> inlayData
  pure inlayHints

fmtTypeStr :: Text -> Text
fmtTypeStr text
  | text == "" = ""
  -- | Text.length text > 50 = "" -- hide overly long inlays and buggy inlays foeoij
  | otherwise = " :: " <> text

getTypedefInlays_ :: AbsPath -> StaticLsM [InlayHint]
getTypedefInlays_ absPath = do
  hieView' <- runMaybeT $ getHieView absPath
  case hieView' of
    Nothing -> pure []
    Just hieView -> do
      let getTypes lineCol = do
            let tys = HieView.Query.fileTysAtRangeList hieView (LineColRange.point lineCol)
            fmap HieView.Type.printType tys
      getTypedefInlays absPath getTypes



includeNode :: DynNode -> [DynNode] -> Bool
includeNode node parents = do
  let isVariable = isJust $ cast @Haskell.Variable node
  let nodeFieldName = node.nodeFieldName 
  let validNodeFieldName = maybe False (`elem` ["name", "pattern", "element", "left_operand", "right_operand"]) nodeFieldName
  isVariable && isNonTopLevel parents && validNodeFieldName


nodeIsVarAtBinding :: [(Int, DynNode)] -> Bool
nodeIsVarAtBinding path = do
  let checkHeadNode n = (isJust (cast @Haskell.Variable n) || isJust (cast @Haskell.Function n))
       && (maybe False (`elem` ["name", "pattern", "element", "left_operand", "right_operand"]) n.nodeFieldName)
  let headNodeGood = maybe False (checkHeadNode . snd) (listToMaybe path)
  let bindLhsP ((0, _):(_, y):_) = isJust (cast @Haskell.Bind y) || isJust (cast @Haskell.Alternative y) || isJust (cast @Haskell.Function y)
      bindLhsP _ = False
  (&&) headNodeGood $ isJust $ do
    bindLhs <- stripUntil bindLhsP path
    let d1 = drop 1 bindLhs 
    guard $ isNonTopLevel (snd <$> d1)
    pure ()


data IndexedTree a = IndexedTree {root :: (Int, a), children :: [IndexedTree a]}

nodeToIndexedTree :: DynNode -> IndexedTree DynNode
nodeToIndexedTree t = go ((-1),t) where
  go x@(_, node) = IndexedTree x $ go <$> zip [0..] node.nodeChildren 

indexedTreeTraversalGeneric :: ((Int, DynNode) -> st -> st) -> st -> IndexedTree DynNode -> [st]
indexedTreeTraversalGeneric fn = go where 
  go st tree = do 
      let newSt = fn tree.root st
      newSt : (go newSt =<< tree.children)


stripUntil :: ([a] -> Bool) -> [a] -> Maybe [a]
stripUntil f = go where 
  go xs | f xs = Just xs
  go [] = Nothing
  go (_:ys) = stripUntil f ys


isFunction :: DynNode -> Bool
isFunction = isJust . cast @Haskell.Function

isBind :: DynNode -> Bool
isBind = isJust . cast @Haskell.Bind

isLet :: DynNode -> Bool
isLet = isJust . cast @Haskell.Let


isNonTopLevel :: [DynNode] -> Bool
isNonTopLevel parents = do
  let stripped = drop 1 $ dropWhile (\x -> not (isLet x || isBind x)) parents 
  any (\x -> isFunction x || isBind x) stripped


selectNodesToType :: DynNode -> [DynNode]
selectNodesToType root = do
  let tree = nodeToIndexedTree root 
  let trav = indexedTreeTraversalGeneric (:) [] tree 
  let trav' = filter nodeIsVarAtBinding trav 
  let trav'' = snd . head <$> trav'
  trav''


lastSafe :: [a] -> Maybe a
lastSafe = listToMaybe . reverse


