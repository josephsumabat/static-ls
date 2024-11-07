{-# LANGUAGE LambdaCase #-}

module StaticLS.IDE.InlayHints where

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
import Data.Rope
import Data.Text (Text)
import Data.Text qualified as Text
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.Type qualified as HieView.Type
import StaticLS.IDE.Monad
import StaticLS.Monad

data InlayHint = InlayHint
  { position :: LineCol
  , label :: Text
  , -- TODO add kind kind :: ??
    textEdits :: Maybe (Rope, [Change]) -- we need the rope to convert ranges in changes to lineColRanges
    -- TODO add tooltip
  }

mkInlayText :: LineCol -> Text -> InlayHint
mkInlayText lineCol text = InlayHint {position = lineCol, label = text, textEdits = Nothing}

getInlayHints :: AbsPath -> StaticLsM [InlayHint]
getInlayHints path = getTypedefInlays_ path

getTypedefInlays :: AbsPath -> (LineCol -> [Text]) -> StaticLsM [InlayHint]
getTypedefInlays absPath getTypes = do
  haskell <- getHaskell absPath
  rope <- getSourceRope absPath
  let targetNodes = selectNodesToType (getDynNode haskell)
  let ranges = nodeToRange <$> targetNodes
  let posns = posToLineCol rope . (.start) <$> ranges
  let endPosns = posToLineCol rope . (.end) <$> ranges
  let typeStrs = getTypes <$> posns
  let posnTypes = zip endPosns (fmtTypeStr . mconcat <$> typeStrs)
  let inlayHints = uncurry mkInlayText <$> posnTypes
  pure inlayHints

fmtTypeStr :: Text -> Text
fmtTypeStr text
  | text == "" = ""
  | Text.length text > 50 = "" -- hide overly long inlays and buggy inlays
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

nodeToDescendants :: DynNode -> [DynNode]
nodeToDescendants node = node : (nodeToDescendants . getDynNode =<< node.nodeChildren)

selectNodesToType :: DynNode -> [DynNode]
selectNodesToType root = do
  let allDescendants = nodeToDescendants root
  let varDescendants = fmap getDynNode $ mapMaybe (cast @Haskell.Variable) allDescendants
  let notableVarDescendants = filter (\n -> n.nodeFieldName `elem` [Just "name", Just "pattern", Just "element"]) varDescendants
  notableVarDescendants
