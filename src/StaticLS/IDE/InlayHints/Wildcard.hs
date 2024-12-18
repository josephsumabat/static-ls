module StaticLS.IDE.InlayHints.Wildcard (getInlayHints) where

import AST.Cast
import AST.Haskell.Generated qualified as Haskell
import AST.Node
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.LineCol
import Data.LineColRange qualified as LineColRange
import Data.Maybe
import Data.Path
import Data.Pos as Pos
import Data.Range
import Data.Rope as Rope (posToLineCol, lineColToPos)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as Text
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.Type qualified as HieView.Type
import StaticLS.IDE.HiePos
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad
import StaticLS.Monad
import Data.List
import StaticLS.IDE.Implementation
import StaticLS.IDE.FileWith
import StaticLS.IDE.InlayHints.Common
import AST.Traversal


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


selectNodesToAnn haskell = do 
  let nodes = indexedTreeTraversalGeneric (:) [] (nodeToIndexedTree $ getDynNode haskell)
  let filteredNodePaths = [(wcn, rest)| ((_, node): rest) <- nodes, Just wcn <- [cast @Haskell.Wildcard node]]
  filteredNodePaths


mkInlayHint absPath haskell rope (wcn, parents) = do 
  wcr <- wcRecord absPath rope parents 
  case wcr of 
    Nothing -> pure Nothing 
    Just x -> pure $ Just $ mkInlayText (posToLineCol rope (nodeToRange wcn).end) (mconcat x)


wcRecord :: AbsPath -> Rope.Rope -> [(Int, DynNode)] -> StaticLsM (Maybe [Text])
wcRecord absPath rope parents = do
  let mrecord = find isRecord $ fmap snd parents
  case mrecord of 
    Nothing -> pure mempty
    Just record -> do 
      impl <- getImplementation absPath $ posToLineCol rope (nodeToRange record).start 
      recordInfo <- case impl of 
        [] -> pure []
        lcr : _ -> do 
          let file = lcr.path
          let lineCol = lcr.loc.start
          getRecordInfo file lineCol
      pure $ Just recordInfo



getRecordInfo :: AbsPath -> LineCol -> StaticLsM [Text]
getRecordInfo absPath lineCol = do
  haskell <- getHaskell absPath 
  rope <- getSourceRope absPath 
  let maybeRecord = getDeepestContaining @Haskell.Record (point $ Rope.lineColToPos rope lineCol) (getDynNode haskell)
  case maybeRecord of 
    Nothing -> pure []
    Just record -> do 
       let nameNodes = analyzeRecord record
       let names = fromMaybe [] $ (fmap . fmap) (Rope.toText . fromMaybe Rope.empty . Rope.indexRange rope . nodeRange) nameNodes 
       pure names

analyzeRecord :: Haskell.Record -> Maybe [DynNode]
analyzeRecord record = do 
  _:fields:_ <- pure (getDynNode record).nodeChildren
  _ <- cast @Haskell.Fields fields
  let fieldInfos = concat $ mapMaybe toFieldInfo (getDynNode fields).nodeChildren 
  pure fieldInfos


toFieldInfo :: DynNode -> Maybe [DynNode]
toFieldInfo node = do
  _ <- cast @Haskell.Field node
  ty: names <- pure node.nodeChildren
  Just names --  [(name, ty) | name <- names] -- feoij
 

isWildcard :: DynNode -> Bool
isWildcard = isJust . cast @Haskell.Wildcard

isRecord = isJust . cast @Haskell.Record


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
