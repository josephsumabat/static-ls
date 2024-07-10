module StaticLS.HieView.Query (
  namesAtRange,
  smallestContainingSatisfying,
  identifiersAtRange,
  flattenAst,
  tysAtRange,
  astsAtRange,
)
where

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Lazy qualified as HashMap
import Data.LineColRange (LineColRange)
import Data.LineColRange qualified as LineColRange
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Monoid (First (..))
import StaticLS.HieView.Name (Name)
import StaticLS.HieView.Type (TypeIndex)
import StaticLS.HieView.View

localBindsAtRange :: File -> LineColRange -> [Name]
localBindsAtRange file range =
  []
  where
  getLocalBind = \case
    ValBind _ (LocalScope scopeSpan) _ -> True
    PatternBind _ (LocalScope _) _ -> True
    PatternBind (LocalScope _) _ _ -> True
    _ -> False


namesAtRange :: File -> LineColRange -> [Name]
namesAtRange file lineCol = Maybe.mapMaybe (identiferName . fst) $ identifiersAtRange file lineCol

tysAtRange :: File -> LineColRange -> [TypeIndex]
tysAtRange file range = nubOrd $ concatMap getAstTys $ Maybe.mapMaybe (smallestContaining range) (Map.elems file.asts)

astsAtRange :: File -> LineColRange -> [Ast TypeIndex]
astsAtRange file range = Maybe.mapMaybe (smallestContaining range) (Map.elems file.asts)

smallestContaining :: LineColRange -> Ast a -> Maybe (Ast a)
smallestContaining range = smallestContainingSatisfying range Just

smallestContainingSatisfying ::
  LineColRange ->
  (Ast a -> Maybe b) ->
  Ast a ->
  Maybe b
smallestContainingSatisfying range cond node
  | node.range `LineColRange.containsRange` range =
      getFirst $
        mconcat
          [ foldMap (First . smallestContainingSatisfying range cond) $
              node.children
          , First $ cond node
          ]
  | range `LineColRange.containsRange` node.range = Nothing
  | otherwise = Nothing

identifiersAtRange :: File -> LineColRange -> [(Identifier, IdentifierDetails TypeIndex)]
identifiersAtRange hieFile span = concatMap NE.toList results
 where
  results =
    Maybe.mapMaybe
      ( \ast -> do
          smallestContainingSatisfying
            span
            ( \ast -> do
                nodeInfo <- getAstSourceInfo ast
                identifiers <- NE.nonEmpty $ HashMap.toList nodeInfo.identifiers
                pure identifiers
            )
            ast
      )
      asts
  asts = Map.elems $ hieFile.asts

getAstIdentifiers :: Ast a -> [(Identifier, IdentifierDetails a)]
getAstIdentifiers ast = concatMap (HashMap.toList . (.identifiers)) $ getAstNodeInfos ast

-- get the source info, not the generated one
getAstSourceInfo :: Ast a -> Maybe (NodeInfo a)
getAstSourceInfo ast = Map.lookup SourceInfo ast.sourcedNodeInfo

getAstNodeInfos :: Ast a -> [NodeInfo a]
getAstNodeInfos ast = Map.elems ast.sourcedNodeInfo

flattenAst :: Ast a -> [Ast a]
flattenAst ast = ast : concatMap flattenAst ast.children

getAstTys :: Ast a -> [a]
getAstTys = concatMap getAstImmediateTys . flattenAst

getAstImmediateTys :: Ast a -> [a]
getAstImmediateTys ast = tys ++ identTys
 where
  identTys = Maybe.mapMaybe ((.ty) . snd) $ getAstIdentifiers ast
  tys = concatMap (.tys) (Map.elems ast.sourcedNodeInfo)
