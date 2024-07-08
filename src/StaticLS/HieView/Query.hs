module StaticLS.HieView.Query (
  namesAtRange,
  smallestContainingSatisfying,
  identifiersAtRange,
  largestContainedBySatisfying,
  largestContainedBy,
  flattenAst,
  tysAtRange,
)
where

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

namesAtRange :: File -> LineColRange -> [Name]
namesAtRange file lineCol = Maybe.mapMaybe (identiferName . fst) $ identifiersAtRange file lineCol

tysAtRange :: File -> LineColRange -> [TypeIndex]
tysAtRange file range = concatMap getAstTys $ Maybe.mapMaybe (largestContainedBy range) (Map.elems file.asts)

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

largestContainedBy :: LineColRange -> Ast a -> Maybe (Ast a)
largestContainedBy range = largestContainedBySatisfying range Just

largestContainedBySatisfying ::
  LineColRange ->
  (Ast a -> Maybe b) ->
  Ast a ->
  Maybe b
largestContainedBySatisfying range cond node
  | range `LineColRange.containsRange` node.range =
      getFirst $
        mconcat
          [ First $ cond node
          , foldMap (First . largestContainedBySatisfying range cond) $
              node.children
          ]
  | node.range `LineColRange.containsRange` range = Nothing
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

-- get the source info, not the generated one
getAstSourceInfo :: Ast a -> Maybe (NodeInfo a)
getAstSourceInfo ast = Map.lookup SourceInfo ast.sourcedNodeInfo

flattenAst :: Ast a -> [Ast a]
flattenAst ast = ast : concatMap flattenAst ast.children

getAstTys :: Ast a -> [a]
getAstTys = concatMap getAstTys . flattenAst

getAstImmediateTys :: Ast a -> [a]
getAstImmediateTys ast = concatMap (.tys) (Map.elems ast.sourcedNodeInfo)
