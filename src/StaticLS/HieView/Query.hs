{-# LANGUAGE NoMonoLocalBinds #-}

module StaticLS.HieView.Query (
  namesAtRange,
  smallestContainingSatisfying,
  fileTysAtRangeList,
  fileNamesAtRangeList,
  fileIdentifiersAtRangeList,
  fileLocalBindsAtRangeList,
  fileNamesWithDefRange,
  fileSymbolsList,
)
where

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Lazy qualified as HashMap
import Data.LineColRange (LineColRange)
import Data.LineColRange qualified as LineColRange
import Data.Monoid (First (..))
import Optics
import StaticLS.HieView.Name qualified as Name
import StaticLS.HieView.Type (Type, TypeIndex)
import StaticLS.HieView.Type qualified as Type
import StaticLS.HieView.View

fileSymbolsList :: File -> [Name]
fileSymbolsList file =
  file ^.. fileAsts % astSourceIdentifiers % filtered filterIdent % _1 % _IdentName
 where
  filterIdent (_, details) = anyOf (to (.info) % folded) isSymbol details

  isSymbol = \case
    ValBind _ ModuleScope _ -> True
    TyDecl -> True
    ClassTyDecl {} -> True
    Decl {} -> True
    _ -> False

fileAsts :: Fold File (Ast TypeIndex)
fileAsts = to (.asts) % folded

fileLocalBindsAtRangeList :: Maybe LineColRange -> File -> [Name]
fileLocalBindsAtRangeList range file = do
  file ^.. (fileAsts % smallestContainingFold range % astAllIdentifers % filtered filterIdent % _1 % _IdentName)
 where
  filterIdent (_, details) = anyOf (to (.info) % folded) getLocalBind details

  getLocalBind = \case
    ValBind _ (LocalScope _) _ -> True
    PatternBind _ (LocalScope _) _ -> True
    PatternBind (LocalScope _) _ _ -> True
    _ -> False

fileNamesAtRangeList :: Maybe LineColRange -> File -> [Name]
fileNamesAtRangeList range file = file ^.. namesAtRange range

fileIdentifiersAtRangeList :: Maybe LineColRange -> File -> [Identifier]
fileIdentifiersAtRangeList range file = file ^.. fileAsts % astIdentifiersAtRange range % _1

fileNamesWithDefRange :: [LineColRange] -> File -> [Name]
fileNamesWithDefRange defRange file =
  file ^.. fileAsts % astIdentifiersAtRange Nothing % _1 % _IdentName % filtered keepName
 where
  keepName name = case Name.getRange name of
    Just range -> range `elem` defRange
    Nothing -> False

namesAtRange :: Maybe LineColRange -> Fold File Name
namesAtRange range = fileAsts % astIdentifiersAtRange range % _1 % _IdentName

astIdentifiersAtRange :: Maybe LineColRange -> Fold (Ast a) (Identifier, IdentifierDetails a)
astIdentifiersAtRange range = smallestContainingFold range % everyAst % astSourceIdentifiers

fileTysAtRangeList :: File -> LineColRange -> [Type]
fileTysAtRangeList file range =
  tys
 where
  tyIxs = file ^.. (fileAsts % to (smallestContaining range) % folded % astEveryTy)
  tyIxs' = nubOrd tyIxs
  tys = fmap (Type.recoverFullType file.typeArray) tyIxs'

smallestContaining :: LineColRange -> Ast a -> Maybe (Ast a)
smallestContaining range = smallestContainingSatisfying range Just

smallestContainingFold :: Maybe LineColRange -> AffineFold (Ast a) (Ast a)
smallestContainingFold (Just range) = to (smallestContaining range) % _Just
smallestContainingFold Nothing = afolding Just

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

astAllIdentifers :: Fold (Ast a) (Identifier, IdentifierDetails a)
astAllIdentifers = nodeInfos % to (.identifiers) % to HashMap.toList % folded

astSourceIdentifiers :: Fold (Ast a) (Identifier, IdentifierDetails a)
astSourceIdentifiers = sourceInfo % to (.identifiers) % to HashMap.toList % folded

nodeInfos :: Fold (Ast a) (NodeInfo a)
nodeInfos = to (.sourcedNodeInfo) % traversed

sourceInfo :: AffineFold (Ast a) (NodeInfo a)
sourceInfo = to (.sourcedNodeInfo) % ix SourceInfo

everyAst :: Fold (Ast a) (Ast a)
everyAst = foldVL $ \k ast -> k ast *> traverseOf_ folded k ast.children

astEveryTy :: Fold (Ast a) a
astEveryTy = everyAst % astImmediateTys

astImmediateTys :: Fold (Ast a) a
astImmediateTys = (astAllIdentifers % _2 % to (.ty) % _Just) `summing` (nodeInfos % to (.tys) % folded)
