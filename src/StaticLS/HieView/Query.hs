-- We use optics in this module because it makes the code a lot cleaner
-- All exported functions return lists, so that optics are only an implementation detail
module StaticLS.HieView.Query (
  namesAtRange,
  smallestContainingSatisfying,
  fileTysAtRangeList,
  fileNamesAtRangeList,
  fileIdentifiersAtRangeList,
  fileLocalBindsAtRangeList,
  fileNamesWithDefRange,
  fileSymbolsList,
  fileRefsWithDefRanges,
  fileEvidenceUsesAtRangeList,
  fileEvidenceBinds,
)
where

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Lazy (HashMap)
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
  file ^.. fileAsts % astIdentifiersAtRange Nothing % filtered filterIdent % _1 % _IdentName
 where
  filterIdent (_, details) = anyOf (to (.info) % folded) isSymbol details

  isSymbol = \case
    ValBind _ ModuleScope _ -> True
    TyDecl -> True
    ClassTyDecl {} -> True
    Decl {} -> True
    _ -> False

-- | Get all references in a file that have a definition in the given ranges
-- Crucially we get the ranges from the ast rather than the name.
-- This is because the ast gives as the *usage* ranges, while the name gives as us the definition ranges.
-- We use an indexed fold to put the range aside while we filter the name. Afterwards, we collect the set aside ranges.
fileRefsWithDefRanges :: [LineColRange] -> File -> [LineColRange]
fileRefsWithDefRanges defRange file =
  fmap fst $
    itoListOf
      ( fileAsts
          % everyAst
          % ifolding (\ast -> (ast.range, ast))
          % astSourceIdentifiers
          % _1
          % _IdentName
          % filtered nameInRange
      )
      file
 where
  nameInRange name = case Name.getRange name of
    Just range -> range `elem` defRange
    Nothing -> False

fileAsts :: Fold File (Ast TypeIndex)
fileAsts = to (.asts) % folded

fileLocalBindsAtRangeList :: Maybe LineColRange -> File -> [Name]
fileLocalBindsAtRangeList range file = do
  toListOf
    ( fileAsts
        % smallestContainingFold range
        % astSourceIdentifiers
        % filtered filterIdent
        % _1
        % _IdentName
    )
    file
 where
  filterIdent (_, details) = anyOf (to (.info) % folded) getLocalBind details

  getLocalBind = \case
    ValBind _ (LocalScope _) _ -> True
    PatternBind _ (LocalScope _) _ -> True
    PatternBind (LocalScope _) _ _ -> True
    _ -> False

fileEvidenceBinds :: File -> HashMap Name [Name]
fileEvidenceBinds file =
  HashMap.fromListWith (++) $
    itoListOf
      ( fileAsts
          % smallestContainingFold Nothing
          % everyAst
          % astSourceIdentifiers
          % to (\(ident, info) -> (,info) <$> (ident ^? _IdentName))
          % _Just
          % ifolding id
          % to (.info)
          % folded
          % _EvidenceVarBind
          % _1
          % to evSourceNames
          % _Just
      )
      file
 where
  evSourceNames (EvInstBind {}) = Nothing
  evSourceNames (EvLetBind (EvBindDeps {deps})) = Just deps
  evSourceNames EvOther = Nothing

fileEvidenceUsesAtRangeList :: Maybe LineColRange -> File -> [Name]
fileEvidenceUsesAtRangeList range file = do
  file
    ^.. fileAsts
    % astIdentifiersAtRange range
    % filtered filterIdent
    % _1
    % _IdentName
 where
  filterIdent (_, details) =
    anyOf
      (to (.info) % folded)
      (\case EvidenceVarUse -> True; _ -> False)
      details

fileNamesAtRangeList :: Maybe LineColRange -> File -> [Name]
fileNamesAtRangeList range file = file ^.. namesAtRange range

fileIdentifiersAtRangeList :: Maybe LineColRange -> File -> [Identifier]
fileIdentifiersAtRangeList range file = file ^.. fileAsts % astIdentifiersAtRange range % _1

fileNamesWithDefRange :: [LineColRange] -> File -> [Name]
fileNamesWithDefRange defRanges file =
  file ^.. fileAsts % astIdentifiersAtRange Nothing % _1 % _IdentName % filtered keepName
 where
  keepName name =
    case Name.getRange name of
      Just range -> range `elem` defRanges
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

children :: Fold (Ast a) (Ast a)
children = to (.children) % folded

everyAst :: Fold (Ast a) (Ast a)
everyAst = foldVL $ \k ast -> k ast *> traverseOf_ (children % everyAst) k ast

astEveryTy :: Fold (Ast a) a
astEveryTy = everyAst % astImmediateTys

astImmediateTys :: Fold (Ast a) a
astImmediateTys = (astAllIdentifers % _2 % to (.ty) % _Just) `summing` (nodeInfos % to (.tys) % folded)
