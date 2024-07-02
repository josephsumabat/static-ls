module StaticLS.HIE.Queries where

import Control.Error.Util (hush)
import Control.Monad (join)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.LineCol (LineCol (..))
import Data.Pos (Pos (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC qualified
import GHC.Data.FastString qualified as GHC
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC
import GHC.Plugins qualified as GHC
import HieDb (pointCommand)
import StaticLS.HIE.Position
import StaticLS.SDoc (showGhc)

getPrintedTypesAtPoint :: GHC.HieFile -> LineCol -> [Text]
getPrintedTypesAtPoint hieFile lineCol =
  ( showGhc
      . GHC.hieTypeToIface
      . flip
        GHC.recoverFullType
        (GHC.hie_types hieFile)
  )
    <$> getTypesAtPoint hieFile (lineColToHieDbCoords lineCol)

getTypesAtPoint :: GHC.HieFile -> HieDbCoords -> [GHC.TypeIndex]
getTypesAtPoint hieFile coords =
  join $
    HieDb.pointCommand
      hieFile
      coords
      Nothing
      ( \hieAst -> do
          let nodeInfo = nodeInfo' hieAst
          let identTypes = mapMaybe GHC.identType $ Map.elems $ GHC.nodeIdentifiers nodeInfo
          identTypes ++ GHC.nodeType nodeInfo
      )

nodeInfo' :: GHC.HieAST GHC.TypeIndex -> GHC.NodeInfo GHC.TypeIndex
nodeInfo' = Map.foldl' combineNodeInfo' GHC.emptyNodeInfo . GHC.getSourcedNodeInfo . GHC.sourcedNodeInfo

combineNodeInfo' :: GHC.NodeInfo GHC.TypeIndex -> GHC.NodeInfo GHC.TypeIndex -> GHC.NodeInfo GHC.TypeIndex
GHC.NodeInfo as ai ad `combineNodeInfo'` GHC.NodeInfo bs bi bd =
  GHC.NodeInfo (Set.union as bs) (mergeSorted ai bi) (Map.unionWith (<>) ad bd)

mergeSorted :: [GHC.TypeIndex] -> [GHC.TypeIndex] -> [GHC.TypeIndex]
mergeSorted la@(a : as0) lb@(b : bs0) = case compare a b of
  LT -> a : mergeSorted as0 lb
  EQ -> a : mergeSorted as0 bs0
  GT -> b : mergeSorted la bs0
mergeSorted as0 [] = as0
mergeSorted [] bs0 = bs0

mkPointSpan :: GHC.FastString -> (Int, Int) -> GHC.Span
mkPointSpan path (line, col) = sp path
 where
  sloc fs = GHC.mkRealSrcLoc fs line col
  eloc fs = sloc fs
  sp fs = GHC.mkRealSrcSpan (sloc fs) (eloc fs)

makeSpans :: GHC.HieFile -> HieDbCoords -> [GHC.Span]
makeSpans hieFile coords =
  fmap
    ( \((GHC.LexicalFastString path), _ast) -> do
        let span = mkPointSpan path coords
        span
    )
    asts
 where
  asts = fileAstList hieFile

namesAtPoint :: GHC.HieFile -> HieDbCoords -> [GHC.Name]
namesAtPoint hieFile position =
  mapMaybe ((\case Left _ -> Nothing; Right x -> Just x) . fst) idents
 where
  spans = makeSpans hieFile position
  idents = concatMap (identifiersAtSpan hieFile) spans

namesWithDefSpan :: GHC.HieFile -> GHC.Span -> [(GHC.Span, GHC.Name)]
namesWithDefSpan hieFile span =
  filter (\(_, name) -> (GHC.srcSpanToRealSrcSpan (GHC.nameSrcSpan name)) == Just span) $ concatMap astAllNames asts
 where
  asts = fmap snd $ fileAstList hieFile

identifiersAtSpan :: GHC.HieFile -> GHC.Span -> [(GHC.Identifier, GHC.IdentifierDetails GHC.TypeIndex)]
identifiersAtSpan hieFile span = results
 where
  results =
    concatMap
      ( \((GHC.LexicalFastString _path), ast) -> do
          let smallestAst =
                GHC.smallestContainingSatisfying
                  span
                  (not . null . nonGeneratedIdentifiers)
                  ast
          let identifiers =
                concatMap (concatMap getNodeInfoIdentifiersWithDetails . getNonGeneratedNodeInfo) $
                  Foldable.toList smallestAst
          identifiers
      )
      asts
  asts = fileAstList hieFile

fileAstList :: GHC.HieFile -> [(GHC.HiePath, GHC.HieAST GHC.TypeIndex)]
fileAstList = Map.toList . GHC.getAsts . GHC.hie_asts

findAllGlobalBinds :: GHC.HieFile -> [Text]
findAllGlobalBinds hieFile = undefined

findLocalBindsAtSpan :: GHC.HieFile -> GHC.Span -> [GHC.Name]
findLocalBindsAtSpan hieFile span =
  mapMaybe (\case Left _ -> Nothing; Right x -> Just x) results
 where
  results =
    concatMap
      ( \((GHC.LexicalFastString _path), ast) -> do
          let smallestAst =
                GHC.smallestContainingSatisfying
                  span
                  ( \ast ->
                      not . null $ getIdentifiersWithContext getLocalBind ast
                  )
                  ast
          concatMap (getIdentifiersWithContext getLocalBind) $ Foldable.toList smallestAst
      )
      asts
  asts = fileAstList hieFile

  getLocalBind = \case
    GHC.ValBind _ (GHC.LocalScope scopeSpan) _ -> True
    GHC.PatternBind _ (GHC.LocalScope _) _ -> True
    GHC.PatternBind (GHC.LocalScope _) _ _ -> True
    _ -> False

hieAstNodeToIdentifiers :: GHC.HieAST a -> [GHC.Identifier]
hieAstNodeToIdentifiers ast = Map.keys . GHC.sourcedNodeIdents . GHC.sourcedNodeInfo $ ast

identifiersToNames :: [GHC.Identifier] -> [GHC.Name]
identifiersToNames =
  mapMaybe hush

hieAstToNames :: GHC.HieAST a -> [GHC.Name]
hieAstToNames =
  identifiersToNames . hieAstNodeToIdentifiers

hieAstsAtPoint :: GHC.HieFile -> HieDbCoords -> Maybe HieDbCoords -> [GHC.HieAST GHC.TypeIndex]
hieAstsAtPoint hiefile start end = pointCommand hiefile start end id

astAllNames :: GHC.HieAST GHC.TypeIndex -> [(GHC.Span, GHC.Name)]
astAllNames = mapMaybe (\(span, ident) -> do ident <- hush ident; pure (span, ident)) . astAllIdentifiers

astAllIdentifiers :: GHC.HieAST GHC.TypeIndex -> [(GHC.Span, GHC.Identifier)]
astAllIdentifiers = foldMapAst go
 where
  go ast = fmap (span,) (hieAstNodeToIdentifiers ast)
   where
    span = GHC.nodeSpan ast

allGlobalSymbols :: GHC.HieFile -> [Text]
allGlobalSymbols hieFile = strings
 where
  strings = fmap (T.pack . GHC.occNameString . GHC.nameOccName) filtered
  filtered = mapMaybe (hush . fst) . filter (\(_ident, details) -> detailsMatchesContext globalDef details) $ res
  globalDef = \case
    GHC.ValBind _ GHC.ModuleScope _ -> True
    GHC.TyDecl -> True
    GHC.ClassTyDecl {} -> True
    GHC.Decl {} -> True
    _ -> False
  res = concatMap astAllIdentifiersWithDetails asts
  asts = snd <$> fileAstList hieFile

astAllIdentifiersWithDetails :: GHC.HieAST GHC.TypeIndex -> [(GHC.Identifier, GHC.IdentifierDetails GHC.TypeIndex)]
astAllIdentifiersWithDetails = foldMapAst go
 where
  go ast = concatMap getNodeInfoIdentifiersWithDetails $ getNonGeneratedNodeInfo ast

foldMapAst :: (Monoid m) => (GHC.HieAST a -> m) -> GHC.HieAST a -> m
foldMapAst f ast = f ast <> foldMap (foldMapAst f) (GHC.nodeChildren ast)

getNonGeneratedNodeInfo :: GHC.HieAST a -> [GHC.NodeInfo a]
getNonGeneratedNodeInfo =
  map snd
    . filter (\(origin, _) -> origin == GHC.SourceInfo)
    . Map.toList
    . GHC.getSourcedNodeInfo
    . GHC.sourcedNodeInfo

getNodeInfoIdentifiers :: GHC.NodeInfo a -> [GHC.Identifier]
getNodeInfoIdentifiers = Map.keys . GHC.nodeIdentifiers

getNodeInfoIdentifiersWithDetails :: GHC.NodeInfo a -> [(GHC.Identifier, GHC.IdentifierDetails a)]
getNodeInfoIdentifiersWithDetails = Map.toList . GHC.nodeIdentifiers

nonGeneratedIdentifiers :: GHC.HieAST a -> [GHC.Identifier]
nonGeneratedIdentifiers = concatMap getNodeInfoIdentifiers . getNonGeneratedNodeInfo

getIdentifiersWithContext :: (GHC.ContextInfo -> Bool) -> GHC.HieAST a -> [GHC.Identifier]
getIdentifiersWithContext pred ast = do
  let infos = getNonGeneratedNodeInfo ast
      identifiers = concatMap getNodeInfoIdentifiersWithDetails infos
  mapMaybe
    ( \(ident, details) -> do
        let contexts = (Set.toList . GHC.identInfo) details
        if any pred contexts then Just ident else Nothing
    )
    identifiers

detailsMatchesContext :: (GHC.ContextInfo -> Bool) -> GHC.IdentifierDetails a -> Bool
detailsMatchesContext pred details = any pred (Set.toList $ GHC.identInfo details)
