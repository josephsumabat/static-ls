module StaticLS.HIE.Queries where

import AST qualified
import Control.Error.Util (hush)
import Control.Monad (join, (<=<))
import Control.Monad.Catch
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.LineColRange (LineColRange (..))
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Pos (LineCol (..), Pos (..))
import Data.Range (Range (..))
import Data.Set qualified as Set
import Data.Text (Text)
import GHC qualified
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC
import HieDb (pointCommand)
import Language.LSP.Protocol.Types qualified as LSP
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

namesAtPoint :: GHC.HieFile -> HieDbCoords -> [GHC.Name]
namesAtPoint hieFile position =
  identifiersToNames $ join (pointCommand hieFile position Nothing hieAstNodeToIdentifiers)

hieAstNodeToIdentifiers :: GHC.HieAST a -> [GHC.Identifier]
hieAstNodeToIdentifiers =
  (Set.toList . Map.keysSet) <=< fmap GHC.nodeIdentifiers . Map.elems . GHC.getSourcedNodeInfo . GHC.sourcedNodeInfo

identifiersToNames :: [GHC.Identifier] -> [GHC.Name]
identifiersToNames =
  mapMaybe hush

hieAstToNames :: GHC.HieAST a -> [GHC.Name]
hieAstToNames =
  identifiersToNames . hieAstNodeToIdentifiers

hieAstsAtPoint :: GHC.HieFile -> HieDbCoords -> Maybe HieDbCoords -> [GHC.HieAST GHC.TypeIndex]
hieAstsAtPoint hiefile start end = pointCommand hiefile start end id
