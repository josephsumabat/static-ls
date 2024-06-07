{-# LANGUAGE TypeApplications #-}

module StaticLS.HIE (
  hieAstNodeToIdentifiers,
  identifiersToNames,
  hieAstToNames,
  hieAstsAtPoint,
  hiedbCoordsToLspPosition,
  lspPositionToHieDbCoords,
  namesAtPoint,
  lspPositionToASTPoint,
  astRangeToLspRange,
  lineColToAstPoint,
  hiedbCoordsToLineCol,
  astRangeToLineColRange,
  lineColToHieDbCoords,
  getTypesAtPoint,
)
where

import AST qualified
import Control.Error.Util (hush)
import Control.Exception (Exception)
import Control.Monad (guard, join, (<=<))
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable qualified as Foldable
import Data.LineColRange (LineColRange (..))
import Data.LineColRange qualified as LineColRange
import Data.List (isSuffixOf)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (LineCol (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import Development.IDE.GHC.Error (
  srcSpanToFilename,
  srcSpanToRange,
 )
import GHC qualified
import GHC.Data.FastString qualified as GHC
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC
import GHC.Iface.Type qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Utils.Monad (mapMaybeM)
import HieDb (pointCommand)
import HieDb qualified
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.FileEnv
import StaticLS.HIE.File
import StaticLS.IDE.FileWith (FileLcRange, FileWith (..))
import StaticLS.Logger
import StaticLS.Maybe
import StaticLS.StaticEnv
import StaticLS.StaticLsEnv
import System.Directory (doesFileExist)

-- | LSP Position is 0 indexed
-- Note HieDbCoords are 1 indexed
type HieDbCoords = (Int, Int)

data UIntConversionException = UIntConversionException
  deriving (Show)

instance Exception UIntConversionException

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

hiedbCoordsToLspPosition :: (Monad m) => HieDbCoords -> ExceptT UIntConversionException m LSP.Position
hiedbCoordsToLspPosition (line, col) = LSP.Position <$> intToUInt (line - 1) <*> intToUInt (col - 1)

hiedbCoordsToLineCol :: HieDbCoords -> LineCol
hiedbCoordsToLineCol (line, col) = LineCol (line - 1) (col - 1)

lspPositionToHieDbCoords :: LSP.Position -> HieDbCoords
lspPositionToHieDbCoords position = (fromIntegral position._line + 1, fromIntegral position._character + 1)

lspPositionToASTPoint :: LSP.Position -> AST.Point
lspPositionToASTPoint position =
  AST.Point
    { row = fromIntegral position._line
    , col = fromIntegral position._character
    }

lineColToHieDbCoords :: LineCol -> HieDbCoords
lineColToHieDbCoords (LineCol line col) = (line + 1, col + 1)

lineColToAstPoint :: LineCol -> AST.Point
lineColToAstPoint (LineCol line col) =
  AST.Point
    { row = fromIntegral line
    , col = fromIntegral col
    }

astRangeToLineColRange :: AST.Range -> LineColRange
astRangeToLineColRange range =
  LineColRange
    ( LineCol
        ( fromIntegral $
            AST.row $
              AST.startPoint range
        )
        (fromIntegral $ AST.col $ AST.startPoint range)
    )
    ( LineCol
        (fromIntegral $ AST.row $ AST.endPoint range)
        (fromIntegral (AST.col (AST.endPoint range)))
    )

-- TODO: this is wrong, ast positions can hit the end of the line exclusive
-- but if lsp positions want to hit include the newline, it must start at the next line
astRangeToLspRange :: AST.Range -> LSP.Range
astRangeToLspRange range =
  LSP.Range
    { _start =
        LSP.Position
          { _line =
              fromIntegral $
                AST.row $
                  AST.startPoint range
          , _character = fromIntegral $ AST.col $ AST.startPoint range
          }
    , _end =
        LSP.Position
          { _line =
              fromIntegral $ AST.row $ AST.endPoint range
          , _character =
              fromIntegral (AST.col (AST.endPoint range))
          }
    }

-- | Use 'fromIntegral' when it is safe to do so
intToUInt :: (Monad m) => Int -> ExceptT UIntConversionException m LSP.UInt
intToUInt x =
  if minBoundAsInt <= x && x <= maxBoundAsInt
    then pure $ fromIntegral x
    else throwE UIntConversionException
 where
  minBoundAsInt = fromIntegral $ minBound @LSP.UInt
  maxBoundAsInt = fromIntegral $ maxBound @LSP.UInt

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
