{-# LANGUAGE TypeApplications #-}

module StaticLS.HIE
  ( hieAstNodeToIdentifiers,
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
  )
where

import AST qualified
import Control.Error.Util (hush)
import Control.Exception (Exception)
import Control.Monad (join, (<=<))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Pos (LineCol (..))
import Data.Set qualified as Set
import GHC qualified
import GHC.Iface.Ext.Types qualified as GHC
import HieDb (pointCommand)
import Language.LSP.Protocol.Types qualified as LSP

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
    { row = fromIntegral position._line,
      col = fromIntegral position._character
    }

lineColToAstPoint :: LineCol -> AST.Point
lineColToAstPoint (LineCol line col) =
  AST.Point
    { row = fromIntegral line,
      col = fromIntegral col
    }

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
                  AST.startPoint range,
            _character = fromIntegral $ AST.col $ AST.startPoint range
          },
      _end =
        LSP.Position
          { _line =
              fromIntegral $ AST.row $ AST.endPoint range,
            _character =
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
