{-# LANGUAGE TypeApplications #-}

module StaticLS.HIE (
    hieAstNodeToIdentifiers,
    identifiersToNames,
    hieAstToNames,
    hieAstsAtPoint,
    hiedbCoordsToLspPosition,
    lspPositionToHieDbCoords,
    namesAtPoint,
    HieDbCoords,
)
where

import Control.Error.Util (hush)
import Control.Exception (Exception)
import Control.Monad (join, (<=<))
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Iface.Ext.Types as GHC
import HieDb (pointCommand)
import qualified Language.LSP.Protocol.Types as LSP

-- | Note HieDbCoords are 1 indexed
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

lspPositionToHieDbCoords :: LSP.Position -> HieDbCoords
lspPositionToHieDbCoords position = (fromIntegral position._line + 1, fromIntegral position._character + 1)

-- | Use 'fromIntegral' when it is safe to do so
intToUInt :: (Monad m) => Int -> ExceptT UIntConversionException m LSP.UInt
intToUInt x =
    if minBoundAsInt <= x && x <= maxBoundAsInt
        then pure $ fromIntegral x
        else throwE UIntConversionException
  where
    minBoundAsInt = fromIntegral $ minBound @LSP.UInt
    maxBoundAsInt = fromIntegral $ maxBound @LSP.UInt
