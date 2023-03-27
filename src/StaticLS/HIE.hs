{-# LANGUAGE TypeApplications #-}

module StaticLS.HIE where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Data.Text
import qualified GHC
import qualified GHC.Iface.Ext.Binary as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Types.Name.Cache as GHC
import HieDb (getHieFilesIn, pointCommand)
import Language.LSP.Types
import qualified Language.LSP.Types as LSP
import StaticLS.HIE.File
import System.Directory (makeAbsolute)
import System.FilePath (normalise, (</>))

type HieDbCoords = (Int, Int)

hieAstNodeToIdentifiers :: GHC.HieAST a -> [GHC.Identifier]
hieAstNodeToIdentifiers =
    (Set.toList . Map.keysSet) <=< fmap GHC.nodeIdentifiers . Map.elems . GHC.getSourcedNodeInfo . GHC.sourcedNodeInfo

identifiersToNames :: [GHC.Identifier] -> [GHC.Name]
identifiersToNames =
    mapMaybe eitherToMaybe
  where
    eitherToMaybe =
        \case
            Left _ -> Nothing
            Right a -> Just a

hieAstToNames :: GHC.HieAST a -> [GHC.Name]
hieAstToNames =
    identifiersToNames . hieAstNodeToIdentifiers

hieAstsAtPoint :: GHC.HieFile -> HieDbCoords -> Maybe HieDbCoords -> [GHC.HieAST GHC.TypeIndex]
hieAstsAtPoint hiefile start end = pointCommand hiefile start end id

hiedbCoordsToLspPosition :: HieDbCoords -> Maybe LSP.Position
hiedbCoordsToLspPosition (line, col) = Position <$> intToUInt (line - 1) <*> intToUInt (col - 1)

lspPositionToHieDbCoords :: LSP.Position -> HieDbCoords
lspPositionToHieDbCoords position = (fromIntegral position._line + 1, fromIntegral position._character + 1)

-- | Use 'fromIntegral' when it is safe to do so
intToUInt :: Int -> Maybe UInt
intToUInt x =
    if minBoundAsInt <= x && x <= maxBoundAsInt
        then Just $ fromIntegral x
        else Nothing
  where
    minBoundAsInt = fromIntegral $ minBound @UInt
    maxBoundAsInt = fromIntegral $ maxBound @UInt
