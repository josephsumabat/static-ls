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
import System.Directory (makeAbsolute)
import System.FilePath (normalise, (</>))

type HieDbCoords = (Int, Int)
type SrcFilePath = FilePath
type HieFilePath = FilePath

data HieInfo = HieInfo
    { hieFilePath :: HieFilePath
    , hieFile :: GHC.HieFile
    }

-- TODO: make this configurable (use hie.yaml?)
hieDir :: FilePath
hieDir = ".hiefiles"

hieAstNodeToIdentifiers :: GHC.HieAST a -> [GHC.Identifier]
hieAstNodeToIdentifiers =
    (Set.toList . Map.keysSet) <=< fmap GHC.nodeIdentifiers . Map.elems . GHC.getSourcedNodeInfo . GHC.sourcedNodeInfo

-- | TODO: Fix the hie src path indexing so we dont need to do this load on startup
getHieFileMap :: FilePath -> IO (Map.Map SrcFilePath HieInfo)
getHieFileMap wsroot = do
    let hieFullPath = wsroot </> hieDir
    hieFilePaths <- getHieFilesIn hieFullPath
    nameCache <- GHC.initNameCache 'a' []
    srcPathHieInfoPairs <- mapM (srcFileToHieFileInfo nameCache) hieFilePaths

    pure $ Map.fromList srcPathHieInfoPairs
  where
    hieFileResultToSrcPath :: GHC.HieFileResult -> SrcFilePath
    hieFileResultToSrcPath = GHC.hie_hs_file . GHC.hie_file_result

    srcFileToHieFileInfo :: GHC.NameCache -> HieFilePath -> IO (SrcFilePath, HieInfo)
    srcFileToHieFileInfo nameCache hieFilePath = do
        hieFileResult <- GHC.readHieFile nameCache hieFilePath
        absSrcFilePath <- makeAbsolute hieFileResult.hie_file_result.hie_hs_file
        absHieFilePath <- makeAbsolute hieFilePath
        let hieInfo =
                HieInfo
                    { hieFilePath = absHieFilePath
                    , hieFile = hieFileResult.hie_file_result
                    }
        pure (absSrcFilePath, hieInfo)

hieFileMapToSrcMap :: Map.Map SrcFilePath HieInfo -> Map.Map HieFilePath SrcFilePath
hieFileMapToSrcMap =
    Map.fromList . (fmap (\(srcPath, hieInfo) -> (hieInfo.hieFilePath, srcPath))) . Map.toList

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
