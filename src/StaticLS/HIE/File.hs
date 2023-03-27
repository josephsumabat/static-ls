module StaticLS.HIE.File where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.List
import Data.List.Extra (dropPrefix)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Iface.Ext.Binary as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Types.Name.Cache as GHC
import qualified GHC.Unit.Types as GHC
import HieDb
import qualified Language.LSP.Types as LSP
import StaticLS.Monad
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath (normalise, (-<.>), (</>))

type SrcFilePath = FilePath
type HieFilePath = FilePath
data HieInfo = HieInfo
    { hieFilePath :: HieFilePath
    , hieFile :: GHC.HieFile
    }

-- TODO: make this configurable (use hie.yaml?)
hieDir :: FilePath
hieDir = ".hiefiles/"

-- TODO: make this configurable (use cabal?)
srcDirs :: [FilePath]
srcDirs = ["src/", "lib/", "test/"]

-- | Retrieve a hie info from a lsp text document identifier
getHieFile :: HasStaticEnv m => LSP.TextDocumentIdentifier -> m (Maybe GHC.HieFile)
getHieFile tdi = do
    staticEnv <- getStaticEnv
    hieFilePath <- runMaybeT $ do
        srcFilePath <- MaybeT $ pure $ LSP.uriToFilePath tdi._uri
        MaybeT $ srcFilePathToHieFilePath srcFilePath
    liftIO $ mapM (pure . GHC.hie_file_result <=< GHC.readHieFile staticEnv.nameCache) hieFilePath

-----------------------------------------------------------------------------------
-- File/Directory method for getting hie files - faster but somewhat "hacky"
-----------------------------------------------------------------------------------

hieFilePathToSrcFilePath :: HasStaticEnv m => HieFilePath -> m SrcFilePath
hieFilePathToSrcFilePath hiePath = do
    staticEnv <- getStaticEnv
    -- TODO: handle failure here
    hieFileResult <- liftIO $ GHC.readHieFile staticEnv.nameCache hiePath
    liftIO $ makeAbsolute hieFileResult.hie_file_result.hie_hs_file

{- | Retrieve a hie file path from a src path

Substitutes the src directory with the hie directory and the src file extension with
the hie file extension. Fragile, but works well in practice.

Presently necessary because hiedb does not currently index the hs_src file location
in the `mods` table
-}
srcFilePathToHieFilePath :: HasStaticEnv m => SrcFilePath -> m (Maybe HieFilePath)
srcFilePathToHieFilePath srcPath = do
    staticEnv <- getStaticEnv
    absoluteRoot <- liftIO $ makeAbsolute staticEnv.wsRoot
    let absoluteHieDir = absoluteRoot </> hieDir
        absoluteSrcDirs = (absoluteRoot </>) <$> srcDirs
    absoluteSrcPath <- liftIO $ makeAbsolute srcPath

    -- Drop all src directory prefixes
    let noPrefixSrcPath =
            foldl' (flip dropPrefix) absoluteSrcPath absoluteSrcDirs
        -- Set the hie directory path and substitute the file extension
        hiePath = absoluteHieDir </> noPrefixSrcPath -<.> ".hie"
    fileExists <- liftIO $ doesFileExist hiePath

    pure $
        if fileExists
            then Just hiePath
            else Nothing

-----------------------------------------------------------------------------------
-- Map index method for getting hie files - too slow in practice on startup but makes
-- finding references for functions that are used a lot much faster
-----------------------------------------------------------------------------------

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
    Map.fromList . fmap (\(srcPath, hieInfo) -> (hieInfo.hieFilePath, srcPath)) . Map.toList
