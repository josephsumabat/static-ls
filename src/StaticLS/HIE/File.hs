{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.HIE.File (
    getHieFileFromTdi,
    getHieFile,
    modToHieFile,
    modToSrcFile,
    srcFilePathToHieFilePath,
    hieFilePathToSrcFilePath,
    -- | An alternate way of getting file information by pre-indexing hie files -
    -- far slower on startup and currently unused
    getHieFileMap,
    hieFileMapToSrcMap,
)
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
import Control.Monad ((<=<))
import Control.Monad.IO.Unlift (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT, runMaybeT)
import Data.Bifunctor (first, second)
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Map as Map
import qualified GHC
import qualified GHC.Iface.Ext.Binary as GHC
import qualified GHC.Iface.Ext.Types as GHC
import GHC.Stack (HasCallStack)
import qualified GHC.Types.Name.Cache as GHC
import qualified HieDb
import qualified Language.LSP.Types as LSP
import StaticLS.HIE.File.Except
import StaticLS.Maybe (flatMaybeT, toAlt)
import StaticLS.StaticEnv
import qualified System.Directory as Dir
import System.FilePath ((-<.>), (</>))

type SrcFilePath = FilePath

-- | Retrieve a hie info from a lsp text document identifier
getHieFileFromTdi :: (HasStaticEnv m, MonadIO m) => LSP.TextDocumentIdentifier -> MaybeT m GHC.HieFile
getHieFileFromTdi = exceptToMaybeT . getHieFile <=< tdiToHieFilePath

tdiToHieFilePath :: (HasStaticEnv m, MonadIO m) => LSP.TextDocumentIdentifier -> MaybeT m HieFilePath
tdiToHieFilePath = srcFilePathToHieFilePath <=< (MaybeT . pure . LSP.uriToFilePath . (._uri))

-- | Retrieve an hie file from a module name
modToHieFile :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> MaybeT m GHC.HieFile
modToHieFile = exceptToMaybeT . getHieFile <=< modToHieFilePath

-- | Retrieve a src file from a module name
modToSrcFile :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> MaybeT m SrcFilePath
modToSrcFile = hieFilePathToSrcFilePath <=< modToHieFilePath

{- | Fetch a src file from an hie file, checking hiedb but falling back on a file manipulation method
if not indexed
-}
srcFilePathToHieFilePath :: (HasStaticEnv m, MonadIO m) => SrcFilePath -> MaybeT m HieFilePath
srcFilePathToHieFilePath srcPath =
    srcFilePathToHieFilePathHieDb srcPath
        <|> srcFilePathToHieFilePathFromFile srcPath

-- | Fetch an hie file from a src file
hieFilePathToSrcFilePath :: (HasStaticEnv m, MonadIO m) => HieFilePath -> MaybeT m SrcFilePath
hieFilePathToSrcFilePath hiePath =
  hieFilePathToSrcFilePathHieDb hiePath
    <|> hieFilePathToSrcFilePathFromFile hiePath

-----------------------------------------------------------------------------------
-- Primitive functions for looking up hie information
-----------------------------------------------------------------------------------

-- | Retrieve an hie file from a hie filepath
getHieFile :: (HasCallStack, HasStaticEnv m, MonadIO m) => HieFilePath -> ExceptT HieFileReadException m GHC.HieFile
getHieFile hieFilePath = do
    staticEnv <- getStaticEnv
    -- Attempt to read valid hie file version
    -- NOTE: attempting to override an incorrect header and read an hie file
    -- seems to cause infinite hangs. TODO: explore why?
    result <-
        liftIO
            ( fmap
                (first HieFileVersionException)
                (GHC.readHieFileWithVersion ((== GHC.hieVersion) . fst) staticEnv.nameCache hieFilePath)
                `catch` (\(_ :: SomeException) -> pure . Left $ HieFileReadException)
            )
    ExceptT $ pure (second GHC.hie_file_result result)

-----------------------------------------------------------------------------------
-- HieDb Method of file lookups - requires hiedb to be indexed using --src-base-dirs from 0.4.4.0
-----------------------------------------------------------------------------------

srcFilePathToHieFilePathHieDb :: (HasStaticEnv m, MonadIO m) => SrcFilePath -> MaybeT m HieFilePath
srcFilePathToHieFilePathHieDb srcPath = do
    absSrcPath <- liftIO $ Dir.makeAbsolute srcPath
    Just hieModRow <- runHieDbMaybeT $ \hieDb -> do
        HieDb.lookupHieFileFromSource hieDb absSrcPath
    pure $ HieDb.hieModuleHieFile hieModRow

hieFilePathToSrcFilePathHieDb :: (HasStaticEnv m, MonadIO m) => SrcFilePath -> MaybeT m HieFilePath
hieFilePathToSrcFilePathHieDb hiePath = do
    absHiePath <- liftIO $ Dir.makeAbsolute hiePath
    Just hieModRow <- runHieDbMaybeT $ \hieDb -> do
          HieDb.lookupHieFileFromSource hieDb absHiePath
    toAlt . HieDb.modInfoSrcFile $ HieDb.hieModInfo hieModRow

modToHieFilePath :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> MaybeT m HieFilePath
modToHieFilePath modName =
    flatMaybeT $ runHieDbMaybeT $ \hieDb ->
        runMaybeT $ do
            Right unitId <- liftIO (HieDb.resolveUnitId hieDb modName)
            Just hieModRow <- liftIO $ HieDb.lookupHieFile hieDb modName unitId
            pure $ hieModRow.hieModuleHieFile

-----------------------------------------------------------------------------------
-- File/Directory method for getting hie files - faster but somewhat "hacky"
-- Useful as a fallback
-----------------------------------------------------------------------------------

-- TODO: make this configurable (use cabal?)
srcDirs :: [FilePath]
srcDirs = ["src/", "lib/", "app/", "test/"]

hieFilePathToSrcFilePathFromFile :: (HasStaticEnv m, MonadIO m) => HieFilePath -> MaybeT m SrcFilePath
hieFilePathToSrcFilePathFromFile hiePath = do
    hieFile <- exceptToMaybeT $ getHieFile hiePath
    liftIO $ Dir.makeAbsolute hieFile.hie_hs_file

{- | Retrieve a hie file path from a src path

Substitutes the src directory with the hie directory and the src file extension with
the hie file extension. Fragile, but works well in practice.

Presently necessary because hiedb does not currently index the hs_src file location
in the `mods` table
-}
srcFilePathToHieFilePathFromFile :: (HasStaticEnv m, MonadIO m) => SrcFilePath -> MaybeT m HieFilePath
srcFilePathToHieFilePathFromFile srcPath = do
    staticEnv <- getStaticEnv
    absoluteRoot <- liftIO $ Dir.makeAbsolute staticEnv.wsRoot
    let hieDir = staticEnv.hieFilesPath
        absoluteHieDir = absoluteRoot </> hieDir
        absoluteSrcDirs = (absoluteRoot </>) <$> srcDirs
    absoluteSrcPath <- liftIO $ Dir.makeAbsolute srcPath

    -- Drop all src directory prefixes
    let noPrefixSrcPath =
            List.foldl' (flip List.dropPrefix) absoluteSrcPath absoluteSrcDirs
        -- Set the hie directory path and substitute the file extension
        hiePath = absoluteHieDir </> noPrefixSrcPath -<.> ".hie"
    True <- liftIO $ Dir.doesFileExist hiePath
    pure hiePath

-----------------------------------------------------------------------------------
-- Map index method for getting hie files - too slow in practice on startup but makes
-- finding references for functions that are used a lot much faster
-----------------------------------------------------------------------------------
data HieInfo = HieInfo
    { hieFilePath :: HieFilePath
    , hieFile :: GHC.HieFile
    }

getHieFileMap :: FilePath -> HieFilePath -> IO (Map.Map SrcFilePath HieInfo)
getHieFileMap wsroot hieDir = do
    let hieFullPath = wsroot </> hieDir
    hieFilePaths <- HieDb.getHieFilesIn hieFullPath
    nameCache <- GHC.initNameCache 'a' []
    srcPathHieInfoPairs <- mapM (srcFileToHieFileInfo nameCache) hieFilePaths

    pure $ Map.fromList srcPathHieInfoPairs
  where
    srcFileToHieFileInfo :: GHC.NameCache -> HieFilePath -> IO (SrcFilePath, HieInfo)
    srcFileToHieFileInfo nameCache hieFilePath = do
        hieFileResult <- GHC.readHieFile nameCache hieFilePath
        absSrcFilePath <- Dir.makeAbsolute hieFileResult.hie_file_result.hie_hs_file
        absHieFilePath <- Dir.makeAbsolute hieFilePath
        let hieInfo =
                HieInfo
                    { hieFilePath = absHieFilePath
                    , hieFile = hieFileResult.hie_file_result
                    }
        pure (absSrcFilePath, hieInfo)

hieFileMapToSrcMap :: Map.Map SrcFilePath HieInfo -> Map.Map HieFilePath SrcFilePath
hieFileMapToSrcMap =
    Map.fromList . fmap (\(srcPath, hieInfo) -> (hieInfo.hieFilePath, srcPath)) . Map.toList
