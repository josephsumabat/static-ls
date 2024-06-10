{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.HIE.File (
  getHieFile,
  modToHieFile,
  modToSrcFile,
  srcFilePathToHieFilePath,
  hieFilePathToSrcFilePath,
  -- | An alternate way of getting file information by pre-indexing hie files -
  -- far slower on startup and currently unused
  getHieFileMap,
  hieFileMapToSrcMap,
  getHieFileFromPath,
)
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
import Control.Monad ((<=<))
import Control.Monad.IO.Unlift (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT, runMaybeT)
import Data.Bifunctor (first, second)
import Data.Map qualified as Map
import Data.Path (AbsPath)
import Data.Path qualified as Path
import GHC qualified
import GHC.Iface.Ext.Binary qualified as GHC
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Types.Name.Cache qualified as GHC
import HieDb qualified
import StaticLS.FilePath
import StaticLS.HIE.File.Except
import StaticLS.HieDb qualified as HieDb
import StaticLS.Maybe (flatMaybeT, toAlt)
import StaticLS.SrcFiles
import StaticLS.StaticEnv
import System.Directory qualified as Dir
import System.FilePath ((</>))

-- | Retrieve a hie info from a lsp text document identifier
-- Returns a Maybe instead of throwing because we want to handle
-- the case when there is no hie file and do something reasonable
-- Most functions that get the file text will throw if the file text is not found
getHieFileFromPath :: (HasStaticEnv m, MonadIO m) => AbsPath -> MaybeT m GHC.HieFile
getHieFileFromPath = (exceptToMaybeT . getHieFile) <=< srcFilePathToHieFilePath

-- uriToHieFilePath :: (HasStaticEnv m, MonadIO m) => LSP.Uri -> MaybeT m AbsPath
-- uriToHieFilePath = srcFilePathToHieFilePath <=< (MaybeT . pure . LSPProto.uriToAbsPath)

-- | Retrieve an hie file from a module name
modToHieFile :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> MaybeT m GHC.HieFile
modToHieFile = exceptToMaybeT . getHieFile <=< modToHieFilePath

-- | Retrieve a src file from a module name
modToSrcFile :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> MaybeT m AbsPath
modToSrcFile = hieFilePathToSrcFilePath <=< modToHieFilePath

-- | Fetch a src file from an hie file, checking hiedb but falling back on a file manipulation method
-- if not indexed
srcFilePathToHieFilePath :: (HasStaticEnv m, MonadIO m) => AbsPath -> MaybeT m AbsPath
srcFilePathToHieFilePath srcPath =
  srcFilePathToHieFilePathFromFile srcPath
    <|> srcFilePathToHieFilePathHieDb srcPath

-- | Fetch an hie file from a src file
hieFilePathToSrcFilePath :: (HasStaticEnv m, MonadIO m) => AbsPath -> MaybeT m AbsPath
hieFilePathToSrcFilePath hiePath = do
  hieFilePathToSrcFilePathHieDb hiePath
    <|> hieFilePathToSrcFilePathFromFile hiePath

-----------------------------------------------------------------------------------
-- Primitive functions for looking up hie information
-----------------------------------------------------------------------------------

-- | Retrieve an hie file from a hie filepath
getHieFile :: (HasCallStack, MonadIO m) => AbsPath -> ExceptT HieFileReadException m GHC.HieFile
getHieFile hieFilePath = do
  -- Attempt to read valid hie file version
  -- NOTE: attempting to override an incorrect header and read an hie file
  -- seems to cause infinite hangs. TODO: explore why?
  nameCache <- liftIO $ GHC.initNameCache 'a' []
  result <-
    liftIO
      ( fmap
          (first HieFileVersionException)
          (GHC.readHieFileWithVersion ((== GHC.hieVersion) . fst) nameCache (Path.toFilePath hieFilePath))
          `catch` (\(_ :: SomeException) -> pure . Left $ HieFileReadException)
      )
  ExceptT $ pure (second GHC.hie_file_result result)

-----------------------------------------------------------------------------------
-- HieDb Method of file lookups - requires hiedb to be indexed using --src-base-dirs from 0.4.4.0
-----------------------------------------------------------------------------------

srcFilePathToHieFilePathHieDb :: (HasCallStack, HasStaticEnv m, MonadIO m) => AbsPath -> MaybeT m AbsPath
srcFilePathToHieFilePathHieDb srcPath = do
  Just hieModRow <- runHieDbMaybeT $ \hieDb -> do
    HieDb.lookupHieFileFromSource hieDb (Path.toFilePath srcPath)
  pure $ Path.unsafeFilePathToAbs $ HieDb.hieModuleHieFile hieModRow

hieFilePathToSrcFilePathHieDb :: (HasStaticEnv m, MonadIO m) => AbsPath -> MaybeT m AbsPath
hieFilePathToSrcFilePathHieDb hiePath = do
  Just hieModRow <- runHieDbMaybeT $ \hieDb -> do
    HieDb.lookupHieFileFromHie hieDb (Path.toFilePath hiePath)
  res <- toAlt $ HieDb.modInfoSrcFile $ HieDb.hieModInfo hieModRow
  Path.filePathToAbs res

modToHieFilePath :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.ModuleName -> MaybeT m AbsPath
modToHieFilePath modName =
  flatMaybeT $ runHieDbMaybeT $ \hieDb ->
    runMaybeT $ do
      Right unitId <- liftIO (HieDb.resolveUnitId hieDb modName)
      Just hieModRow <- liftIO $ HieDb.lookupHieFile hieDb modName unitId
      Path.filePathToAbs hieModRow.hieModuleHieFile

-----------------------------------------------------------------------------------
-- File/Directory method for getting hie files - faster but somewhat "hacky"
-- Useful as a fallback
-----------------------------------------------------------------------------------

hieFilePathToSrcFilePathFromFile :: (HasStaticEnv m, MonadIO m) => AbsPath -> MaybeT m AbsPath
hieFilePathToSrcFilePathFromFile hiePath = do
  hieFile <- exceptToMaybeT $ getHieFile hiePath
  liftIO $ Path.filePathToAbs hieFile.hie_hs_file

-- | Retrieve a hie file path from a src path
--
-- Substitutes the src directory with the hie directory and the src file extension with
-- the hie file extension. Fragile, but works well in practice.
--
-- Presently necessary because hiedb does not currently index the hs_src file location
-- in the `mods` table
srcFilePathToHieFilePathFromFile :: (HasStaticEnv m, MonadIO m) => AbsPath -> MaybeT m AbsPath
srcFilePathToHieFilePathFromFile srcPath = do
  staticEnv <- getStaticEnv
  -- let hieDir = staticEnv.wsRoot </> staticEnv.hieFilesPath
  subRootExtensionFilepath staticEnv.wsRoot staticEnv.hieFilesPath ".hie" srcPath

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
