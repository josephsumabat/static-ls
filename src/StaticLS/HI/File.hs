{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.HI.File (
    readHiFile,
    srcFilePathToHiFilePath,
    getModIfaceFromTdi,
    tdiToHiFilePath,
    modToHiFile,
) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Unlift (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Iface.Binary as GHC
import qualified GHC.Platform as GHC
import qualified GHC.Platform.Profile as GHC
import qualified GHC.Types.Name.Cache as GHC
import qualified Language.LSP.Protocol.Types as LSP
import StaticLS.FilePath
import StaticLS.Maybe
import StaticLS.SrcFiles
import StaticLS.StaticEnv
import System.FilePath ((</>))

getModIfaceFromTdi :: (HasStaticEnv m, MonadIO m) => LSP.TextDocumentIdentifier -> MaybeT m GHC.ModIface
getModIfaceFromTdi = MaybeT . readHiFile <=< tdiToHiFilePath

tdiToHiFilePath :: (HasStaticEnv m, MonadIO m) => LSP.TextDocumentIdentifier -> MaybeT m HiFilePath
tdiToHiFilePath = srcFilePathToHiFilePath <=< (MaybeT . pure . LSP.uriToFilePath . (._uri))

modToHiFile :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> MaybeT m HiFilePath
modToHiFile modName = do
    staticEnv <- getStaticEnv
    hiFiles <- toAlt staticEnv.hiFilesPath
    pure $ staticEnv.wsRoot </> hiFiles </> modToFilePath modName ".hi"

-- | Only supports 64 bit platforms
readHiFile :: (MonadIO m) => FilePath -> m (Maybe GHC.ModIface)
readHiFile filePath = do
    nameCache <- liftIO $ GHC.initNameCache 'a' []
    liftIO $
        ( Just
            <$> GHC.readBinIface
                GHC.Profile
                    { GHC.profilePlatform = GHC.genericPlatform
                    , GHC.profileWays = Set.empty
                    }
                nameCache
                GHC.IgnoreHiWay
                GHC.QuietBinIFace
                filePath
        )
            `catch` (\(_ :: IOException) -> pure Nothing)
            `catch` (\(_ :: GHC.GhcException) -> pure Nothing)
            `catch` (\(_ :: SomeException) -> pure Nothing)

srcFilePathToHiFilePath :: (HasStaticEnv m, MonadIO m) => SrcFilePath -> MaybeT m HiFilePath
srcFilePathToHiFilePath srcPath = do
    staticEnv <- getStaticEnv
    hiFiles <- toAlt staticEnv.hiFilesPath
    let hiDir = staticEnv.wsRoot </> hiFiles
    subRootExtensionFilepath staticEnv.wsRoot hiDir ".hi" srcPath
