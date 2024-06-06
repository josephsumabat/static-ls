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
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Set qualified as Set
import GHC qualified
import GHC.Iface.Binary qualified as GHC
import GHC.Platform qualified as GHC
import GHC.Platform.Profile qualified as GHC
import GHC.Types.Name.Cache qualified as GHC
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.FilePath
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.StaticEnv

data HiException
  = HiIOException IOException
  | HiGhcException GHC.GhcException
  | HiOtherException SomeException
  deriving (Show)

instance Exception HiException

getModIfaceFromTdi :: (HasStaticEnv m, MonadIO m) => LSP.TextDocumentIdentifier -> MaybeT m GHC.ModIface
getModIfaceFromTdi = readHiFile <=< (fmap Path.toFilePath . tdiToHiFilePath)

tdiToHiFilePath :: (HasStaticEnv m, MonadIO m) => LSP.TextDocumentIdentifier -> MaybeT m AbsPath
tdiToHiFilePath = srcFilePathToHiFilePath <=< (MaybeT . pure . ProtoLSP.uriToAbsPath . (._uri))

modToHiFile :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> MaybeT m AbsPath
modToHiFile modName = do
  staticEnv <- getStaticEnv
  let hiFiles = staticEnv.hiFilesPath
  pure $ hiFiles Path.</> modToFilePath modName ".hi"

readHiFile :: (MonadIO m) => FilePath -> MaybeT m GHC.ModIface
readHiFile = exceptToMaybeT . readHiFileExceptT

-- | Only supports 64 bit platforms
readHiFileExceptT :: (MonadIO m) => FilePath -> ExceptT HiException m GHC.ModIface
readHiFileExceptT filePath = do
  nameCache <- liftIO $ GHC.initNameCache 'a' []
  ExceptT $
    liftIO $
      ( Right
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
        `catch` (pure . Left . HiIOException)
        `catch` (pure . Left . HiGhcException)
        `catch` (pure . Left . HiOtherException)

srcFilePathToHiFilePath :: (HasStaticEnv m, MonadIO m) => AbsPath -> MaybeT m AbsPath
srcFilePathToHiFilePath srcPath = do
  staticEnv <- getStaticEnv
  subRootExtensionFilepath staticEnv.wsRoot staticEnv.hiFilesPath ".hi" srcPath
