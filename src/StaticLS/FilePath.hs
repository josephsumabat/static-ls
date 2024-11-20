module StaticLS.FilePath (modToFilePath, subRootExtensionFilepath, getFileModifiedAt) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.List qualified as List
import Data.Path (AbsPath, Path (..), RelPath)
import Data.Path qualified as Path
import Data.Time
import GHC.Plugins qualified as GHC
import StaticLS.SrcFiles
import System.Directory qualified as Dir
import System.FilePath ((-<.>))
import System.IO.Error

getFileModifiedAt :: (MonadIO m) => AbsPath -> MaybeT m UTCTime
getFileModifiedAt path = do
  MaybeT $
    liftIO $
      (Just <$> Dir.getModificationTime (Path.toFilePath path))
        `catchIOError` const (pure Nothing)

modToFilePath :: GHC.ModuleName -> String -> RelPath
modToFilePath modName ext =
  Path.filePathToRel (GHC.moduleNameSlashes modName -<.> ext)

-- | Substitute a filepath extension and parent directory starting from some root
subRootExtensionFilepath :: (MonadIO m) => AbsPath -> AbsPath -> String -> AbsPath -> MaybeT m AbsPath
subRootExtensionFilepath wsRoot parent extension srcPath = do
  let absoluteSrcDirs = (wsRoot Path.</>) <$> srcDirs
  -- Normalize to absolute paths to drop the prefix
  let noPrefixSrcPath =
        List.foldl' (\path absSrcDir -> Path.makeRelative absSrcDir path) (Path.absToRel srcPath) absoluteSrcDirs
      -- Set the directory path and substitute the file extension
      newPath = parent Path.</> noPrefixSrcPath Path.-<.> extension
  True <- liftIO $ Dir.doesFileExist newPath.path
  pure newPath
