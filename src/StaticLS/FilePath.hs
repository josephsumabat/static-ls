module StaticLS.FilePath (modToFilePath, subRootExtensionFilepathCandidates, subRootExtensionFilepath, getFileModifiedAt) where

import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Data.List qualified as List
import Data.Path (AbsPath, Path (..), RelPath)
import Data.Path qualified as Path
import Data.Time
import GHC.Plugins qualified as GHC
import StaticLS.Maybe
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

subRootExtensionFilepathCandidates :: (MonadIO m) => [AbsPath] -> [AbsPath] -> String -> RelPath -> MaybeT m AbsPath
subRootExtensionFilepathCandidates removeParents candidateParents extension targetPath = do
  candidates <- catMaybes <$> mapM (\candidateParent -> runMaybeT $ subRootExtensionFilepath removeParents candidateParent extension targetPath) candidateParents
  existingCandidates <- filterM (liftIO . Dir.doesFileExist . Path.toFilePath) candidates
  toAlt $ headMay existingCandidates

-- | Substitute a filepath extension and parent directory starting from some root
subRootExtensionFilepath :: (MonadIO m) => [AbsPath] -> AbsPath -> String -> RelPath -> MaybeT m AbsPath
subRootExtensionFilepath removeParents addParent extension targetPath = do
  let newPath = subRootExtensionFilepathUnchecked removeParents addParent extension targetPath
  True <- liftIO $ Dir.doesFileExist newPath.path
  pure newPath

subRootExtensionFilepathUnchecked :: [AbsPath] -> AbsPath -> String -> RelPath -> AbsPath
subRootExtensionFilepathUnchecked removeParents addParent extension targetPath = do
  let
    -- Normalize to absolute paths to drop the prefix
    noPrefixSrcPath =
      List.foldl' (\path parent -> Path.makeRelative parent path) targetPath removeParents
    -- Set the directory path and substitute the file extension
    newPath = addParent Path.</> noPrefixSrcPath Path.-<.> extension
   in
    newPath
