module StaticLS.FilePath (modToFilePath, subRootExtensionFilepath) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.List qualified as List
import Data.Path (AbsPath, OsString, Path (..), RelPath)
import Data.Path qualified as Path
import GHC.Plugins qualified as GHC
import StaticLS.SrcFiles
import System.Directory.OsPath qualified as DirOs
import System.FilePath ((-<.>))

modToFilePath :: GHC.ModuleName -> String -> RelPath
modToFilePath modName ext =
  Path.filePathToRel (GHC.moduleNameSlashes modName -<.> ext)

-- | Substitute a filepath extension and parent directory starting from some root
subRootExtensionFilepath :: (MonadIO m) => AbsPath -> AbsPath -> OsString -> AbsPath -> MaybeT m AbsPath
subRootExtensionFilepath wsRoot parent extension srcPath =
  do
    -- absoluteRoot <- liftIO $ Dir.makeAbsolute wsRoot
    let absoluteSrcDirs = (wsRoot Path.</>) <$> srcDirs
    -- absoluteSrcPath <- liftIO $ Dir.makeAbsolute srcPath
    -- Normalize to absolute paths to drop the prefix
    let noPrefixSrcPath =
          List.foldl' (\path absSrcDir -> Path.makeRelative absSrcDir path) (Path.absToRel srcPath) absoluteSrcDirs
        -- Set the directory path and substitute the file extension
        newPath = parent Path.</> noPrefixSrcPath Path.-<.> extension
    True <- liftIO $ DirOs.doesFileExist newPath.path
    pure newPath
