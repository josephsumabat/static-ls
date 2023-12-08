module StaticLS.FilePath (modToFilePath, subRootExtensionFilepath) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified GHC.Plugins as GHC
import StaticLS.SrcFiles
import qualified System.Directory as Dir
import System.FilePath ((-<.>), (</>))

modToFilePath :: GHC.ModuleName -> String -> FilePath
modToFilePath modName ext =
    GHC.moduleNameSlashes modName -<.> ext

-- | Substitute a filepath extension and parent directory starting from some root
subRootExtensionFilepath :: (MonadIO m) => FilePath -> FilePath -> String -> FilePath -> MaybeT m FilePath
subRootExtensionFilepath wsRoot parent extension srcPath =
    do
        absoluteRoot <- liftIO $ Dir.makeAbsolute wsRoot
        let absoluteSrcDirs = (absoluteRoot </>) <$> srcDirs
        absoluteSrcPath <- liftIO $ Dir.makeAbsolute srcPath
        -- Normalize to absolute paths to drop the prefix
        let noPrefixSrcPath =
                List.foldl' (flip List.dropPrefix) absoluteSrcPath absoluteSrcDirs
            -- Set the directory path and substitute the file extension
            newPath = absoluteRoot </> parent </> noPrefixSrcPath -<.> extension
        True <- liftIO $ Dir.doesFileExist newPath
        pure newPath
