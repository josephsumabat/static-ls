module StaticLS.IDE.Utils (makeRelativeMaybe, pathToModule) where

import Control.Applicative
import Control.Monad
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Text qualified as T
import StaticLS.Hir qualified as Hir
import StaticLS.Monad
import StaticLS.StaticEnv
import System.FilePath

makeRelativeMaybe :: FilePath -> FilePath -> Maybe FilePath
makeRelativeMaybe base path = do
  let rel = makeRelative base path
  guard $ path /= rel
  pure rel

pathToModule :: AbsPath -> StaticLsM (Maybe Hir.Module)
pathToModule absPath = do
  let fp = Path.toFilePath absPath
  staticEnv <- getStaticEnv
  let srcDirs = staticEnv.srcDirs
  pure $ do
    modPath <- asum ((\srcDir -> makeRelativeMaybe (Path.toFilePath srcDir) fp) <$> srcDirs)
    let (modPathWithoutExt, ext) = splitExtension modPath
    guard $ ext == ".hs"
    let modText = T.replace (T.pack [pathSeparator]) "." (T.pack modPathWithoutExt)
    pure $ Hir.parseModuleFromText modText
