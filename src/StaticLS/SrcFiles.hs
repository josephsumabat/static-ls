module StaticLS.SrcFiles (
  SrcFilePath,
  srcDirs,
) where

import Data.Path (RelPath)
import Data.Path qualified as Path

type SrcFilePath = FilePath

-- TODO: make this configurable (use cabal?)
srcDirs :: [RelPath]
srcDirs = Path.filePathToRel <$> ["src/", "lib/", "app/", "test/"]
