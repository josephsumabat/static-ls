module StaticLS.SrcFiles (
    SrcFilePath,
    srcDirs,
) where

type SrcFilePath = FilePath

-- TODO: make this configurable (use cabal?)
srcDirs :: [FilePath]
srcDirs = ["src/", "lib/", "app/", "test/"]
