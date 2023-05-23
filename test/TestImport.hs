{-# LANGUAGE CPP #-}
module TestImport where

import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as StaticEnv
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

initStaticEnv :: IO StaticEnv
initStaticEnv = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot defaultTestStaticEnvOptions

#if __GLASGOW_HASKELL__ >= 906
ghcVerDir :: FilePath
ghcVerDir =
        "ghc961/"
#else
ghcVerDir :: FilePath
ghcVerDir =
        "ghc944/"
#endif

testHieDir :: FilePath
testHieDir = "test/TestData/.hiefiles"

defaultTestStaticEnvOptions :: StaticEnvOptions
defaultTestStaticEnvOptions =
    StaticEnvOptions
        { optionHieDbPath = Just ("test/TestData/" </> ghcVerDir </> ".hiedb")
        , optionHieFilesPath = Just testHieDir
        }

initStaticEnvOpts :: StaticEnvOptions -> IO StaticEnv
initStaticEnvOpts options = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot options
