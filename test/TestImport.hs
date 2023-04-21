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

ghcVerDir :: FilePath
ghcVerDir =
#if __GLASGOW_HASKELL__ >= 960
        "ghc961/"
#else
        "ghc944/"
#endif

defaultTestStaticEnvOptions :: StaticEnvOptions
defaultTestStaticEnvOptions =
    StaticEnvOptions
        { optionHieDbPath = Just ("test/TestData/" </> ghcVerDir </> ".hiedb")
        , optionHieFilesPath = Just ("test/TestData/" </> ghcVerDir </> ".hiefiles")
        }

initStaticEnvOpts :: StaticEnvOptions -> IO StaticEnv
initStaticEnvOpts options = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot options
