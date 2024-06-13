{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.CodeActions.AutoImportSpec (spec) where

import NeatInterpolation
import Test.Hspec
import TestImport qualified
import TestImport.Compilation qualified

spec :: Spec
spec = do
  it "" do
    TestImport.Compilation.setupCompilation
      "AutoImportSpec"
      ( [
          ( "first.hs"
          , [trimming|
            module First where

            first :: IO ()
            first = do
              putStrLn "first"
              pure ()
            |]
          )
        ,
          ( "second.hs"
          , [trimming|
            module Second where

            import First

            second :: IO ()
            second = do
              first
              putStrLn "second"
              pure ()
            |]
          )
        ]
      )
      ( \sources ->
          pure ()
      )
    pure @IO ()
  pure ()
