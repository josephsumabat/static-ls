{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.CodeActions.AutoImportSpec (spec) where

import Data.Function ((&))
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Path qualified as Path
import NeatInterpolation
import StaticLS.IDE.CodeActions.AutoImport qualified as CodeActions.AutoImport
import StaticLS.IDE.CodeActions.TestUtils qualified as CodeActions.TestUtils
import StaticLS.Utils (isJustOrThrowS)
import Test.Hspec
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
              putStrLn "first"
              pure ()
            |]
          )
        ,
          ( "second.hs"
          , [trimming|
            module Second where

            second :: IO ()
            second = do
              putStrLn "second"
              @0first
              pure ()
            |]
          )
        ]
      )
      ( \dir sources -> do
          let second = dir Path.</> "second.hs"
          (_, placeholders) <- Map.lookup second sources & isJustOrThrowS ""
          pos <- IntMap.lookup 0 placeholders & isJustOrThrowS ""
          let expected =
                [trimming|
          module Second where
          
          import First

          second :: IO ()
          second = do
            putStrLn "second"
            first
            pure ()
          |]

          CodeActions.TestUtils.checkCodeAction
            second
            pos
            CodeActions.AutoImport.codeAction
            (Just (expected, (\case (x : _) -> Just x; [] -> Nothing)))

          pure ()
      )
    pure @IO ()
  pure ()
