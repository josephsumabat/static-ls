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
  it "Auto imports tests" do
    let firstRelPath = "src/First.hs"
        secondRelPath = "src/Second.hs"
    TestImport.Compilation.setupCompilation
      "AutoImportSpec"
      ( [
          ( firstRelPath
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
          ( secondRelPath
          , [trimming|
            module Second where

            second :: IO ()
            second = do
              putStrLn "second"
              @0first
              pure ()

            somethingElse :: IO ()
            somethingElse = @1second
            |]
          )
        ]
      )
      ( \dir sources -> do
          let second = dir Path.</> secondRelPath
          (_, placeholders) <- Map.lookup second sources & isJustOrThrowS "placeholders not found"
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

          somethingElse :: IO ()
          somethingElse = second
          |]

          CodeActions.TestUtils.checkCodeAction
            second
            pos
            CodeActions.AutoImport.codeAction
            (Just (expected, (\case (x : _) -> Just x; [] -> Nothing)))

          secondPos <- IntMap.lookup 1 placeholders & isJustOrThrowS ""

          -- Expect no assist when function is in same module
          CodeActions.TestUtils.checkCodeAction
            second
            secondPos
            CodeActions.AutoImport.codeAction
            Nothing

          pure ()
      )
    pure @IO ()
  pure ()
