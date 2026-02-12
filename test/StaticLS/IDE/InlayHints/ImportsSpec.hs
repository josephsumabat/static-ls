{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.InlayHints.ImportsSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Path qualified as Path
import Data.Text (Text)
import Data.Text qualified as T
import NeatInterpolation
import StaticLS.IDE.InlayHints.Imports qualified as Imports
import StaticLS.IDE.InlayHints.Types (InlayHint (..), InlayHintLabelPart (..))
import StaticLS.StaticEnv.Options qualified as StaticEnv.Options
import Test.Hspec
import TestImport.Compilation qualified

spec :: Spec
spec = do
  describe "Import inlay hints" $ do
    it "shows imported symbols for imports without explicit lists" $ do
      let libPath = "src/Lib.hs"
          mainPath = "src/Main.hs"
      TestImport.Compilation.setupCompilation
        "ImportsSpec-basic"
        [
          ( libPath
          , [trimming|
            module Lib where

            myFunction :: Int -> Int
            myFunction x = x + 1

            anotherFunction :: String -> String
            anotherFunction s = s ++ "!"
            |]
          )
        ,
          ( mainPath
          , [trimming|
            module Main where

            import Lib

            main :: IO ()
            main = do
              let x = myFunction 5
              putStrLn (anotherFunction "hello")
            |]
          )
        ]
        \dir _sources -> do
          let mainAbsPath = dir Path.</> mainPath
          hints <- Imports.getInlayHints mainAbsPath StaticEnv.Options.defaultStaticEnvOptions
          -- Should have a hint for the Lib import
          liftIO $ length hints `shouldSatisfy` (>= 1)
          -- The hint should contain the imported symbols
          let hintTexts = map getHintText hints
          liftIO $ any (T.isInfixOf "myFunction") hintTexts `shouldBe` True
          liftIO $ any (T.isInfixOf "anotherFunction") hintTexts `shouldBe` True
      pure @IO ()

    it "does not show hints for imports with explicit lists" $ do
      let libPath = "src/Lib.hs"
          mainPath = "src/Main.hs"
      TestImport.Compilation.setupCompilation
        "ImportsSpec-explicit"
        [
          ( libPath
          , [trimming|
            module Lib where

            myFunction :: Int -> Int
            myFunction x = x + 1
            |]
          )
        ,
          ( mainPath
          , [trimming|
            module Main where

            import Lib (myFunction)

            main :: IO ()
            main = print (myFunction 5)
            |]
          )
        ]
        \dir _sources -> do
          let mainAbsPath = dir Path.</> mainPath
          hints <- Imports.getInlayHints mainAbsPath StaticEnv.Options.defaultStaticEnvOptions
          -- Should not have any hints since import has explicit list
          liftIO $ length hints `shouldBe` 0
      pure @IO ()

    it "shows multiple symbols from the same module" $ do
      let libPath = "src/Lib.hs"
          mainPath = "src/Main.hs"
      TestImport.Compilation.setupCompilation
        "ImportsSpec-multi"
        [
          ( libPath
          , [trimming|
            module Lib where

            foo :: Int
            foo = 1

            bar :: Int
            bar = 2

            baz :: Int
            baz = 3
            |]
          )
        ,
          ( mainPath
          , [trimming|
            module Main where

            import Lib

            main :: IO ()
            main = print (foo + bar)
            |]
          )
        ]
        \dir _sources -> do
          let mainAbsPath = dir Path.</> mainPath
          hints <- Imports.getInlayHints mainAbsPath StaticEnv.Options.defaultStaticEnvOptions
          liftIO $ length hints `shouldSatisfy` (>= 1)
          let hintTexts = map getHintText hints
          -- Should contain both foo and bar (but not baz since it's not used)
          liftIO $ any (T.isInfixOf "foo") hintTexts `shouldBe` True
          liftIO $ any (T.isInfixOf "bar") hintTexts `shouldBe` True
      pure @IO ()

    it "handles multiple modules and qualified imports" $ do
      let libAPath = "src/LibA.hs"
          libBPath = "src/LibB.hs"
          mainPath = "src/Main.hs"
      TestImport.Compilation.setupCompilation
        "ImportsSpec-multi-module"
        [
          ( libAPath
          , [trimming|
            module LibA where

            funcA :: Int -> Int
            funcA x = x + 1
            |]
          )
        ,
          ( libBPath
          , [trimming|
            module LibB where

            funcB :: String -> String
            funcB s = s ++ "!"
            |]
          )
        ,
          ( mainPath
          , [trimming|
            module Main where

            import LibA
            import LibB
            import Data.List qualified as L

            main :: IO ()
            main = do
              print (funcA 5)
              putStrLn (funcB "hello")
              print (L.sort [3, 1, 2])
            |]
          )
        ]
        \dir _sources -> do
          let mainAbsPath = dir Path.</> mainPath
          hints <- Imports.getInlayHints mainAbsPath StaticEnv.Options.defaultStaticEnvOptions
          let hintTexts = map getHintText hints
          -- Should have hints for all imports without explicit lists
          liftIO $ any (T.isInfixOf "funcA") hintTexts `shouldBe` True
          liftIO $ any (T.isInfixOf "funcB") hintTexts `shouldBe` True
          liftIO $ any (T.isInfixOf "sort") hintTexts `shouldBe` False
      pure @IO ()

getHintText :: InlayHint -> Text
getHintText hint = case hint.label of
  Left t -> t
  Right parts -> T.concat [v | InlayHintLabelPart v _ _ _ <- parts]
