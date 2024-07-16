{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.Tasty
import Test.Tasty.Expect
import Data.Text qualified as T
import qualified HirTest

main :: IO ()
main = do
  defaultMainWithIngredients (expectIngredient : defaultIngredients) tests
  pure ()
  
tests :: TestTree
tests =
  testGroup
    "Tests"
    [ HirTest.tests
    ]
