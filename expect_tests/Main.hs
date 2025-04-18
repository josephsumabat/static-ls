module Main where

import HirTest qualified
import Test.Tasty
import Test.Tasty.Expect

main :: IO ()
main = do
  -- defaultMainWithIngredients (expectIngredient : defaultIngredients) tests
  pure ()

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ HirTest.tests
    ]
