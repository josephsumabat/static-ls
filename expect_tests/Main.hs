{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.Tasty
import Test.Tasty.Expect
import Data.Text qualified as T

main :: IO ()
main = do
  defaultMainWithIngredients (expectIngredient : defaultIngredients) tests
  pure ()
  
tests :: TestTree
tests =
  testGroup
    "Tests"
    [ test "first" [expect|hello world|] $ do
        pure $ T.pack "hello world"
    ]
