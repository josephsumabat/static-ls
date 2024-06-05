{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Text qualified as T
import Language.Haskell.Lexer qualified as Lexer
import Spec qualified
import Test.Hspec.Formatters
import Test.Hspec.Runner
import Test.Tasty
import Test.Tasty.Expect

main :: IO ()
main = do
  hspecWith defaultConfig {configFormatter = Just progress} Spec.spec

-- defaultMainWithIngredients (expectIngredient : defaultIngredients) tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ test "" [expect||] $ do
        pure $ T.pack $ show $ Lexer.lexerPass0 "module Main where\nmain :: IO ()"
    ]
