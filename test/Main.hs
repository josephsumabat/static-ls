module Main where

import Data.Text qualified as T
import Language.Haskell.Lexer qualified as Lexer
import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = do
  hspecWith defaultConfig Spec.spec
