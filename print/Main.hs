module Main where

import System.Environment (getArgs)
import Data.Text.IO qualified as T.IO
import StaticLS.HieView.View qualified as HieView
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

main :: IO ()
main = do
  [file] <- getArgs
  hieFile <- runMaybeT $ HieView.readFile file
  case hieFile of
    Nothing -> putStrLn "No hie file found"
    Just hieFile ->
      T.IO.putStrLn $ HieView.pPrintColor hieFile
  pure ()