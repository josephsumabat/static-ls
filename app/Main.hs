module Main where

import qualified StaticLS.Server as StaticLS

main :: IO ()
main = do
    StaticLS.runServer
