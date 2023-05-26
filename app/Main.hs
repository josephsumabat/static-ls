module Main where

import qualified App.Arguments as App
import qualified StaticLS.Server as StaticLS

main :: IO ()
main = do
    staticEnvOpts <- App.execArgParser
    _ <- StaticLS.runServer staticEnvOpts
    pure ()
