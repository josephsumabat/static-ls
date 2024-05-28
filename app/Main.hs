module Main where

import qualified App.Arguments as App
import qualified StaticLS.Server as StaticLS
import qualified StaticLS.Logger as StaticLS.Logger

main :: IO ()
main = do
    staticEnvOpts <- App.execArgParser
    logger <- StaticLS.Logger.setupLogger
    _ <- StaticLS.runServer staticEnvOpts logger
    pure ()
