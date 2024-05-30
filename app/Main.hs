module Main where

import App.Arguments qualified as App
import StaticLS.Logger qualified as StaticLS.Logger
import StaticLS.Server qualified as StaticLS

main :: IO ()
main = do
    staticEnvOpts <- App.execArgParser
    logger <- StaticLS.Logger.setupLogger
    _ <- StaticLS.runServer staticEnvOpts logger
    pure ()
