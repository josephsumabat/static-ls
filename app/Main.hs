module Main where

import App.Arguments
import Options.Applicative
import qualified StaticLS.Server as StaticLS
import StaticLS.StaticEnv.Options

main :: IO ()
main = do
    staticEnvOpts <- execArgParser
    _ <- StaticLS.runServer staticEnvOpts
    pure ()
