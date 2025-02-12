module Main where

import App.Arguments qualified as App
import App.Configuration
import Control.Error
import StaticLS.Logger
import StaticLS.Server qualified as StaticLS
import StaticLS.StaticEnv.Options (defaultStaticEnvOptions)

main :: IO ()
main = do
  logger <- StaticLS.Logger.setupLogger
  mFileConfig <- getFileConfig logger
  staticEnvOpts <- App.execArgParser (fromMaybe defaultStaticEnvOptions mFileConfig)
  _ <- StaticLS.runServer staticEnvOpts logger
  pure ()
