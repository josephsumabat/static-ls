module Main where

import App.Arguments qualified as App
import App.Ghcid (ghcid)
import App.Configuration
import Control.Error
import StaticLS.Logger
import StaticLS.Server qualified as StaticLS
import StaticLS.StaticEnv.Options (defaultStaticEnvOptions)
import Options.Applicative

main :: IO ()
main = do
  logger <- StaticLS.Logger.setupLogger
  mFileConfig <- getFileConfig logger
  App.execArgParser (fromMaybe defaultStaticEnvOptions mFileConfig) >>= \case
    Success (App.GHCIDOptions{args}) -> ghcid args
    argsRes -> do
      staticEnvOpts <- App.handleParseResultWithSuppression defaultStaticEnvOptions argsRes
      _ <- StaticLS.runServer staticEnvOpts logger
      pure ()
