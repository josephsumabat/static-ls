module Main where

import App.Arguments qualified as App
import App.Configuration
import App.Ghcid (ghcid)
import Control.Error
import Options.Applicative
import StaticLS.Logger
import StaticLS.Server qualified as StaticLS
import StaticLS.StaticEnv.Options (defaultStaticEnvOptions)
import GHC.Debug.Stub

main :: IO ()
main = withGhcDebug $ do
  logger <- StaticLS.Logger.setupLogger
  mFileConfig <- getFileConfig logger
  let jsonOrDefaultOpts = (fromMaybe defaultStaticEnvOptions mFileConfig)
  App.execArgParser jsonOrDefaultOpts >>= \case
    Success (App.GHCIDOptions {args}) -> ghcid args
    argsRes -> do
      staticEnvOpts <- App.handleParseResultWithSuppression jsonOrDefaultOpts argsRes
      _ <- StaticLS.runServer staticEnvOpts logger
      pure ()
