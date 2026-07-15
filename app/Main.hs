module Main where

import App.Arguments qualified as App
import App.Configuration
import App.Ghcid (ghcid)
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Error
import Control.Exception (throwTo)
import Control.Monad (replicateM_, when)
import Options.Applicative
import StaticLS.Logger
import StaticLS.Server qualified as StaticLS
import StaticLS.StaticEnv.Options (StaticEnvOptions (..), defaultStaticEnvOptions)
import System.Exit (ExitCode (ExitSuccess))

main :: IO ()
main = do
  logger <- StaticLS.Logger.setupLogger
  mFileConfig <- getFileConfig logger
  let jsonOrDefaultOpts = (fromMaybe defaultStaticEnvOptions mFileConfig)
  App.execArgParser jsonOrDefaultOpts >>= \case
    Success (App.GHCIDOptions {args}) -> ghcid args
    argsRes -> do
      staticEnvOpts <- App.handleParseResultWithSuppression jsonOrDefaultOpts argsRes
      scheduleRestart staticEnvOpts.restartIntervalMinutes
      _ <- StaticLS.runServer staticEnvOpts logger
      pure ()

-- | Mitigation for a known memory leak in long-running sessions: after the
-- configured number of minutes, exit the process so the LSP client relaunches
-- static-ls (which frees the leaked memory). Exiting is done by throwing
-- 'ExitSuccess' to the main thread so it unwinds and terminates cleanly.
-- A non-positive interval disables the timed restart.
scheduleRestart :: Int -> IO ()
scheduleRestart minutes =
  when (minutes > 0) $ do
    mainTid <- myThreadId
    _ <- forkIO $ do
      -- Sleep in one-minute chunks to avoid overflowing threadDelay's Int microsecond argument.
      replicateM_ minutes (threadDelay oneMinuteMicros)
      throwTo mainTid ExitSuccess
    pure ()
 where
  oneMinuteMicros = 60 * 1000000
