module Main where

import App.Arguments qualified as App
import App.Configuration
import Control.Error
import StaticLS.Logger
import StaticLS.Server qualified as StaticLS
import StaticLS.StaticEnv.Options (defaultStaticEnvOptions)
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.Process.Typed (proc, runProcess_)
import qualified Data.Text as T
import Control.Monad.Reader (runReaderT)

main :: IO ()
main = do
  logger <- StaticLS.Logger.setupLogger
  mFileConfig <- getFileConfig logger
  App.execArgParser (fromMaybe defaultStaticEnvOptions mFileConfig) >>= \case
    Success (App.GHCIDOptions{args}) -> do
      flip runReaderT logger $ logInfo "starting ghcid"
      flip runReaderT logger $ logInfo (T.pack . show $ args)
      ghcidPwd <- getCurrentDirectory
      let ghcidProcessConfig = proc "ghcid" $ ["-o", "ghcid.txt", "--setup", ":!pwd > " <> ghcidPwd <> "/.ghcid_session"] <> args
      runProcess_ ghcidProcessConfig
    argsRes -> do
      staticEnvOpts <- App.handleParseResultWithSuppression defaultStaticEnvOptions argsRes
      _ <- StaticLS.runServer staticEnvOpts logger
      pure ()
