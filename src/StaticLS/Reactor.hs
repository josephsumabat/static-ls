module StaticLS.Reactor where

import Control.Monad qualified as Monad
import Control.Monad.Reader
import Data.Text qualified as T
import Language.LSP.Server qualified as LSP
import StaticLS.Logger
import StaticLS.Monad
import UnliftIO.Concurrent qualified as Conc
import UnliftIO.Exception qualified as Exception

type LspConfig = ()

data ReactorMsg where
  ReactorMsgAct :: StaticLsM () -> ReactorMsg
  ReactorMsgRequest :: StaticLsM a -> Conc.MVar a -> ReactorMsg
  ReactorMsgLspAct :: LSP.LspT LspConfig StaticLsM () -> ReactorMsg

reactor :: Conc.Chan ReactorMsg -> LSP.LanguageContextEnv LspConfig -> LoggerM IO -> StaticLsM ()
reactor chan lspEnv _logger = do
  Monad.forever do
    msg <- liftIO $ Conc.readChan chan
    Exception.catchAny
      ( case msg of
          ReactorMsgAct act -> act
          ReactorMsgRequest act resp -> do
            a <- act
            Conc.putMVar resp a
          ReactorMsgLspAct act -> do
            LSP.runLspT lspEnv act
      )
      ( \e -> do
          logError $ "Error in reactor: " <> T.pack (show e)
      )
