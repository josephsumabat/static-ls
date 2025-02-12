module StaticLS.Logger (
  Msg (..),
  Logger,
  LoggerM,
  HasLogger,
  setupLogger,
  noOpLogger,
  HasCallStack,
  CallStack,
  callStack,
  logWith,
  logWithLogger,
  logInfo,
  logError,
  logWarn,
  getLogger,
)
where

import Colog.Core qualified as Colog
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (MaybeT)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text.Encoding qualified as T.Encoding
import GHC.Stack (CallStack, HasCallStack, callStack)
import System.IO qualified as IO

type Logger = LoggerM IO

class HasLogger m where
  getLogger :: m Logger

instance (Monad m, HasLogger m) => HasLogger (MaybeT m) where
  getLogger = lift getLogger

instance HasLogger ((->) Logger) where
  getLogger = id

instance (Monad m) => HasLogger (ReaderT Logger m) where
  getLogger = ask

logWith :: (HasCallStack, HasLogger m, MonadIO m) => Colog.Severity -> Text -> CallStack -> m ()
logWith severity text stack = do
  logger <- getLogger
  logWithLogger logger severity text stack

logWithLogger :: (MonadIO m) => Colog.LogAction IO Msg -> Colog.Severity -> Text -> CallStack -> m ()
logWithLogger logger severity text stack =
  liftIO $ logger Colog.<& Msg {severity, text, stack}

logInfo :: (HasCallStack, HasLogger m, MonadIO m) => Text -> m ()
logInfo text = logWith Colog.Info text callStack

logError :: (HasCallStack, HasLogger m, MonadIO m) => Text -> m ()
logError text = logWith Colog.Error text callStack

logWarn :: (HasCallStack, HasLogger m, MonadIO m) => Text -> m ()
logWarn text = logWith Colog.Warning text callStack

textStderrLogger :: Colog.LogAction IO Text
textStderrLogger = Colog.LogAction $ \msg ->
  B.hPutStr IO.stderr $ T.Encoding.encodeUtf8 (msg <> "\n")

noOpLogger :: Colog.LogAction IO Msg
noOpLogger = Colog.LogAction $ \_msg -> pure ()

logger :: Colog.LogAction IO Msg
logger = Colog.cmap msgToText textStderrLogger

type LoggerM m = Colog.LogAction m Msg

data Msg = Msg
  { severity :: !Colog.Severity
  , stack :: !CallStack
  , text :: !Text
  }
  deriving (Show)

msgToText :: Msg -> Text
msgToText msg = msg.text

-- | Intended to inspect environment variables and then log stuff out
setupLogger :: IO (LoggerM IO)
setupLogger = do
  pure logger
