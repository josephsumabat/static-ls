module StaticLS.Logger
(
  Msg(..),
  LoggerM,
  setupLogger,
  HasCallStack,
  CallStack,
  callStack,
)
where

import qualified Colog.Core as Colog
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import qualified Data.ByteString as B
import GHC.Stack (CallStack, HasCallStack, callStack)
import qualified System.IO as IO

textStderrLogger :: Colog.LogAction IO Text
textStderrLogger = Colog.LogAction $ \msg ->
  B.hPutStr IO.stderr $ T.Encoding.encodeUtf8 (msg <> "\n")

logger :: Colog.LogAction IO Msg
logger = Colog.cmap msgToText textStderrLogger

type LoggerM m = Colog.LogAction m Msg

data Msg = Msg {
  severity :: !Colog.Severity,
  stack :: !CallStack,
  text :: !Text
  } deriving (Show)

msgToText :: Msg -> Text
msgToText msg = T.pack $ show msg

-- | Intended to inspect environment variables and then log stuff out
setupLogger :: IO (LoggerM IO)
setupLogger = do
  pure logger
