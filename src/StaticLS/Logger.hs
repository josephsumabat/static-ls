module StaticLS.Logger (
    Msg (..),
    LoggerM,
    setupLogger,
    noOpLogger,
    HasCallStack,
    CallStack,
    callStack,
)
where

import Colog.Core qualified as Colog
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import GHC.Stack (CallStack, HasCallStack, callStack)
import System.IO qualified as IO

textStderrLogger :: Colog.LogAction IO Text
textStderrLogger = Colog.LogAction $ \msg ->
    B.hPutStr IO.stderr $ T.Encoding.encodeUtf8 (msg <> "\n")

noOpLogger :: Colog.LogAction IO Msg
noOpLogger = Colog.LogAction $ \msg -> pure ()

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
msgToText msg = T.pack $ show msg

-- | Intended to inspect environment variables and then log stuff out
setupLogger :: IO (LoggerM IO)
setupLogger = do
    pure logger
