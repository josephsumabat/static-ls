module StaticLS.IDE.Format where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy qualified as BL
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.Function ((&))
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text.Encoding qualified as T.Encoding
import System.Process.Typed qualified as Process
import UnliftIO.Exception qualified as Exception
import Data.Path (AbsPath)
import qualified Data.Path as Path

format :: (MonadIO m) => AbsPath -> Rope -> Text -> m Edit
format path sourceRope source = do
  let len = Rope.length sourceRope
  let sourceBs = T.Encoding.encodeUtf8 source
  let stdin = Process.byteStringInput (BL.fromStrict sourceBs)
  let proc =
        Process.proc "fourmolu" ["--stdin-input-file", Path.toFilePath path]
          & Process.setStdin stdin
  stdoutBs <- Process.readProcessStdout_ proc
  formattedSource <- Exception.evaluate $ T.Encoding.decodeUtf8 (BL.toStrict stdoutBs)
  let edit = Edit.replace (Range (Pos 0) (Pos len)) formattedSource
  pure edit
