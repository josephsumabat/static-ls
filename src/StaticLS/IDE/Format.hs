module StaticLS.IDE.Format (format) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy qualified as BL
import Data.Diff qualified as Diff
import Data.Edit (Edit)
import Data.Function ((&))
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Text (Text)
import Data.Text.Encoding qualified as T.Encoding
import System.Process.Typed qualified as Process
import UnliftIO.Exception qualified as Exception

format :: (MonadIO m) => AbsPath -> Text -> FilePath -> m Edit
format path source formatCommand = do
  let sourceBs = T.Encoding.encodeUtf8 source
  let stdin = Process.byteStringInput (BL.fromStrict sourceBs)
  let proc =
        Process.proc formatCommand ["--stdin-input-file", Path.toFilePath path]
          & Process.setStdin stdin
  stdoutBs <- Process.readProcessStdout_ proc
  formattedSource <- Exception.evaluate $ T.Encoding.decodeUtf8 (BL.toStrict stdoutBs)
  let edit = Diff.diffEdit source formattedSource
  pure edit
