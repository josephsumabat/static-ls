{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.IDE.CodeActions where

import Data.Text
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.CodeActions.AutoImport
import Data.Aeson.TH
import Language.LSP.Server
import Language.LSP.VFS
import Language.LSP.Protocol.Types hiding (ApplyWorkspaceEditParams(..))
import Language.LSP.Protocol.Message (Method (..), ResponseError (..), SMethod (..), TMessage, TRequestMessage (..))
import Control.Monad.Trans.Class (lift)
import Data.Aeson hiding (Null)
import Control.Monad.IO.Class (liftIO)
import System.IO
import Data.Maybe (fromMaybe)
import qualified Data.Text.Utf16.Rope.Mixed as Rope
import qualified Data.List as List
import qualified Data.Text as T
import Data.Function ((&))

-- TODO: rename these to static code actions
-- these are the code actions that will always either be present or not present, and are not dynamically generated
globalCodeActions :: [GlobalCodeAction]
globalCodeActions =
  [
    -- GlobalCodeAction { run = runCodeAction }
  ]
  
createAutoImportCodeAction :: TextDocumentIdentifier -> Text ->  StaticLs CodeAction
createAutoImportCodeAction tdi toImport =
  pure CodeAction
    { _title = "import " <> toImport
    , _kind = Just CodeActionKind_QuickFix
    , _diagnostics = Nothing
    , _edit = Nothing
    , _command = Nothing
    , _isPreferred = Nothing
    , _disabled = Nothing
    , _data_ = Just $ toJSON CodeActionMessage { kind = AutoImportActionMessage toImport, tdi = tdi }
    }

handleCodeAction :: Handler (LspT c StaticLs) Method_TextDocumentCodeAction
handleCodeAction req resp = do
  let params = req._params
  let tdi = params._textDocument
  let range = params._range
  modulesToImport <- lift $ getModulesToImport tdi (range._start)
  codeActions <- lift $ mapM (createAutoImportCodeAction tdi) modulesToImport
  resp (Right (InL (fmap InR codeActions)))
  pure ()
  
handleResolveCodeAction :: Handler (LspT c StaticLs) Method_CodeActionResolve
handleResolveCodeAction req resp = do
  let action = req._params
  liftIO $ do
    hPutStrLn stderr "Resolving code action"
    hPutStrLn stderr $ show action
  case action._data_ of
    Nothing ->
      liftIO $ hPutStrLn stderr "Error: No data in code action"
    Just jsonMessage -> do
      let message = fromJSON @CodeActionMessage jsonMessage
      case message of
        Success message -> do
          virtualFile <- getVirtualFile (toNormalizedUri message.tdi._uri)
          virtualFile <- pure $ fromMaybe (error "no virtual file") virtualFile
          let contents = Rope.toText virtualFile._file_text
          let (lineNum, lineLength) =
                T.lines contents
                & List.zip [0 :: Int ..]
                & List.find (\(i, line) -> T.count (T.pack "where") line == 1)
                & fmap (\(i, line) -> (i, T.length line))
                & fromMaybe (error "bruh")
          case message.kind of
            AutoImportActionMessage toImport -> do
              liftIO $ hPutStrLn stderr "Resolving auto import action"
              let textDocumentEdit = TextDocumentEdit
                    { _textDocument = OptionalVersionedTextDocumentIdentifier
                      { _uri = message.tdi._uri
                      , _version = InR Null
                      }
                    , _edits =
                      fmap InL
                        [
                          textEditInsert (Position (fromIntegral lineNum) (fromIntegral lineLength)) (T.pack "\n\nimport " <> toImport)
                        ]
                    }
              let workspaceEdit = WorkspaceEdit
                    { _changes = Nothing
                    , _documentChanges = Just [InL textDocumentEdit]
                    , _changeAnnotations = Nothing
                    }
              liftIO $ hPutStrLn stderr ("workspace edit: " ++ show workspaceEdit)
              resp (Right (action { _edit = Just workspaceEdit }))
              pure ()
            _ -> do
              liftIO $ hPutStrLn stderr "don't know how to resolve this code action"
              pure ()
        _ -> do
          liftIO $ hPutStrLn stderr "could deserialize data"
          pure ()
          
textEditInsert :: Position -> Text -> TextEdit
textEditInsert pos text = TextEdit (Range pos pos) text

positionToRange :: Position -> Range
positionToRange pos = Range pos pos
          
createSimpleWorkspaceEdit :: ()
createSimpleWorkspaceEdit = ()