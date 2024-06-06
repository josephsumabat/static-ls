{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.CodeActions where

import Control.Lens.Operators
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson hiding (Null)
import Data.List qualified as List
import Data.Text
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as LSP
import StaticLS.IDE.CodeActions.AutoImport
import StaticLS.IDE.CodeActions.Types
import StaticLS.Logger (logInfo)
import StaticLS.StaticLsEnv
import StaticLS.Utils
import System.IO
import UnliftIO.Exception qualified as Exception

-- TODO: rename these to static code actions
-- these are the code actions that will always either be present or not present, and are not dynamically generated
globalCodeActions :: [GlobalCodeAction]
globalCodeActions =
  []

-- GlobalCodeAction { run = runCodeAction }

createAutoImportCodeActions :: LSP.TextDocumentIdentifier -> Text -> StaticLsM [LSP.CodeAction]
createAutoImportCodeActions tdi toImport =
  pure
    [ LSP.CodeAction
        { _title = "import " <> toImport
        , _kind = Just LSP.CodeActionKind_QuickFix
        , _diagnostics = Nothing
        , _edit = Nothing
        , _command = Nothing
        , _isPreferred = Nothing
        , _disabled = Nothing
        , _data_ = Just $ toJSON CodeActionMessage {kind = AutoImportActionMessage toImport, tdi = tdi}
        }
    ]

getCodeActions :: StaticLsM [CodeAction]
getCodeActions = undefined

handleCodeAction :: LSP.Handler (LSP.LspT c StaticLsM) LSP.Method_TextDocumentCodeAction
handleCodeAction req resp = do
  _ <- lift $ logInfo "handleCodeAction"
  let params = req._params
  let tdi = params._textDocument
  let range = params._range
  modulesToImport <- lift $ getModulesToImport tdi range._start
  codeActions <- lift $ List.concat <$> mapM (createAutoImportCodeActions tdi) modulesToImport
  resp (Right (LSP.InL (fmap LSP.InR codeActions)))
  pure ()

handleResolveCodeAction :: LSP.Handler (LSP.LspT c StaticLsM) LSP.Method_CodeActionResolve
handleResolveCodeAction req resp = do
  let action = req._params
  liftIO $ do
    hPutStrLn stderr "Resolving code action"
    hPutStrLn stderr $ show action
  let resultSuccessOrThrow res = case res of
        Success a -> pure a
        Error e -> Exception.throwString ("failed to parse json: " ++ e)
  message <- isJustOrThrow "expected data in code action" action._data_
  message <- pure $ fromJSON @CodeActionMessage message
  message <- resultSuccessOrThrow message
  virtualFile <- LSP.getVirtualFile (LSP.toNormalizedUri message.tdi._uri)
  virtualFile <- isJustOrThrow "no virtual file" virtualFile
  let contents = Rope.toText virtualFile._file_text
  (lineNum, lineLength) <-
    T.lines contents
      & List.zip [0 :: Int ..]
      & List.find (\(_i, line) -> T.count (T.pack "where") line == 1)
      & fmap (\(i, line) -> (i, T.length line))
      & isJustOrThrow "could not find where in the header of the module"
  case message.kind of
    AutoImportActionMessage toImport -> do
      liftIO $ hPutStrLn stderr "Resolving auto import action"
      let textDocumentEdit =
            LSP.TextDocumentEdit
              { _textDocument =
                  LSP.OptionalVersionedTextDocumentIdentifier
                    { _uri = message.tdi._uri
                    , _version = LSP.InR LSP.Null
                    }
              , _edits =
                  fmap
                    LSP.InL
                    [ textEditInsert (LSP.Position (fromIntegral lineNum) (fromIntegral lineLength)) (T.pack "\n\nimport " <> toImport)
                    ]
              }
      let workspaceEdit =
            LSP.WorkspaceEdit
              { _changes = Nothing
              , _documentChanges = Just [LSP.InL textDocumentEdit]
              , _changeAnnotations = Nothing
              }
      liftIO $ hPutStrLn stderr ("workspace edit: " ++ show workspaceEdit)
      resp (Right (action & LSP.edit .~ Just workspaceEdit))
      pure ()
    _ -> Exception.throwString "don't know how to resolve this code action"

textEditInsert :: LSP.Position -> Text -> LSP.TextEdit
textEditInsert pos text = LSP.TextEdit (LSP.Range pos pos) text

positionToRange :: LSP.Position -> LSP.Range
positionToRange pos = LSP.Range pos pos
