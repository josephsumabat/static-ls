{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC  #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use hPrint" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.CodeActions where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson hiding (Null)
import Data.Function ((&))
import Data.List qualified as List
import Data.Text
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Protocol.Message (Method (..), TRequestMessage (..))
import Language.LSP.Protocol.Types hiding (ApplyWorkspaceEditParams (..))
import Language.LSP.Server
import Language.LSP.VFS
import StaticLS.IDE.CodeActions.AutoImport
import StaticLS.IDE.CodeActions.Types
import StaticLS.StaticEnv
import System.IO
import UnliftIO.Exception qualified as Exception

-- TODO: rename these to static code actions
-- these are the code actions that will always either be present or not present, and are not dynamically generated
globalCodeActions :: [GlobalCodeAction]
globalCodeActions =
    []

-- GlobalCodeAction { run = runCodeAction }

createAutoImportCodeActions :: TextDocumentIdentifier -> Text -> StaticLs [CodeAction]
createAutoImportCodeActions tdi toImport =
    pure
        [ CodeAction
            { _title = "import " <> toImport
            , _kind = Just CodeActionKind_QuickFix
            , _diagnostics = Nothing
            , _edit = Nothing
            , _command = Nothing
            , _isPreferred = Nothing
            , _disabled = Nothing
            , _data_ = Just $ toJSON CodeActionMessage{kind = AutoImportActionMessage toImport, tdi = tdi}
            }
        ]

handleCodeAction :: Handler (LspT c StaticLs) Method_TextDocumentCodeAction
handleCodeAction req resp = do
    let params = req._params
    let tdi = params._textDocument
    let range = params._range
    modulesToImport <- lift $ getModulesToImport tdi (range._start)
    codeActions <- lift $ List.concat <$> mapM (createAutoImportCodeActions tdi) modulesToImport
    resp (Right (InL (fmap InR codeActions)))
    pure ()

handleResolveCodeAction :: Handler (LspT c StaticLs) Method_CodeActionResolve
handleResolveCodeAction req resp = do
    let action = req._params
    liftIO $ do
        hPutStrLn stderr "Resolving code action"
        hPutStrLn stderr $ show action
    let resultSuccessOrThrow res = case res of
            Success a -> pure a
            Error e -> Exception.throwString ("failed to parse json: " ++ e)
    let isJustOrThrow s m = case m of
            Just a -> pure a
            Nothing -> Exception.throwString s
    message <- isJustOrThrow "expected data in code action" action._data_
    message <- pure $ fromJSON @CodeActionMessage message
    message <- resultSuccessOrThrow message
    virtualFile <- getVirtualFile (toNormalizedUri message.tdi._uri)
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
                    TextDocumentEdit
                        { _textDocument =
                            OptionalVersionedTextDocumentIdentifier
                                { _uri = message.tdi._uri
                                , _version = InR Null
                                }
                        , _edits =
                            fmap
                                InL
                                [ textEditInsert (Position (fromIntegral lineNum) (fromIntegral lineLength)) (T.pack "\n\nimport " <> toImport)
                                ]
                        }
            let workspaceEdit =
                    WorkspaceEdit
                        { _changes = Nothing
                        , _documentChanges = Just [InL textDocumentEdit]
                        , _changeAnnotations = Nothing
                        }
            liftIO $ hPutStrLn stderr ("workspace edit: " ++ show workspaceEdit)
            resp (Right (action{_edit = Just workspaceEdit}))
            pure ()
        _ -> Exception.throwString "don't know how to resolve this code action"

textEditInsert :: Position -> Text -> TextEdit
textEditInsert pos text = TextEdit (Range pos pos) text

positionToRange :: Position -> Range
positionToRange pos = Range pos pos
