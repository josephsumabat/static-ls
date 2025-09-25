module StaticLS.IDE.CodeActions.Utils where

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP

insertAt :: LSP.UInt -> LSP.UInt -> LSP.Range
insertAt line col =
  let p = LSP.Position line col
   in LSP.Range p p

insertBelow :: LSP.Range -> LSP.Range
insertBelow (LSP.Range start _) =
  insertAt (start._line + 1) 0

indentation :: LSP.Range -> T.Text
indentation (LSP.Range start _) =
  T.replicate (fromIntegral start._character + 4) " "

prefer :: LSP.CodeAction -> LSP.CodeAction
prefer action =
  action {LSP._isPreferred = Just True}

quickFix ::
  LSP.TextDocumentIdentifier ->
  LSP.Diagnostic ->
  T.Text ->
  LSP.Range ->
  T.Text ->
  LSP.CodeAction
quickFix tdi diag title range newText =
  LSP.CodeAction
    { _title = title
    , _kind = Just LSP.CodeActionKind_QuickFix
    , _diagnostics = Just [diag]
    , _edit = Just wsEdit
    , _isPreferred = Nothing
    , _command = Nothing
    , _disabled = Nothing
    , _data_ = Nothing
    }
 where
  wsEdit = LSP.WorkspaceEdit Nothing (Just [LSP.InL txtDocEdit]) Nothing
  txtDocEdit = LSP.TextDocumentEdit txtDoc [LSP.InL $ LSP.TextEdit range newText]
  txtDoc = LSP.OptionalVersionedTextDocumentIdentifier tdi._uri (LSP.InR LSP.Null)
