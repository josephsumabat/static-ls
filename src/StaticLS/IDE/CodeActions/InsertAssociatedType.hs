module StaticLS.IDE.CodeActions.InsertAssociatedType where

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.CodeActions.Utils (insertBelow, prefer, quickFix)

codeAction :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> T.Text -> LSP.CodeAction
codeAction = insertAssociatedType

insertAssociatedType :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> T.Text -> LSP.CodeAction
insertAssociatedType tdi diag ty =
  let rng = insertBelow diag._range
      txt = T.concat ["    type ", ty, " = ()\n"]
   in prefer $ quickFix tdi diag "Insert associated type." rng txt
