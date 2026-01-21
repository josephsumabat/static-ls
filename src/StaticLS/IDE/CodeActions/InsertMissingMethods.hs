module StaticLS.IDE.CodeActions.InsertMissingMethods where

import StaticLS.IDE.CodeActions.Utils

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP

codeAction :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> [T.Text] -> LSP.CodeAction
codeAction = insertMissingMethods

insertMissingMethods :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> [T.Text] -> LSP.CodeAction
insertMissingMethods tdi diag methods =
  let rng = insertBelow diag._range
      txt = T.concat ["    ", T.intercalate " = _\n    " methods, "\n"]
   in prefer $ quickFix tdi diag "Insert missing methods." rng txt
