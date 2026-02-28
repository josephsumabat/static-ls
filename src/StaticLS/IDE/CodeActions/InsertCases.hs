module StaticLS.IDE.CodeActions.InsertCases where

import StaticLS.IDE.CodeActions.Utils

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP

codeAction :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> [T.Text] -> LSP.CodeAction
codeAction = insertCases

insertCases :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> [T.Text] -> LSP.CodeAction
insertCases tdi diag pats =
  let spaces = indentation diag._range
      rng = insertBelow diag._range
      cases = foldMap (\pat -> spaces <> pat <> " -> _") pats
   in prefer $ quickFix tdi diag "Insert cases." rng cases
