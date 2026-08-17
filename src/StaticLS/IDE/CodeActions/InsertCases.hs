module StaticLS.IDE.CodeActions.InsertCases where

import StaticLS.IDE.CodeActions.Utils

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP

codeAction :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> [T.Text] -> Int -> LSP.CodeAction
codeAction = insertCases

insertCases :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> [T.Text] -> Int -> LSP.CodeAction
insertCases tdi diag pats leadingSpaces =
  let spaces = T.replicate (leadingSpaces + 4) " "
      rng = insertBelow diag._range
      cases = foldMap (\pat -> spaces <> pat <> " -> _\n") pats
   in prefer $ quickFix tdi diag "Insert missing cases" rng cases
