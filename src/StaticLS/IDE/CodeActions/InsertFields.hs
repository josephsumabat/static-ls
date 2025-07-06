module StaticLS.IDE.CodeActions.InsertFields where

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.CodeActions.Utils (indentation, prefer, quickFix)

codeAction ::
  LSP.TextDocumentIdentifier ->
  LSP.Diagnostic ->
  T.Text ->
  Maybe T.Text ->
  [T.Text] ->
  LSP.CodeAction
codeAction = insertFields

insertFields ::
  LSP.TextDocumentIdentifier ->
  LSP.Diagnostic ->
  T.Text ->
  Maybe T.Text ->
  [T.Text] ->
  LSP.CodeAction
insertFields tdi diag ctor existingFields missingFields =
  let spaces = indentation diag._range
      seps = "{ " : repeat ", "
      formatNewField sep fld = T.concat [spaces, sep, fld, " = _"]
      formatOldFields flds = T.concat [spaces, ", ", flds]
      newFields = zipWith formatNewField seps missingFields
      allFields = case existingFields of
          Nothing -> newFields
          Just flds -> newFields <> [formatOldFields flds]
      renderedExpr = T.concat [ctor, "\n" <> T.unlines allFields <> spaces <> "}\n"]
   in prefer $ quickFix tdi diag "Insert fields." (diag._range) renderedExpr
