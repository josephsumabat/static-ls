module StaticLS.IDE.CodeActions.InsertFields (codeAction, renderFields) where

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.CodeActions.Utils (prefer, quickFix)

codeAction ::
  LSP.TextDocumentIdentifier ->
  LSP.Diagnostic ->
  T.Text ->
  Maybe T.Text ->
  [T.Text] ->
  Int ->
  LSP.CodeAction
codeAction tdi diag constructor existingFields missingFields leadingSpaces =
  prefer . quickFix tdi diag "Insert missing fields" (diag._range) $ renderFields constructor existingFields missingFields leadingSpaces

renderFields ::
  T.Text ->
  Maybe T.Text ->
  [T.Text] ->
  Int ->
  T.Text
renderFields constructor existingFields missingFields leadingSpaces =
  let spaces = T.replicate (leadingSpaces + 2) " "
      seps = "{ " : repeat ", "
      formatNewField sep fld = T.concat [spaces, sep, fld, " = _"]
      formatOldFields flds = T.concat [spaces, ", ", T.replace ", " ("\n" <> spaces <> ", ") flds]
      newFields = zipWith formatNewField seps missingFields
      allFields = case existingFields of
        Nothing -> newFields
        Just flds -> newFields <> [formatOldFields flds]
   in T.concat [constructor, "\n" <> T.unlines allFields <> spaces <> "}"]
