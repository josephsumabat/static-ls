module StaticLS.IDE.CodeActions.AddRequiredExtension where

import StaticLS.IDE.CodeActions.Utils

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.CodeActions.Parse qualified as Parse

codeAction ::
  LSP.TextDocumentIdentifier ->
  LSP.Diagnostic ->
  Parse.KnownExtension ->
  LSP.CodeAction
codeAction = addRequiredExtension

addRequiredExtension ::
  LSP.TextDocumentIdentifier ->
  LSP.Diagnostic ->
  Parse.KnownExtension ->
  LSP.CodeAction
addRequiredExtension tdi diag (Parse.KnownExtension _ text) =
  let title = "Add language extension: " <> text
      txt = T.concat ["{-# LANGUAGE ", text, " #-}\n"]
      rng = insertAt 0 0
   in prefer $ quickFix tdi diag title rng txt
