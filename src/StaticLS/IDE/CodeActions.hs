{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.CodeActions where

import Control.Error (mapMaybe)
import Control.Monad (join)
import Data.LineCol (LineCol (..))
import Data.Path (AbsPath)
import Data.Rope qualified as Rope
import Data.Set qualified as S
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.CodeActions.AddRequiredExtension qualified as AddRequiredExtension
import StaticLS.IDE.CodeActions.AddTypeSig qualified as AddTypeSig
import StaticLS.IDE.CodeActions.AutoImport qualified as AutoImport
import StaticLS.IDE.CodeActions.InsertAssociatedType qualified as InsertAssociatedType
import StaticLS.IDE.CodeActions.InsertCases qualified as InsertCases
import StaticLS.IDE.CodeActions.InsertFields qualified as InsertFields
import StaticLS.IDE.CodeActions.InsertMissingMethods qualified as InsertMissingMethods
import StaticLS.IDE.CodeActions.Parse qualified as Parse
import StaticLS.IDE.CodeActions.RemoveRedundantImports qualified as RemoveRedundantImports
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.CodeActions.UseValidHoleFit qualified as UseValidHoleFit
import StaticLS.IDE.Monad
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.Monad (StaticLsM)
import StaticLS.ProtoLSP (assistToCodeAction)

getCodeActions ::
  LSP.TextDocumentIdentifier ->
  LSP.CodeActionContext ->
  AbsPath ->
  LineCol ->
  StaticLsM [LSP.CodeAction]
getCodeActions tdi ctx path lineCol = do
  rope <- getSourceRope path
  let pos = Rope.lineColToPos rope lineCol
  let cx = CodeActionContext {path, pos, lineCol}
  typesCodeActions <- AddTypeSig.codeAction cx
  importCodeActions <- AutoImport.codeAction cx
  removeRedundantImports <- RemoveRedundantImports.codeAction cx
  let issues = S.fromList (mapMaybe Parse.actionableIssue ctx._diagnostics)
  issueActions <- join <$> traverse (issueToActions tdi) (S.toList issues)
  assistActions <-
    traverse assistToCodeAction $
      typesCodeActions
        ++ importCodeActions
        ++ removeRedundantImports
  pure $ assistActions ++ issueActions

resolveLazyAssist :: CodeActionMessage -> StaticLsM SourceEdit
resolveLazyAssist (CodeActionMessage {kind, path}) = do
  case kind of
    AutoImportActionMessage toImport -> AutoImport.resolveLazy path toImport
    NoMessage -> do
      pure SourceEdit.empty

issueToActions ::
  LSP.TextDocumentIdentifier ->
  Parse.ActionableIssue ->
  StaticLsM [LSP.CodeAction]
issueToActions tdi issue =
  case issue of
    Parse.MissingMethods (Parse.Ignored diag) methods ->
      pure [InsertMissingMethods.codeAction tdi diag methods]

    Parse.MissingAssociatedType (Parse.Ignored diag) ty ->
      pure [InsertAssociatedType.codeAction tdi diag ty]

    Parse.MissingFields (Parse.Ignored diag) ctor ext flds ->
      pure [InsertFields.codeAction tdi diag ctor ext flds]

    Parse.MissingCasses (Parse.Ignored diag) pats ->
      pure [InsertCases.codeAction tdi diag pats]

    Parse.RequiredExtension (Parse.Ignored diag) ext ->
      pure [AddRequiredExtension.codeAction tdi diag ext]

    Parse.TypedHoleFits (Parse.Ignored diag) fits ->
      pure $ UseValidHoleFit.codeAction tdi diag <$> fits
