{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.CodeActions where

import Data.LineCol (LineCol (..))
import Data.Path (AbsPath)
import Data.Rope qualified as Rope
import StaticLS.IDE.CodeActions.AddTypeSig qualified as AddTypeSig
import StaticLS.IDE.CodeActions.AutoExport as AutoExport
import StaticLS.IDE.CodeActions.AutoImport qualified as AutoImport
import StaticLS.IDE.CodeActions.AutoImportExisting qualified as AutoImportExisting
import StaticLS.IDE.CodeActions.RemoveRedundantImports as RemoveRedundantImports
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Monad
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.Monad (StaticLsM)

getCodeActions :: AbsPath -> LineCol -> StaticLsM [Assist]
getCodeActions path lineCol = do
  rope <- getSourceRope path
  let pos = Rope.lineColToPos rope lineCol
  let cx = CodeActionContext {path, pos, lineCol}
  typesCodeActions <- AddTypeSig.codeAction cx
  importCodeActions <- AutoImport.codeAction cx
  removeRedundantImports <- RemoveRedundantImports.codeAction cx
  exportCodeActions <- AutoExport.codeAction cx
  autoImportExistingCodeActions <- AutoImportExisting.codeAction cx
  let codeActions = typesCodeActions ++ importCodeActions ++ removeRedundantImports ++ exportCodeActions ++ autoImportExistingCodeActions
  pure codeActions

resolveLazyAssist :: CodeActionMessage -> StaticLsM SourceEdit
resolveLazyAssist (CodeActionMessage {kind, path}) = do
  case kind of
    AutoImportActionMessage toImport -> AutoImport.resolveLazy path toImport
    NoMessage -> do
      pure SourceEdit.empty
