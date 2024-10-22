{-# LANGUAGE OverloadedStrings #-}

module StaticLS.IDE.CodeActions.RemoveRedundantImports where

import Control.Exception
import Control.Monad.IO.Class
import Data.Edit
import Data.Map ()
import Data.Path
import Data.Pos qualified as Pos
import Data.Range qualified as Range
import Data.Rope qualified as Rope
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Diagnostics
import StaticLS.IDE.Diagnostics.ParseGHC
import StaticLS.IDE.FileWith
import StaticLS.IDE.SourceEdit
import StaticLS.Monad
import StaticLS.StaticEnv
import System.FilePath

ghcidFile :: FilePath
ghcidFile = "ghcid.txt"

codeAction :: CodeActionContext -> StaticLsM [Assist]
codeAction CodeActionContext {path, lineCol, pos} = do
  diagnostics <- getDiagnostics
  let globalRedundantImportDiagnostics = filter isRedundantImportDiagnostic diagnostics
  let localRedundantImportDiagnostics = filter (isDiagnosticInFile path) globalRedundantImportDiagnostics
  let diagnosticsToAssist message diagnostics = mkAssist message <$> actOnDiagnostics diagnostics
  -- globalAssist <- diagnosticsToAssist "Remove redundant imports in project" globalRedundantImportDiagnostics -- Does not work reliably yet.
  localAssist <- diagnosticsToAssist "Remove redundant imports in file" localRedundantImportDiagnostics
  pure [localAssist]

getDiagnostics :: StaticLsM [Diagnostic]
getDiagnostics = do
  staticEnv <- getStaticEnv
  let wsRootPath = toFilePath staticEnv.wsRoot
  let makeAbsPath = unsafeFilePathToAbs . (wsRootPath System.FilePath.</>) . toFilePath
  let ghcidPath = wsRootPath System.FilePath.</> ghcidFile
  info <- liftIO $ catch @IOException (TextIO.readFile ghcidPath) (const $ pure "")
  let diagnostics = parse makeAbsPath info
  pure diagnostics

actOnDiagnostics :: [Diagnostic] -> StaticLsM SourceEdit
actOnDiagnostics diagnostics = do
  deletions <- traverse createDeletion diagnostics
  let sourceEdit = mconcat deletions
  pure sourceEdit

isRedundantImportDiagnostic :: Diagnostic -> Bool
isRedundantImportDiagnostic diagnostic = do
  let dmessage = diagnostic.message
  let correctPrefix = Text.isPrefixOf "The import of" dmessage || Text.isPrefixOf "The qualified import of" dmessage
  let correctSuffix = Text.isSuffixOf "is redundant" dmessage
  correctPrefix && correctSuffix

isDiagnosticInFile :: AbsPath -> Diagnostic -> Bool
isDiagnosticInFile curPath diagnostic = do
  let dpath = diagnostic.range.path
  toFilePath dpath == toFilePath curPath

createDeletion :: Diagnostic -> StaticLsM SourceEdit
createDeletion diagnostic = do
  let path = diagnostic.range.path
  sourceText <- liftIO $ TextIO.readFile (toFilePath path)
  let sourceRope = Rope.fromText sourceText
  let range = extend 1 0 $ Rope.lineColRangeToRange sourceRope diagnostic.range.loc
  let edit = single path (delete range)
  pure edit

-- This allows us to remove the whole line rather than just the contents of the line
extend :: Int -> Int -> Range.Range -> Range.Range
extend startex endex (Range.Range (Pos.Pos start) (Pos.Pos end)) = Range.mkRange (Pos.mkPos (start - startex)) (Pos.mkPos (end + endex))
