{-# LANGUAGE OverloadedStrings #-}

module StaticLS.IDE.CodeActions.RemoveRedundantImports where


import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Diagnostics.ParseGHC 
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import StaticLS.Monad
import Control.Monad.IO.Class
import Data.Path
import StaticLS.StaticEnv
import Control.Exception
import System.FilePath
import StaticLS.IDE.Diagnostics
import StaticLS.IDE.FileWith
import Data.Edit
import Data.Rope qualified as Rope
import StaticLS.IDE.SourceEdit
import Data.Map()
import Data.Range qualified as Range 
import Data.Pos qualified as Pos

ghcidFile :: FilePath
ghcidFile = "ghcid.txt"

codeAction :: CodeActionContext -> StaticLsM [Assist]
codeAction CodeActionContext {path, lineCol, pos} = do 
  diagnostics <- getDiagnostics
  let globalRedundantImportDiagnostics = filter isRedundantImportDiagnostic diagnostics 
  let localRedundantImportDiagnostics = filter (isDiagnosticInFile path) globalRedundantImportDiagnostics
  let diagnosticsToAssist message diagnostics = mkAssist message <$> actOnDiagnostics diagnostics
  globalAssist <- diagnosticsToAssist "Remove redundant imports in project" globalRedundantImportDiagnostics
  localAssist <- diagnosticsToAssist "Remove redundant imports in file" localRedundantImportDiagnostics
  pure [localAssist, globalAssist]

getDiagnostics :: StaticLsM [Diagnostic]
getDiagnostics = do 
  staticEnv <- getStaticEnv
  let wsRootPath = toFilePath staticEnv.wsRoot
  let absolutize = unsafeFilePathToAbs . (wsRootPath System.FilePath.</>) . toFilePath
  let ghcidPath = wsRootPath System.FilePath.</> ghcidFile
  info <- liftIO $ catch @IOException (TextIO.readFile ghcidPath) (const $ pure "")
  let diagnostics = parse absolutize info
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
   let range = extend 0 1 $ Rope.lineColRangeToRange sourceRope diagnostic.range.loc
   let edit = single path (delete range)
   pure edit

-- This allows us to remove the whole line rather than just the contents of the line
extend :: Int -> Int -> Range.Range -> Range.Range 
extend startex endex (Range.Range (Pos.Pos start) (Pos.Pos end)) = Range.mkRange (Pos.mkPos (start - startex)) (Pos.mkPos (end + endex))
