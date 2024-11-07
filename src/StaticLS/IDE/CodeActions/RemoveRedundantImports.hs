{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module StaticLS.IDE.CodeActions.RemoveRedundantImports where

import AST qualified
import AST.Haskell qualified as Haskell
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Edit
import Data.Foldable
import Data.LineCol
import Data.List qualified as List
import Data.Map ()
import Data.Maybe
import Data.Path
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import Data.Range qualified as Range
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Diagnostics
import StaticLS.IDE.Diagnostics.ParseGHC
import StaticLS.IDE.FileWith
import StaticLS.IDE.Monad as Monad
import StaticLS.IDE.SourceEdit as SourceEdit
import StaticLS.Monad
import StaticLS.Semantic
import StaticLS.StaticEnv
import StaticLS.Tree
import System.FilePath

ghcidFile :: FilePath
ghcidFile = "ghcid.txt"

codeAction :: CodeActionContext -> StaticLsM [Assist]
codeAction CodeActionContext {path, lineCol, pos} = do
  diagnostics <- getDiagnostics
  let relevantDiagnostics = filter isRedundantImportDiagnostic diagnostics
  deletionInfos <- traverse mkDeletionInfo relevantDiagnostics
  globalRedundantImportDiagnostics <- filterM (\case Full f -> isFileSaved f.path; Partial _ -> pure False) deletionInfos
  let localRedundantImportDiagnostics = filter (isDeletionInFile path) deletionInfos
  let globalAssist = deletionsToAssist "Remove redundant imports in project" globalRedundantImportDiagnostics
  let localAssist = deletionsToAssist "Remove redundant imports in file" localRedundantImportDiagnostics
  pure [localAssist, globalAssist]

getDiagnostics :: StaticLsM [Diagnostic]
getDiagnostics = do
  staticEnv <- getStaticEnv
  let wsRootPath = toFilePath staticEnv.wsRoot
  let makeAbsPath = unsafeFilePathToAbs . (wsRootPath System.FilePath.</>) . toFilePath
  let ghcidPath = wsRootPath System.FilePath.</> ghcidFile
  info <- liftIO $ catch @IOException (TextIO.readFile ghcidPath) (const $ pure "")
  let diagnostics = parse makeAbsPath info
  pure diagnostics

data DeletionInfo = Partial PartialDeletionInfo | Full FullDeletionInfo

data PartialDeletionInfo = PartialDeletionInfo {}

data FullDeletionInfo = FullDeletionInfo
  { path :: AbsPath
  , sourceRope :: Rope
  , haskell :: Haskell.Haskell
  , loc :: Range
  , node :: Maybe Haskell.Import
  , isPartial :: Bool
  }

mkDeletionInfo :: Diagnostic -> StaticLsM DeletionInfo
mkDeletionInfo diagnostic
  | isPartialDiagnostic diagnostic = pure $ Partial PartialDeletionInfo {}
  | otherwise = Full <$> mkFullDeletionInfo diagnostic

mkFullDeletionInfo :: Diagnostic -> StaticLsM FullDeletionInfo
mkFullDeletionInfo diagnostic = do
  let path = diagnostic.range.path
  sourceRope <- getSourceRope path
  haskell <- getHaskell path
  let loc = Rope.lineColRangeToRange sourceRope diagnostic.range.loc
  node <- getImportAtLoc path loc
  pure FullDeletionInfo {..}

getImportAtLoc :: AbsPath -> Range -> StaticLsM (Maybe Haskell.Import)
getImportAtLoc path loc = do
  haskell <- getHaskell path
  let imports = getImports haskell
  let importList = case imports of
        Right (Just imp) -> imp.imports
        _ -> []
  let hsImport = find (\imp -> (AST.nodeToRange imp) `Range.containsRange` loc) importList
  pure hsImport

isFileSaved :: AbsPath -> StaticLsM Bool
isFileSaved absPath = do
  fileState <- getFileState absPath
  hieSource <- runMaybeT $ Monad.getHieSource absPath
  let contentsText = fileState.contentsText
  pure $ case hieSource of
    Just src -> src == contentsText
    Nothing -> False

deletionsToAssist :: Text.Text -> [DeletionInfo] -> Assist
deletionsToAssist message deletions = do
  let numFixes = length deletions
  let fixCase = if numFixes == 1 then "fix" else "fixes"
  let message' = message <> " (" <> (Text.pack . show) numFixes <> " " <> fixCase <> ")"
  mkAssist message' $ actOnDeletions deletions

actOnDeletions :: [DeletionInfo] -> SourceEdit
actOnDeletions deletionInfos = do
  let deletions = createDeletion <$> deletionInfos
  let sourceEdit = mconcat deletions
  sourceEdit

isRedundantImportDiagnostic :: Diagnostic -> Bool
isRedundantImportDiagnostic diagnostic = do
  let dmessage = diagnostic.message
  let isPrefixCorrect = Text.isPrefixOf "The import of" dmessage || Text.isPrefixOf "The qualified import of" dmessage
  let isSuffixCorrect = Text.isInfixOf "is redundant" dmessage
  isPrefixCorrect && isSuffixCorrect

isPartialDiagnostic :: Diagnostic -> Bool
isPartialDiagnostic diagnostic = Text.isInfixOf "from module" diagnostic.message

isDeletionInFile :: AbsPath -> DeletionInfo -> Bool
isDeletionInFile curPath = \case
  Partial PartialDeletionInfo -> False
  Full FullDeletionInfo {..} -> do
    toFilePath path == toFilePath curPath

createDeletion :: DeletionInfo -> SourceEdit
createDeletion = \case
  Partial pdi -> SourceEdit.empty
  Full fdi -> createFullDeletion fdi

createFullDeletion :: FullDeletionInfo -> SourceEdit
createFullDeletion FullDeletionInfo {..} = do
  let edit = case node of
        Nothing -> SourceEdit.empty
        Just hsImport -> single path (delete $ extend sourceRope haskell $ AST.nodeToRange hsImport)
  edit

createPartialDeletion :: DeletionInfo -> SourceEdit
createPartialDeletion diagnostic = do
  -- let startChar = '‘'
  -- let endChar = '’'
  SourceEdit.empty

extend :: Rope -> Haskell.Haskell -> Range -> Range
extend rope haskell range@(Range start end) = do
  let newEnd = fromMaybe end $ lastPosOnLine rope end
  let nodePred node = if (AST.nodeRange node).start.pos > start.pos then Just node else Nothing
  let node = getDeepestContainingSatisfying nodePred (Range.point newEnd) haskell.dynNode
  let newEnd' = case node of
        Nothing -> newEnd
        Just cmt -> (AST.nodeToRange cmt).end
  Range (Pos $ start.pos - 1) newEnd'

lastPosOnLine :: Rope -> Pos -> Maybe Pos
lastPosOnLine rope pos@(Pos posidx) = do
  let lineCol = Rope.posToLineCol rope pos
  lineLen <- Text.length . Rope.toText <$> Rope.getLine rope lineCol.line
  let (Pos col) = lineCol.col
  let diff = lineLen - col
  let newPos = Pos (posidx + diff - 1)
  pure newPos

-- This function is copy-pasted from tree-sitter-haskell.
getDeepestContainingSatisfying :: (AST.DynNode -> Maybe b) -> Range -> AST.DynNode -> Maybe b
getDeepestContainingSatisfying f range node = go node
 where
  go n =
    ( do
        n' <- List.find (\n -> n.nodeRange `Range.containsRange` range) n.nodeChildren
        go n'
    )
      <|> f n
