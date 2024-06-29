module StaticLS.IDE.Rename (
  rename,
  canRenameAtPos,
)
where

import AST qualified
import Data.Edit qualified as Edit
import Data.LineColRange (LineColRange (..))
import Data.Path (AbsPath)
import Data.Pos (LineCol (..), Pos (..))
import Data.Range (Range (..))
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.FileWith (FileWith' (..))
import StaticLS.IDE.Monad
import StaticLS.IDE.References qualified as References
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.Logger
import StaticLS.Monad
import StaticLS.Semantic.Position qualified as Semantic.Position
import StaticLS.Tree qualified as Tree

rename :: AbsPath -> LineCol -> Text -> StaticLsM SourceEdit
rename path lineCol newName = do
  refs <- References.findRefsPos path lineCol
  sourceEdits <- for refs \ref -> do
    let path = ref.path
    sourceRope <- getSourceRope path
    haskell <- getHaskell path
    let lineColRange = Rope.rangeToLineColRange sourceRope ref.loc
    let astPoint = Semantic.Position.lineColToAstPoint lineColRange.start
    let qualified = Tree.getQualifiedAtPoint haskell astPoint
    let edit = Edit.replace ref.loc newName
    let sourceEdit = SourceEdit.single ref.path edit
    case qualified of
      Left e -> do
        logError $ T.pack $ show e
        pure sourceEdit
      Right Nothing -> pure sourceEdit
      Right (Just q) -> do
        logInfo "got qualified for rename"
        let id = q.id
        let idStart = (AST.nodeToRange id).startByte
        logInfo $ "idStart: " <> T.pack (show idStart) <> " oldStart: " <> T.pack (show ref.loc.start)
        let edit = Edit.replace (Range (Pos idStart) ref.loc.end) newName
        pure $ SourceEdit.single ref.path edit
  let sourceEdit = mconcat sourceEdits
  pure sourceEdit

canRenameAtPos :: AbsPath -> LineCol -> StaticLsM (Maybe LineColRange)
canRenameAtPos path lineCol = do
  let astPoint = Semantic.Position.lineColToAstPoint lineCol
  haskell <- getHaskell path
  let name = AST.getDeepestContaining @Hir.NameTypes astPoint (AST.getDynNode haskell)
  case name of
    Just n -> do
      let range = AST.nodeToRange n
      pure $ Just $ Semantic.Position.astRangeToLineColRange range
    Nothing -> pure Nothing
