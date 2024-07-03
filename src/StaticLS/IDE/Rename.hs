module StaticLS.IDE.Rename
  ( rename,
    canRenameAtPos,
  )
where

import AST (Cast)
import AST qualified
import AST.Haskell qualified as H
import AST.Node (DynNode)
import Control.Applicative ((<|>))
import Data.Change qualified as Change
import Data.Edit qualified as Edit
import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import Data.Range qualified as Range
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
import StaticLS.Tree qualified as Tree

data RenameContext
  = RenameQualified Hir.Qualified
  | RenameTopSplice H.TopSplice
  | RenameSplice H.Splice
  | RenameOther
  deriving (Show)

getRenameContext :: H.Haskell -> Range -> AST.Err RenameContext
getRenameContext hs range = do
  let topSplice = AST.getDeepestContaining @H.TopSplice range hs.dynNode
  let splice = AST.getDeepestContaining @H.Splice range hs.dynNode
  qualified <- Hir.getQualifiedAtPoint range hs
  let res = (RenameQualified <$> qualified) <|> (RenameTopSplice <$> topSplice) <|> (RenameSplice <$> splice)
  pure $ Maybe.fromMaybe RenameOther res

getEverything :: (Cast b) => DynNode -> [b]
getEverything node = case AST.cast node of
  Just n -> n : concatMap getEverything node.nodeChildren
  Nothing -> concatMap getEverything node.nodeChildren

rename :: AbsPath -> LineCol -> Text -> StaticLsM SourceEdit
rename path lineCol newName = do
  sourceRope <- getSourceRope path
  let pos = Rope.lineColToPos sourceRope lineCol
  refs <- References.findRefsPos path lineCol
  haskell <- getHaskell path
  let nameTypes = Hir.getNameTypes (Range.empty pos) haskell
  let nameText = AST.nodeToText <$> nameTypes
  sourceEdits <- for refs \ref -> do
    let path = ref.path
    -- let lineColRange = Rope.rangeToLineColRange sourceRope ref.loc
    -- let astPoint = Semantic.Position.lineColToAstPoint lineColRange.start
    let context = getRenameContext haskell ref.loc
    let defaultEdit = Edit.replace ref.loc newName
    let defaultSourceEdit = SourceEdit.single ref.path defaultEdit
    let onSplice dynNode =
          case nameText of
            Nothing -> pure SourceEdit.empty
            Just nameText -> do
              let everything = traverse Hir.parseThQuotedName (getEverything @H.ThQuotedName haskell.dynNode)
              case everything of
                Left _e -> do
                  pure SourceEdit.empty
                Right everything -> do
                  let changes =
                        Maybe.mapMaybe
                          ( \quoted ->
                              if quoted.node.nodeText == nameText
                                then Just $ Change.replace (quoted.node.nodeRange) newName
                                else Nothing
                          )
                          everything
                  logInfo $ "splice changes: " <> T.pack (show changes)
                  pure $ SourceEdit.single ref.path (Edit.changesToEdit changes)
    case context of
      Left e -> do
        logError $ T.pack $ show e
        pure defaultSourceEdit
      Right context -> do
        logInfo $ "got context: " <> T.pack (show context)
        case context of
          RenameQualified q -> do
            logInfo "got qualified for rename"
            let start = q.node.dynNode.nodeRange.start
            logInfo $ "idStart: " <> T.pack (show start) <> " oldStart: " <> T.pack (show ref.loc.start)
            let edit = Edit.replace (Range start ref.loc.end) newName
            pure $ SourceEdit.single ref.path edit
          RenameTopSplice topSplice -> onSplice topSplice.dynNode
          RenameSplice splice -> onSplice splice.dynNode
          RenameOther -> pure defaultSourceEdit
  let sourceEdit = mconcat sourceEdits
  pure sourceEdit

canRenameAtPos :: AbsPath -> Pos -> StaticLsM (Maybe Range)
canRenameAtPos path pos = do
  haskell <- getHaskell path
  let name = AST.getDeepestContaining @Hir.NameTypes (Range.empty pos) (AST.getDynNode haskell)
  case name of
    Just n -> do
      let range = AST.nodeToRange n
      pure $ Just range
    Nothing -> pure Nothing
