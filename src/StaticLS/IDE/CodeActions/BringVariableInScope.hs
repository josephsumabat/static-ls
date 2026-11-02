module StaticLS.IDE.CodeActions.BringVariableInScope (codeAction) where

import AST qualified
import AST.Haskell qualified as H
import Control.Monad (guard)
import Data.Char qualified as Char
import Data.Either (fromRight)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Path (AbsPath)
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.CodeActions.Parse (VariableNotInScopeFixes (..))
import StaticLS.IDE.CodeActions.Utils (insertAt, quickFix)
import StaticLS.IDE.Monad (getHaskell, getSourceRope)
import StaticLS.Monad (StaticLsM)
import StaticLS.ProtoLSP (posToLSPPosition)
import StaticLS.Tree qualified as Tree

codeAction ::
  AbsPath ->
  LSP.TextDocumentIdentifier ->
  LSP.Diagnostic ->
  VariableNotInScopeFixes ->
  StaticLsM (Maybe LSP.CodeAction)
codeAction _path tdi diag (MissSpelled oldName newName) =
  pure $ fixMissSpelled tdi diag oldName newName
codeAction path tdi diag (AddToImportList name mdl) = do
  haskell <- getHaskell path
  rope <- getSourceRope path
  pure do
    (closingParen, existing) <- findImportList mdl haskell
    let LSP.Position line col = posToLSPPosition rope closingParen
        sep
          | T.null existing = ""
          | "," `T.isSuffixOf` existing = " "
          | otherwise = ", "
        title = "Add " <> name <> " to the import list of " <> mdl
    pure $ quickFix tdi diag title (insertAt line col) (sep <> importItem name)

-- The diagnostic range can be a zero-width point (ghcid output without a
-- column span), so the replaced range is derived from the name in the source.
fixMissSpelled :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> T.Text -> T.Text -> Maybe LSP.CodeAction
fixMissSpelled tdi diag oldName newName = do
  let start = diag._range._start
      end = LSP.Position start._line (start._character + fromIntegral (T.length oldName))
  pure $ quickFix tdi diag ("Change spelling to " <> newName) (LSP.Range start end) newName

importItem :: T.Text -> T.Text
importItem name = case T.uncons name of
  Just (c, _) | not (Char.isAlpha c || c == '_') -> "(" <> name <> ")"
  _ -> name

-- | Position of the closing paren of the first non-hiding import list of the
-- module, together with the current contents of that list.
findImportList :: T.Text -> H.Haskell -> Maybe (Pos, T.Text)
findImportList mdl haskell = do
  imports <- fromRight Nothing $ Tree.getImports haskell
  listToMaybe (mapMaybe importListOf imports.imports)
 where
  importListOf imp = do
    hirImport <- either (const Nothing) Just $ Hir.parseImport imp
    guard $ hirImport.mod.text == mdl && not hirImport.hiding
    names <- fromRight Nothing . AST.collapseErr $ imp.names
    let txt = AST.nodeToText names
    guard (")" `T.isSuffixOf` txt)
    let end = (AST.nodeToRange names).end.pos
    pure (Pos (end - 1), T.strip (T.drop 1 (T.dropEnd 1 txt)))
