module StaticLS.IDE.CodeActions.AutoExport where

import AST qualified
import AST.Haskell qualified
import Data.Range (Range)
import Data.Range qualified as Range
import StaticLS.Hir
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Monad
import StaticLS.Monad

getDeclNode :: Hir.Decl -> AST.DynNode
getDeclNode decl = case decl of
  DeclData d -> d.node.dynNode
  DeclNewtype n -> n.node.dynNode
  DeclClass c -> c.node.dynNode
  DeclSig s -> s.node.dynNode
  DeclBind b -> AST.getDynNode b.node
  DeclDataFamily d -> d.node.dynNode
  DeclPatternSig p -> p.node.dynNode
  DeclPattern p -> p.node.dynNode
  DeclTypeFamily t -> t.node.dynNode
  DeclTypeSynonym t -> t.node.dynNode

-- declToExportItem :: Hir.Decl -> ExportItem
-- declToExportItem decl = case decl of
--   DeclData d -> ExportItem (getDeclName decl) (AST.getDynNode d.node)
--   DeclNewtype n -> ExportItem (getDeclName decl) (AST.getDynNode n.node)
--   DeclClass c -> ExportItem (getDeclName decl) (AST.getDynNode c.node)
--   DeclSig s -> ExportItem (getDeclName decl) (AST.getDynNode s.node)
--   DeclBind b -> ExportItem (getDeclName decl) (AST.getDynNode b.node)
--   DeclDataFamily d -> ExportItem (getDeclName decl) (AST.getDynNode d.node)
--   DeclPatternSig p -> ExportItem (getDeclName decl) (AST.getDynNode p.node)
--   DeclPattern p -> ExportItem (getDeclName decl) (AST.getDynNode p.node)
--   DeclTypeFamily t -> ExportItem (getDeclName decl) (AST.getDynNode t.node)
--   DeclTypeSynonym t -> ExportItem (getDeclName decl) (AST.getDynNode t.node)

getDeclarationsAtPoint :: Range -> [Hir.Decl] -> [Hir.Decl]
getDeclarationsAtPoint range decls =
  filter (\decl -> (getDeclNode decl).nodeRange `Range.containsRange` range) decls

codeAction :: CodeActionContext -> StaticLsM [Assist]
codeAction cx = do
  hir <- getHir cx.path
  let _decls = getDeclarationsAtPoint (Range.point cx.pos) hir.decls
  pure []
