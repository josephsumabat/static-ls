module StaticLS.IDE.CodeActions.AutoExport where

import AST qualified
import AST.Haskell as Haskell
import Arborist.AutoExport (getAllDeclExportEdit, getDeclExportEdit)
import Data.Path
import Data.Range (Range)
import Data.Range qualified as Range
import Data.Text (Text)
import Hir
import Hir.Parse as AST
import Hir.Types qualified as Hir
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Monad
import StaticLS.IDE.SourceEdit as SourceEdit
import StaticLS.Monad

dropModule :: Hir.Qualified Hir.HirRead -> Hir.Name Hir.HirRead
dropModule (Hir.Qualified _ name) = name

qualifiedToText :: Hir.Name Hir.HirRead -> Text
qualifiedToText nm = AST.nodeToText (nm.dynNode)

isAlreadyExported :: Hir.Program Hir.HirRead -> Hir.Decl Hir.HirRead -> Bool
isAlreadyExported prog decl =
  let current = getCurrentExportNames prog
      nameTxt = declNameText decl
   in nameTxt `elem` current

getCurrentExportNames :: Hir.Program Hir.HirRead -> [Text]
getCurrentExportNames prog =
  case prog.exports of
    Nothing -> []
    Just xs -> map (qualifiedToText . dropModule) (exportItemNames xs)

isSupportedDecl :: Hir.Decl Hir.HirRead -> Bool
isSupportedDecl decl =
  case decl of
    Hir.DeclBind _ -> True
    Hir.DeclData _ -> True
    Hir.DeclNewtype _ -> True
    Hir.DeclClass _ -> True
    _ -> False

getDeclarationsAtPoint :: Range -> [Hir.Decl Hir.HirRead] -> [Hir.Decl Hir.HirRead]
getDeclarationsAtPoint range decls =
  filter (\decl -> (declName decl).dynNode.nodeRange `Range.containsRange` range) decls

getHeaderAtPoint :: Range -> Haskell.HeaderP -> Maybe Haskell.HeaderP
getHeaderAtPoint cursorLocation headerP =
  if (AST.getDynNode headerP).nodeRange `Range.containsRange` cursorLocation
    then Just headerP
    else Nothing

mkAssistForAllDecl :: AbsPath -> Hir.Program Hir.HirRead -> Haskell.HeaderP -> Assist
mkAssistForAllDecl path prog headerP =
  let allExportEdit = getAllDeclExportEdit prog headerP
      sourceEdit = SourceEdit.single path allExportEdit
      label = "Add exports for all declarations"
   in mkAssist label sourceEdit

mkAssistForDecl :: AbsPath -> Haskell.HeaderP -> Hir.Decl Hir.HirRead -> Assist
mkAssistForDecl path headerP decl =
  let declExportEdit = getDeclExportEdit headerP decl
      sourceEdit = SourceEdit.single path declExportEdit
      label = "Add export for " <> declNameText decl
   in mkAssist label sourceEdit

codeAction :: CodeActionContext -> StaticLsM [Assist]
codeAction CodeActionContext {path, pos} = do
  hir <- getHir path

  -- get the decl at the current cursor/highlight pos
  let cursorLocation = Range.point pos
      allDeclsAtPoint = getDeclarationsAtPoint cursorLocation (filter isSupportedDecl hir.decls)
      declsAtPoint = filter (not . isAlreadyExported hir) allDeclsAtPoint

  -- get header
  let dynNode = AST.getDynNode hir.node
      mHeaderP = AST.findNode (AST.cast @Haskell.HeaderP) dynNode

  case mHeaderP of
    Nothing -> pure []
    Just headerP -> do
      let mHeaderPCursor = getHeaderAtPoint cursorLocation headerP
          assistAll = case mHeaderPCursor of
            Just headerP -> [mkAssistForAllDecl path hir headerP]
            _ -> []
          assistsPerDecl = map (mkAssistForDecl path headerP) declsAtPoint

      pure (assistAll ++ assistsPerDecl)
