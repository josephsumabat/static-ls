module StaticLS.Hir.Print (
  printExportItem,
  printExportItems,
)
where

import AST qualified
import Data.Text.Lazy qualified as TL
import StaticLS.Hir.Types

printName :: Name -> TL.Text
printName name = TL.fromStrict name.node.nodeText

printModuleName :: ModuleName -> TL.Text
printModuleName name = TL.fromStrict name.mod.text

printQualified :: Qualified -> TL.Text
printQualified (Qualified {mod, name}) = case mod of
  Nothing -> printName name
  Just modName -> TL.concat [printModuleName modName, ".", printName name]

printExportChildren :: ExportChildren -> TL.Text
printExportChildren export = case export of
  ExportAllChildren -> ".."
  ExportChild namespace name -> printQualified name

printExportItem :: ExportItem -> TL.Text
printExportItem export = case export of
  ExportItem {namespace, name, children} ->
    printQualified name <> TL.intercalate ", " (map printExportChildren children)
  ExportModuleItem mod -> "module " <> printModuleName mod

printExportItems :: [ExportItem] -> TL.Text
printExportItems exports = TL.intercalate ",\n" (map printExportItem exports)
