module StaticLS.IDE.CodeActions.AutoQualify where

import AST qualified
import AST.Haskell as Haskell
import Arborist.AutoQualify (qualifyIdentifier)
import Arborist.Scope.Global (getGlobalAvailableDecls)
import Arborist.Scope.Types (GlblDeclInfo (..))
import Control.Monad (forM)
import Data.Either.Extra (eitherToMaybe)
import Data.HashMap.Lazy qualified as Map
import Data.Path
import Data.Range (Range)
import Data.Range qualified as Range
import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace
import Hir.Parse as AST
import Hir.Types (ModuleText (..))
import Hir.Types qualified as Hir
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Monad
import StaticLS.IDE.SourceEdit as SourceEdit
import StaticLS.Monad

-- get the identifier at the cursor position
getIdentifierAtPoint :: Range -> Hir.Program Hir.HirRead -> Maybe (Maybe Text, Text)
getIdentifierAtPoint range prog =
  case AST.getQualifiedAtPoint range prog.node of
    Right (Just qualified) ->
      let name = AST.nodeToText qualified.name.dynNode
          qualifier = case qualified.mod of
            Nothing -> Nothing
            Just modName -> Just (modName.mod.text)
       in Just (qualifier, name)
    _ -> Nothing

-- find which modules an identifier can be imported from
findModulesForIdentifier :: AbsPath -> Text -> StaticLsM [(Text, Hir.Decl Hir.HirRead)]
findModulesForIdentifier path identifier = do
  hir <- getHir path

  programIndex <- getPrgIndex
  let exportIndex = Map.empty

  -- get all available declarations
  let availableDecls = getGlobalAvailableDecls programIndex exportIndex hir
      matchingDecls = filter (\info -> info.name == identifier) availableDecls
      moduleDecls =
        Map.toList $
          Map.fromList
            [(info.originatingMod.text, info.decl) | info <- matchingDecls]

  pure moduleDecls

parseImportToHir :: Haskell.ImportP -> Maybe (Hir.Import Hir.HirRead)
parseImportToHir = eitherToMaybe . AST.parseImport

-- get all imports from the current file
getCurrentImports :: Hir.Program Hir.HirRead -> [Haskell.ImportP]
getCurrentImports prog =
  let dynNode = AST.getDynNode prog.node
   in getAllImports dynNode
 where
  getAllImports node =
    let childImports = concatMap getAllImports node.nodeChildren
        thisImport = case AST.cast @Haskell.ImportP node of
          Just imp -> [imp]
          Nothing -> []
     in thisImport ++ childImports

-- check if identifier is in an import
identifierInImport :: Text -> Hir.Import Hir.HirRead -> Bool
identifierInImport identifier hirImport =
  case hirImport.importList of
    Nothing -> not hirImport.hiding
    Just [] -> False
    Just items -> any (\item -> AST.nodeToText item.name.dynNode == identifier) items

-- find qualified imports that can provide this identifier
findQualifiedImportsForIdentifier :: AbsPath -> Text -> [(Haskell.ImportP, Hir.Import Hir.HirRead)] -> StaticLsM [(Haskell.ImportP, Hir.Import Hir.HirRead)]
findQualifiedImportsForIdentifier path identifier imports = do
  -- get modules that actually export this identifier
  modulesThatExport <- findModulesForIdentifier path identifier
  let exportingModules = map fst modulesThatExport
  pure $ filter (canProvideIdentifier exportingModules) imports
 where
  canProvideIdentifier exportingModules (_, hirImport) =
    hirImport.qualified
      && hirImport.mod.text `elem` exportingModules
      && identifierInImport identifier hirImport

-- create assist for qualifying
mkAssistForQualify :: AbsPath -> Text -> Hir.Name Hir.HirRead -> Haskell.ImportP -> Hir.Import Hir.HirRead -> Assist
mkAssistForQualify path identifier identifierName importP hirImport =
  let
    qualifyEdit = qualifyIdentifier identifierName hirImport
    sourceEdit = SourceEdit.single path qualifyEdit
    qualifier = case hirImport.alias of
      Just alias -> alias.text
      Nothing -> hirImport.mod.text
    label = "Qualify as " <> qualifier <> "." <> identifier
   in
    mkAssist label sourceEdit

codeAction :: CodeActionContext -> StaticLsM [Assist]
codeAction CodeActionContext {path, pos} = do
  hir <- getHir path

  let cursorLocation = Range.point pos
      astImports = getCurrentImports hir
      parsedImports = [(imp, hirImp) | imp <- astImports, Just hirImp <- [parseImportToHir imp]]

  -- get identifier at cursor position
  case getIdentifierAtPoint cursorLocation hir of
    Nothing -> pure []
    Just (mQualifier, identifier) -> do
      case mQualifier of
        Just _ -> pure []
        Nothing -> do
          case AST.getQualifiedAtPoint cursorLocation hir.node of
            Right (Just qualified) -> do
              let identifierName = qualified.name
              qualifiedImports <- findQualifiedImportsForIdentifier path identifier parsedImports

              let assists = map (uncurry (mkAssistForQualify path identifier identifierName)) qualifiedImports
              pure assists
            _ -> pure []
