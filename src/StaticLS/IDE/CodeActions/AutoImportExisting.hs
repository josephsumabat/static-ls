module StaticLS.IDE.CodeActions.AutoImportExisting where

import AST qualified
import AST.Haskell as Haskell
import Arborist.AutoImport (addDeclToImportEdit)
import Arborist.Scope.Global (getGlobalAvalibleDecls)
import Arborist.Scope.Types (GlblDeclInfo(..))
import Control.Monad (forM)
import Data.HashMap.Lazy qualified as Map
import Data.Path
import Data.Range (Range)
import Data.Range qualified as Range
import Data.Text (Text)
import Data.Either.Extra (eitherToMaybe)
import Hir.Parse as AST
import Hir.Types (ModuleText(..))
import Hir.Types qualified as Hir
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Monad
import StaticLS.IDE.SourceEdit as SourceEdit
import StaticLS.Monad

-- get the identifier at the cursor position
getIdentifierAtPoint :: Range -> Hir.Program -> Maybe (Maybe Text, Text)
getIdentifierAtPoint range prog = 
  case AST.getQualifiedAtPoint range prog.node of
    Right (Just qualified) -> 
      let name = AST.nodeToText qualified.name.node
          qualifier = case qualified.mod of
            Nothing -> Nothing
            Just modName -> Just (modName.mod.text)
      in Just (qualifier, name)
    _ -> Nothing

-- find which modules an identifier can be imported from
findModulesForIdentifier :: AbsPath -> Text -> StaticLsM [(Text, Hir.Decl)]
findModulesForIdentifier path identifier = do
  hir <- getHir path
  
  programIndex <- getPrgIndex
  let exportIndex = Map.empty
  
  -- get all available declarations
  let availableDecls = getGlobalAvalibleDecls programIndex exportIndex hir
      matchingDecls = filter (\info -> info.name == identifier) availableDecls
      moduleDecls = Map.toList $ Map.fromList 
        [(info.originatingMod.text, info.decl) | info <- matchingDecls]
  
  pure moduleDecls

parseImportToHir :: Haskell.ImportP -> Maybe Hir.Import
parseImportToHir = eitherToMaybe . AST.parseImport

-- find existing imports for a module in the current file
findExistingImports :: Text -> Maybe Text -> [(Haskell.ImportP, Hir.Import)] -> [(Haskell.ImportP, Hir.Import)]
findExistingImports moduleName mQualifier imports =
  filter moduleMatches imports
  where
    moduleMatches (_, hirImport) = 
      hirImport.mod.text == moduleName &&
      case mQualifier of
        Nothing -> not hirImport.qualified
        Just qual ->
          hirImport.qualified && 
          case hirImport.alias of
            Just alias -> alias.text == qual
            Nothing -> hirImport.mod.text == qual 

-- get all imports from the current file
getCurrentImports :: Hir.Program -> [Haskell.ImportP]
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

-- check if identifier is already accessible through any import for this module
isIdentifierAccessible :: Text -> Text -> Maybe Text -> [(Haskell.ImportP, Hir.Import)] -> Bool
isIdentifierAccessible moduleName identifier mQualifier imports =
  any canAccess imports
  where
    canAccess (_, hirImport) 
      | hirImport.mod.text /= moduleName = False
      | hirImport.hiding = False
      | otherwise = 
          case mQualifier of
            Nothing -> 
              not hirImport.qualified && 
              case hirImport.importList of
                Nothing -> True 
                Just [] -> False 
                Just items -> any (\item -> AST.nodeToText item.name.node == identifier) items  
            Just qual ->
              hirImport.qualified &&
              matchesQualifier qual hirImport &&
              case hirImport.importList of
                Nothing -> True
                Just [] -> False 
                Just items -> any (\item -> AST.nodeToText item.name.node == identifier) items
    
    matchesQualifier qual hirImport =
      case hirImport.alias of
        Just alias -> alias.text == qual
        Nothing -> hirImport.mod.text == qual

-- check if identifier is in a specific import
identifierInImport :: Text -> Hir.Import -> Bool  
identifierInImport identifier hirImport =
  case hirImport.importList of
    Nothing -> not hirImport.hiding
    Just [] -> False
    Just items -> any (\item -> AST.nodeToText item.name.node == identifier) items

-- create assist for adding import
mkAssistForImport :: AbsPath -> Text -> Text -> Hir.Decl -> Haskell.ImportP -> Hir.Import -> Assist
mkAssistForImport path identifier moduleName decl existingImport hirImport =
  let 
    dynNode = AST.getDynNode existingImport
    importEdit = addDeclToImportEdit dynNode hirImport decl
    sourceEdit = SourceEdit.single path importEdit
    label = "Import " <> identifier <> " from " <> moduleName
  in mkAssist label sourceEdit

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
      moduleInfos <- findModulesForIdentifier path identifier
      -- fore each module that exports this identifier
      assists <- forM moduleInfos $ \(moduleName, decl) -> do
        -- check if identifier is already accessible 
        if isIdentifierAccessible moduleName identifier mQualifier parsedImports
          then pure []
          else do
            let matchingImports = findExistingImports moduleName mQualifier parsedImports
                importsNeedingAdd = filter (\(_, hirImp) -> not (identifierInImport identifier hirImp)) matchingImports
            pure $ map (\(imp, hirImp) -> mkAssistForImport path identifier moduleName decl imp hirImp) importsNeedingAdd
      pure $ concat assists