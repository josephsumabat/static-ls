module StaticLS.Hir where

import AST (DynNode)
import AST qualified
import AST.Haskell qualified as H
import AST.Haskell qualified as Haskell
import AST.Sum (Nil, (:+))
import Control.Applicative (asum, (<|>))
import Data.Either qualified as Either
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Range (Range)
import Data.Text (Text)
import Data.Text qualified as T

data NameSpace
  = NameSpaceValue
  | NameSpaceType
  | NameSpacePattern
  deriving (Show)

data Name = Name
  { node :: !DynNode
  , isOperator :: !Bool
  , isConstructor :: !Bool
  }
  deriving (Show)

data Qualified = Qualified
  { mod :: Maybe ModuleName
  , name :: Name
  }
  deriving (Show)

data ModuleText = ModuleText
  { parts :: NonEmpty Text
  , text :: Text
  }
  deriving (Show, Eq)

data ModuleName = ModuleName
  { mod :: ModuleText
  , node :: H.Module
  }
  deriving (Show, Eq)

data ImportChildren
  = ImportAllChildren
  | ImportChild NameSpace Name
  deriving (Show)

data ImportItem = ImportItem
  { namespace :: NameSpace
  , name :: Name
  , children :: [ImportChildren]
  }
  deriving (Show)

data ExportChildren
  = ExportAllChildren
  | ExportChild NameSpace Qualified
  deriving (Show)

data ExportItem
  = ExportItem
      { namespace :: NameSpace
      , name :: Qualified
      , children :: [ExportChildren]
      }
  | ExportModuleItem ModuleName
  deriving (Show)

data ImportName = ImportName
  { name :: Text
  }
  deriving (Show, Eq)

data Import = Import
  { mod :: ModuleText
  , alias :: Maybe ModuleText
  , qualified :: !Bool
  , hiding :: !Bool
  , importList :: [ImportItem]
  }
  deriving (Show)

pattern OpenImport :: ModuleText -> Import
pattern OpenImport mod = Import {mod, alias = Nothing, qualified = False, hiding = False, importList = []}

type ParseNameTypes =
  Haskell.Name
    :+ Haskell.Constructor
    :+ Haskell.Variable
    :+ Haskell.Operator
    :+ Haskell.FieldName
    :+ Haskell.ConstructorOperator
    :+ Nil

parseName :: ParseNameTypes -> Name
parseName ast = case ast of
  AST.Inj @H.Name _ ->
    Name
      { node
      , isOperator = False
      , isConstructor = False
      }
  AST.Inj @H.Constructor _ ->
    Name
      { node
      , isOperator = False
      , isConstructor = True
      }
  AST.Inj @H.Variable _ ->
    Name
      { node
      , isOperator = False
      , isConstructor = False
      }
  AST.Inj @H.Operator _ ->
    Name
      { node
      , isOperator = True
      , isConstructor = False
      }
  AST.Inj @H.FieldName _ ->
    Name
      { node
      , isOperator = False
      , isConstructor = False
      }
  AST.Inj @H.ConstructorOperator _ ->
    Name
      { node
      , isOperator = True
      , isConstructor = True
      }
  _ -> error "could not parse name"
 where
  node = AST.getDynNode ast

parseModuleTextFromText :: Text -> ModuleText
parseModuleTextFromText text =
  ModuleText
    { parts = NE.fromList (T.splitOn "." text)
    , text
    }

importQualifier :: Import -> ModuleText
importQualifier i =
  -- even if something is not imported qualified,
  -- it still produced a namespace that can be used as a qualifier
  -- for example
  -- `import Data.Text`
  -- allows you to use `Data.Text.Text` with the qualifier
  -- or just `FilePath` without the qualifier
  Maybe.fromMaybe i.mod i.alias

findNode :: (AST.DynNode -> Maybe b) -> AST.DynNode -> Maybe b
findNode f n = go n
 where
  go n = f n <|> asum (go <$> (AST.nodeChildren n))

parseImportName :: H.ImportName -> AST.Err ImportName
parseImportName name = do
  let text = AST.nodeToText name
  pure $ ImportName {name = text}

parseNameSpace :: H.Namespace -> AST.Err NameSpace
parseNameSpace n = case n.dynNode.nodeText of
  "data" -> pure NameSpaceValue
  "type" -> pure NameSpaceType
  "pattern" -> pure NameSpacePattern
  _ -> Left $ "could not parse namespace: " <> T.pack (show n)

parseImportOperator :: H.PrefixId -> AST.Err Name
parseImportOperator operator = do
  operator <- operator.children
  parseName <$> removeQualified operator

parseExportOperator :: H.PrefixId -> AST.Err Qualified
parseExportOperator operator = do
  operator <- operator.children
  parseQualified $ AST.subset operator

removeQualified :: (AST.Subset n (H.Qualified :+ ParseNameTypes)) => n -> AST.Err ParseNameTypes
removeQualified n = case AST.subset @_ @(H.Qualified :+ ParseNameTypes) n of
  AST.X qualified -> Left $ "qualified name in import: " <> qualified.dynNode.nodeText
  AST.Rest name -> pure name

type ParseImportChildren = H.Qualified :+ H.AllNames :+ H.AssociatedType :+ H.PrefixId :+ ParseNameTypes

parseImportChild :: ParseImportChildren -> AST.Err ImportChildren
parseImportChild child = case child of
  AST.X qualified -> Left $ "qualified name in import children: " <> qualified.dynNode.nodeText
  AST.Rest (AST.X _allNames) -> pure ImportAllChildren
  AST.Rest (AST.Rest (AST.X assocType)) -> do
    type' <- assocType.type'
    name <- removeQualified type'
    pure $ ImportChild NameSpaceType (parseName name)
  AST.Rest (AST.Rest (AST.Rest (AST.X prefixId))) -> do
    operator <- prefixId.children
    name <- removeQualified operator
    pure $ ImportChild NameSpaceValue (parseName name)
  AST.Rest (AST.Rest (AST.Rest (AST.Rest rest))) -> pure $ ImportChild NameSpaceValue (parseName rest)

parseExportChild :: ParseImportChildren -> AST.Err ExportChildren
parseExportChild child = case child of
  AST.X qualified -> do
    name <- parseQualified (AST.Inj qualified)
    pure $ ExportChild NameSpaceValue name
  AST.Rest (AST.X _allNames) -> pure ExportAllChildren
  AST.Rest (AST.Rest (AST.X assocType)) -> do
    type' <- assocType.type'
    name <- parseQualified $ AST.subset type'
    pure $ ExportChild NameSpaceType name
  AST.Rest (AST.Rest (AST.Rest (AST.X prefixId))) -> do
    operator <- prefixId.children
    name <- parseQualified $ AST.subset operator
    pure $ ExportChild NameSpaceValue name
  AST.Rest (AST.Rest (AST.Rest (AST.Rest rest))) -> do
    name <- parseQualified (AST.subset rest)
    pure $ ExportChild NameSpaceValue name

parseImportChildren :: H.Children -> AST.Err [ImportChildren]
parseImportChildren children = do
  element <- AST.collapseErr children.element
  let children = AST.subset @_ @ParseImportChildren <$> element
  children <- traverse parseImportChild children
  pure children

parseImportItem :: H.ImportName -> AST.Err ImportItem
parseImportItem i = do
  namespace <- traverse parseNameSpace =<< AST.collapseErr i.namespace
  namespace <- pure $ Maybe.fromMaybe NameSpaceValue namespace
  name <- do
    operator <- traverse parseImportOperator =<< AST.collapseErr i.operator
    type' <- traverse (fmap parseName . removeQualified) =<< AST.collapseErr i.type'
    variable <- traverse (fmap parseName . removeQualified) =<< AST.collapseErr i.variable
    case operator <|> type' <|> variable of
      Just n -> pure n
      Nothing -> Left "could not parse import name"
  children <- traverse parseImportChildren =<< AST.collapseErr i.children'
  children <- pure $ Maybe.fromMaybe [] children
  pure
    ImportItem
      { namespace
      , name
      , children
      }

parseExportChildren :: H.Children -> AST.Err [ExportChildren]
parseExportChildren children = do
  element <- AST.collapseErr children.element
  let children = AST.subset @_ @ParseImportChildren <$> element
  children <- traverse parseExportChild children
  pure children

parseExportItem :: H.Export -> AST.Err ExportItem
parseExportItem e = do
  namespace <- traverse parseNameSpace =<< AST.collapseErr e.namespace
  namespace <- pure $ Maybe.fromMaybe NameSpaceValue namespace
  name <- do
    operator <- traverse parseExportOperator =<< AST.collapseErr e.operator
    type' <- traverse (parseQualified . AST.subset) =<< AST.collapseErr e.type'
    variable <- traverse (parseQualified . AST.subset) =<< AST.collapseErr e.variable
    case operator <|> type' <|> variable of
      Just n -> pure n
      Nothing -> Left "could not parse import name"
  children <- traverse parseExportChildren =<< AST.collapseErr e.children'
  children <- pure $ Maybe.fromMaybe [] children
  pure
    ExportItem
      { namespace
      , name
      , children
      }
parseModuleExportItem :: H.ModuleExport -> AST.Err ExportItem
parseModuleExportItem e = do
  module' <- parseModuleName =<< e.module'
  pure $ ExportModuleItem module'

parseExportList :: H.Exports -> AST.Err [ExportItem]
parseExportList exports = do
  export <- AST.collapseErr exports.export
  moduleExports <- AST.collapseErr exports.children
  normalExports <- traverse parseExportItem export
  moduleExports <- traverse parseModuleExportItem moduleExports
  pure $ normalExports ++ moduleExports

parseImportList :: H.ImportList -> AST.Err [ImportItem]
parseImportList i = do
  name <- AST.collapseErr i.name
  items <- traverse parseImportItem name
  pure items

parseModuleText :: H.Module -> AST.Err ModuleText
parseModuleText m = do
  ids <- AST.collapseErr m.children
  pure $
    ModuleText
      { text =
          -- the text sometimes includes trailing dots
          T.dropWhileEnd (== '.') (AST.nodeToText m)
      , parts = fmap AST.nodeToText ids
      }

parseModuleName :: H.Module -> AST.Err ModuleName
parseModuleName m = do
  mod <- parseModuleText m
  pure $ ModuleName {mod, node = m}

parseImport :: H.Import -> AST.Err Import
parseImport i = do
  mod <- i.module'
  mod <- parseModuleText mod
  alias <- AST.collapseErr i.alias
  alias <- traverse parseModuleText alias
  importList <- AST.collapseErr i.names
  importList <- traverse parseImportList importList
  importList <- pure $ Maybe.fromMaybe [] importList
  let qualified = Maybe.isJust $ findNode (AST.cast @(AST.Token "qualified")) (AST.getDynNode i)
  let hiding = Maybe.isJust $ findNode (AST.cast @(AST.Token "hiding")) (AST.getDynNode i)
  pure
    Import
      { mod
      , alias
      , qualified
      , hiding
      , importList
      }

type ParseQualifiedTypes = H.Qualified :+ ParseNameTypes

parseQualified :: ParseQualifiedTypes -> AST.Err Qualified
parseQualified q = do
  case q of
    AST.X q -> do
      mod <- q.module'
      mod <- parseModuleName mod
      name <- q.id
      let name' = AST.subset @_ @ParseNameTypes name
      name <- pure $ parseName name'
      pure $ Qualified {mod = Just mod, name}
    AST.Rest q -> do
      let name = parseName q
      pure $ Qualified {mod = Nothing, name}

getQualifiedAtPoint :: Range -> H.Haskell -> AST.Err (Maybe Qualified)
getQualifiedAtPoint range h = do
  let node = AST.getDeepestContaining @H.Qualified range (AST.getDynNode h)
  case node of
    Nothing ->
      traverse
        parseQualified
        (AST.getDeepestContaining @ParseQualifiedTypes range (AST.getDynNode h))
    Just node -> Just <$> parseQualified (AST.Inj node)

parseImports :: H.Imports -> AST.Err ([Text], [Import])
parseImports i = do
  import' <- i.import'
  let (es, imports) = Either.partitionEithers (NE.toList import')
  imports <- pure $ parseImport <$> imports
  let (es', imports') = Either.partitionEithers imports
  pure (es ++ es', imports')

data DataDecl = DataDecl
  { name :: Name
  }
  deriving (Show)

data Decl = DeclData DataDecl
  deriving (Show)

parseDataType :: H.DataType -> AST.Err DataDecl
parseDataType dt = do
  undefined
parseDeclaration :: H.Declaration -> AST.Err (Maybe Decl)
parseDeclaration decl = case decl.getDeclaration of
  AST.Inj @H.DataType d -> do
    undefined

-- AST.Inj @H.

data Program = Program
  { imports :: [Import]
  , exports :: [ExportItem]
  , decls :: [Decl]
  }
  deriving (Show)

emptyProgram :: Program
emptyProgram =
  Program
    { imports = []
    , exports = []
    , decls = []
    }

parseHaskell :: H.Haskell -> ([Text], Program)
parseHaskell h = do
  let res = do
        imports <- AST.collapseErr h.imports
        (es, imports) <- case imports of
          Nothing -> pure ([], [])
          Just imports -> parseImports imports
        header <- AST.collapseErr h.children
        (es', exports) <- case header of
          Nothing -> pure (es, [])
          Just header -> do
            exports <- AST.collapseErr header.exports
            exports <- traverse parseExportList exports
            pure (es, Maybe.fromMaybe [] exports)
        pure (es ++ es', Program {imports, exports, decls = []})
  case res of
    Right (es, program) -> (es, program)
    Left e -> ([e], emptyProgram)

type GetNameTypes =
  Haskell.Name
    :+ Haskell.Constructor
    :+ Haskell.Variable
    :+ Haskell.Operator
    :+ Haskell.FieldName
    :+ Haskell.ConstructorOperator
    :+ Nil

getNameTypes :: Range -> H.Haskell -> Maybe GetNameTypes
getNameTypes range hs = AST.getDeepestContaining @GetNameTypes range hs.dynNode

data ThQuotedName = ThQuotedName
  { isTy :: Bool
  , node :: AST.DynNode
  }

parseThQuotedName :: H.ThQuotedName -> AST.Err ThQuotedName
parseThQuotedName thQuotedName = do
  name <- AST.collapseErr thQuotedName.name
  type' <- AST.collapseErr thQuotedName.type'
  case (ThQuotedName False . AST.getDynNode <$> name)
    <|> (ThQuotedName True . AST.getDynNode <$> type') of
    Just text -> pure text
    Nothing -> Left "ThQuotedName must have either a name or a type"
