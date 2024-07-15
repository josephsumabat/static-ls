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

data NameSpace = NameSpaceValue | NameSpaceType
  deriving (Show)

data Name = Name
  { node :: !DynNode
  , isOperator :: !Bool
  , isConstructor :: !Bool
  }
  deriving (Show)

data Qualified = Qualified
  { mod :: Module
  , name :: Name
  , node :: H.Qualified
  }
  deriving (Show)

data Module = Module
  { parts :: NonEmpty Text
  , text :: Text
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

data ImportName = ImportName
  { name :: Text
  }
  deriving (Show, Eq)

data Import = Import
  { mod :: Module
  , alias :: Maybe Module
  , qualified :: !Bool
  , hiding :: !Bool
  , importList :: [ImportItem]
  }
  deriving (Show)

pattern OpenImport :: Module -> Import
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

parseModuleFromText :: Text -> Module
parseModuleFromText text =
  Module
    { parts = NE.fromList (T.splitOn "." text)
    , text
    }

importQualifier :: Import -> Module
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
  _ -> Left $ "could not parse namespace: " <> T.pack (show n)

parseImportOperator :: H.PrefixId -> AST.Err Name
parseImportOperator operator = do
  operator <- operator.children
  parseName <$> removeQualified operator

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
  _ -> undefined

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

parseImportList :: H.ImportList -> AST.Err [ImportItem]
parseImportList i = do
  name <- AST.collapseErr i.name
  items <- traverse parseImportItem name
  pure items

parseModule :: H.Module -> AST.Err Module
parseModule m = do
  ids <- AST.collapseErr m.children
  pure $
    Module
      { text =
          -- the text sometimes includes trailing dots
          T.dropWhileEnd (== '.') (AST.nodeToText m)
      , parts = fmap AST.nodeToText ids
      }

parseImport :: H.Import -> AST.Err Import
parseImport i = do
  mod <- i.module'
  mod <- parseModule mod
  alias <- AST.collapseErr i.alias
  alias <- traverse parseModule alias
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

parseQualified :: H.Qualified -> AST.Err Qualified
parseQualified q = do
  mod <- q.module'
  mod <- parseModule mod
  name <- q.id
  let name' = AST.subset @_ @ParseNameTypes name
  name <- pure $ parseName $ name'
  pure $ Qualified {mod, name, node = q}

getQualifiedAtPoint :: Range -> H.Haskell -> AST.Err (Maybe Qualified)
getQualifiedAtPoint range h = do
  let node = AST.getDeepestContaining @H.Qualified range (AST.getDynNode h)
  qualified <- traverse parseQualified node
  pure qualified

parseImports :: H.Imports -> AST.Err ([Text], [Import])
parseImports i = do
  import' <- i.import'
  let (es, imports) = Either.partitionEithers (NE.toList import')
  imports <- pure $ parseImport <$> imports
  let (es', imports') = Either.partitionEithers imports
  pure (es ++ es', imports')

data Program = Program
  { imports :: [Import]
  }
  deriving (Show)

emptyProgram :: Program
emptyProgram = Program {imports = []}

parseHaskell :: H.Haskell -> ([Text], Program)
parseHaskell h = do
  let res = do
        imports <- AST.collapseErr h.imports
        (es, imports) <- case imports of
          Nothing -> pure ([], [])
          Just imports -> parseImports imports
        pure (es, Program {imports})
  case res of
    Right (es, program) -> (es, program)
    Left e -> ([e], emptyProgram)

type GetNameTypes =
  Haskell.Name
    :+ Haskell.Constructor
    :+ Haskell.Qualified
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
