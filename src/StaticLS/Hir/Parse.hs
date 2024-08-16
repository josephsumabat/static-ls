{-# LANGUAGE QuasiQuotes #-}

module StaticLS.Hir.Parse where

import AST (DynNode)
import AST qualified
import AST.Haskell qualified as H
import AST.Sum ((:+))
import Control.Applicative (asum, (<|>))
import Control.Error (hush)
import Control.Error qualified as Error
import Control.Error.Util (note)
import Control.Monad (guard)
import Data.Either qualified as Either
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Range (Range)
import Data.Text (Text)
import Data.Text qualified as T
import StaticLS.Hir.Types

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

parseNamePrefix :: (H.PrefixId :+ H.PrefixList :+ H.Unit :+ ParseNameTypes) -> AST.Err Name
parseNamePrefix node =
  case node of
    [AST.x1|prefixId|] -> do
      prefixId <- AST.unwrap prefixId
      parseName <$> removeQualified prefixId.children
    [AST.x2|prefixList|] -> do
      pure
        Name
          { node = prefixList.dynNode
          , isOperator = True
          , isConstructor = True
          }
    [AST.x3|unit|] -> do
      pure
        Name
          { node = unit.dynNode
          , isOperator = False
          , isConstructor = True
          }
    [AST.rest3|name|] -> pure $ parseName name

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

removeQualified :: forall n n'. (AST.Subset n (H.Qualified :+ n')) => n -> AST.Err n'
removeQualified n = case AST.subset @_ @(H.Qualified :+ n') n of
  AST.X qualified -> Left $ "qualified name in import: " <> qualified.dynNode.nodeText
  AST.Rest name -> pure name

type ParseImportChildren = H.Qualified :+ H.AllNames :+ H.AssociatedType :+ H.PrefixId :+ ParseNameTypes

parseImportChild :: ParseImportChildren -> AST.Err ImportChildren
parseImportChild child = case child of
  [AST.x1|qualified|] -> Left $ "qualified name in import children: " <> qualified.dynNode.nodeText
  [AST.x2|_allNames|] -> pure ImportAllChildren
  [AST.x3|assocType|] -> do
    type' <- assocType.type'
    name <- removeQualified type'
    pure $ ImportChild NameSpaceType (parseName name)
  [AST.x4|prefixId|] -> do
    operator <- prefixId.children
    name <- removeQualified operator
    pure $ ImportChild NameSpaceValue (parseName name)
  [AST.rest4|rest|] -> pure $ ImportChild NameSpaceValue (parseName rest)

parseExportChild :: ParseImportChildren -> AST.Err ExportChildren
parseExportChild child = case child of
  [AST.x1|qualified|] -> do
    name <- parseQualified (AST.Inj qualified)
    pure $ ExportChild NameSpaceValue name
  [AST.x2|_allNames|] -> pure ExportAllChildren
  [AST.x3|assocType|] -> do
    type' <- assocType.type'
    name <- parseQualified $ AST.subset type'
    pure $ ExportChild NameSpaceType name
  [AST.x4|prefixId|] -> do
    operator <- prefixId.children
    name <- parseQualified $ AST.subset operator
    pure $ ExportChild NameSpaceValue name
  [AST.rest4|rest|] -> do
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
  e <- AST.unwrap e
  namespace <- traverse parseNameSpace e.namespace
  namespace <- pure $ Maybe.fromMaybe NameSpaceValue namespace
  name <- do
    operator <- traverse parseExportOperator e.operator
    type' <- traverse (parseQualified . AST.subset) e.type'
    variable <- traverse (parseQualified . AST.subset) e.variable
    case operator <|> type' <|> variable of
      Just n -> pure n
      Nothing -> Left "could not parse import name"
  children <- traverse parseExportChildren e.children'
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
        (AST.getDeepestContaining @ParseQualifiedTypes range h.dynNode)
    Just node -> Just <$> parseQualified (AST.Inj node)

parseImports :: H.Imports -> AST.Err ([Text], [Import])
parseImports i = do
  import' <- i.import'
  let (es, imports) = Either.partitionEithers (NE.toList import')
  imports <- pure $ parseImport <$> imports
  let (es', imports') = Either.partitionEithers imports
  pure (es ++ es', imports')

parseDataType :: H.DataType -> AST.Err DataDecl
parseDataType node = do
  dt <- AST.unwrap node
  name <- Error.note "no name for data type" dt.name
  name <- parseNamePrefix =<< removeQualified name
  pure DataDecl {name, node}

parseBind :: H.Decl -> AST.Err (Decl)
parseBind decl = do
  case decl.getDecl of
    [AST.x1|bindNode|] -> do
      bind <- AST.unwrap bindNode
      name <- Error.note "no bind name" bind.name
      name <- parseNamePrefix $ AST.subset name
      pure $ DeclBind BindDecl {name, node = AST.Inj bindNode}
    [AST.x2|fnNode|] -> do
      fn <- AST.unwrap fnNode
      name <- Error.note "no function name" fn.name
      name <- parseNamePrefix $ AST.subset name
      pure $ DeclBind BindDecl {name, node = AST.Inj fnNode}
    [AST.x3|sigNode|] -> do
      sig <- AST.unwrap sigNode
      name <- Error.note "no signature name" sig.name
      name <- parseNamePrefix $ AST.subset name
      pure $ DeclSig SigDecl {name, node = sigNode}
    [AST.rest3|nil|] -> case nil of {}

parseClass :: H.Class -> AST.Err (Decl)
parseClass c = do
  cu <- AST.unwrap c
  name <- note "no name for class" cu.name
  name <- parseNamePrefix $ AST.subset name
  pure $ DeclClass ClassDecl {name, node = c}

parseDataFamily :: H.DataFamily -> AST.Err (Decl)
parseDataFamily d = do
  du <- AST.unwrap d
  name <- note "no name for data family" du.name
  name <- parseNamePrefix $ AST.subset name
  pure $ DeclDataFamily DataFamilyDecl {name, node = d}

parseNewtype :: H.Newtype -> AST.Err (Decl)
parseNewtype n = do
  nu <- AST.unwrap n
  name <- note "no name for newtype" nu.name
  name <- parseNamePrefix =<< removeQualified name
  pure $ DeclNewtype NewtypeDecl {name, node = n}

parseBindingList :: H.BindingList -> AST.Err [Name]
parseBindingList bs = do
  bsu <- AST.unwrap bs
  let names = NE.toList bsu.name
  traverse (parseNamePrefix . AST.subset) names

parsePattern :: H.PatternSynonym -> AST.Err ([Decl])
parsePattern p = do
  p <- p.children
  case p of
    AST.Inj @H.Equation e -> do
      eu <- AST.unwrap e
      synonym <- note "no synonym" eu.synonym
      res <- note "no name found" $ AST.getDeepestSatisfying (AST.cast @ParseNameTypes) (AST.getDynNode synonym)
      let name = parseName res
      pure [DeclPattern PatternDecl {name, node = e}]
    AST.Inj @H.Signature s -> do
      su <- AST.unwrap s
      case su.names of
        Just bindingList -> do
          names <- parseBindingList bindingList
          pure $ fmap (\name -> DeclPatternSig PatternSigDecl {name, node = s}) names
        Nothing -> do
          synonym <- note "no synonym" su.synonym
          case synonym of
            AST.X bindingList -> do
              names <- parseBindingList bindingList
              pure $ fmap (\name -> DeclPatternSig PatternSigDecl {name, node = s}) names
            AST.Rest name -> do
              name <- parseNamePrefix =<< removeQualified name
              pure [DeclPatternSig PatternSigDecl {name, node = s}]
    _ -> pure []

parseTypeFamily :: H.TypeFamily -> AST.Err (Decl)
parseTypeFamily t = do
  tu <- AST.unwrap t
  name <- note "no name for type family" tu.name
  name <- parseNamePrefix =<< removeQualified name
  pure $ DeclTypeFamily TypeFamilyDecl {name, node = t}

parseTypeSynonym :: H.TypeSynomym -> AST.Err (Decl)
parseTypeSynonym t = do
  tu <- AST.unwrap t
  name <- note "no name for type synonym" tu.name
  name <- parseNamePrefix =<< removeQualified name
  pure $ DeclTypeSynonym TypeSynonymDecl {name, node = t}

parseDeclaration :: H.Declaration -> AST.Err ([Decl])
parseDeclaration decl = case decl.getDeclaration of
  AST.Inj @H.DataType d -> do
    (pure @[] . DeclData) <$> parseDataType d
  AST.Inj @H.Decl b -> pure @[] <$> parseBind b
  AST.Inj @H.Class c -> pure @[] <$> parseClass c
  AST.Inj @H.DataFamily d -> pure @[] <$> parseDataFamily d
  AST.Inj @H.Newtype n -> pure @[] <$> parseNewtype n
  AST.Inj @H.PatternSynonym p -> parsePattern p
  AST.Inj @H.TypeFamily t -> pure @[] <$> parseTypeFamily t
  AST.Inj @H.TypeSynomym t -> pure @[] <$> parseTypeSynonym t
  _ -> pure []

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
        let imports = Maybe.fromMaybe Nothing $ Error.hush $ AST.collapseErr h.imports
        (es, imports) <- case imports of
          Nothing -> pure ([], [])
          Just imports -> parseImports imports
        header <- AST.collapseErr h.children
        (es', exports) <- case header of
          Nothing -> pure (es, [])
          Just header -> do
            let exports = Maybe.fromMaybe Nothing $ Error.hush $ AST.collapseErr header.exports
            let exports' = Maybe.fromMaybe [] $ Maybe.fromMaybe Nothing $ Error.hush $ traverse parseExportList exports
            pure (es, exports')
        (es'', decls) <- do
          let decls = Maybe.fromMaybe Nothing $ Error.hush $ AST.collapseErr h.declarations
          case decls of
            Nothing -> pure ([], [])
            Just decls -> do
              -- let children =
              let children = Maybe.fromMaybe [] $ fmap NE.toList $ Error.hush $ AST.collapseErr decls.children
              let parseChild (child :: H.Declaration :+ H.Import :+ AST.Nil) = do
                    case child of
                      AST.X decl -> do
                        decl <- parseDeclaration decl
                        pure decl
                      AST.Rest (AST.X _imp) -> do
                        Left "cannot have import in declaration list"
                      AST.Rest (AST.Rest nil) -> case nil of {}
              let decls = fmap parseChild children
              let (es, decls') = Either.partitionEithers decls
              let decls'' = concat decls'
              pure (es, decls'')
        pure (es ++ es' ++ es'', Program {imports, exports, decls})
  case res of
    Right (es, program) -> (es, program)
    Left e -> ([e], emptyProgram)

getNameTypes :: Range -> H.Haskell -> Maybe GetNameTypes
getNameTypes range hs = AST.getDeepestContaining @GetNameTypes range hs.dynNode

parseThQuotedName :: H.ThQuotedName -> AST.Err ThQuotedName
parseThQuotedName thQuotedName = do
  name <- AST.collapseErr thQuotedName.name
  type' <- AST.collapseErr thQuotedName.type'
  case (ThQuotedName False . AST.getDynNode <$> name)
    <|> (ThQuotedName True . AST.getDynNode <$> type') of
    Just text -> pure text
    Nothing -> Left "ThQuotedName must have either a name or a type"

getPersistentModelAtPoint :: Range -> H.Haskell -> Maybe Text
getPersistentModelAtPoint range hs = do
  splice <- AST.getDeepestContaining @H.TopSplice range hs.dynNode
  _ <- AST.getDeepestSatisfying getMkModelApply splice.dynNode
  modelFileArg <- AST.getDeepestSatisfying getModelFileApply splice.dynNode
  (AST.Inj @H.Literal modelFileLit) <- pure modelFileArg.getExpression
  modelFileLit <- hush $ AST.unwrap modelFileLit
  (AST.Inj @H.String modelFileStr) <- pure modelFileLit.children
  let persistentModelName = modelFileStr.dynNode.nodeText
  persistentModelName <- T.stripPrefix "\"" persistentModelName
  persistentModelName <- T.stripSuffix "\"" persistentModelName
  pure persistentModelName
 where
  getMkModelApply :: DynNode -> Maybe H.Expression
  getMkModelApply = getApplyVarWithName "mkModel"

  getModelFileApply :: DynNode -> Maybe H.Expression
  getModelFileApply = getApplyVarWithName "modelFile"

  getApplyVarWithName :: Text -> DynNode -> Maybe H.Expression
  getApplyVarWithName name node = do
    apply <- AST.cast @H.Apply node
    apply <- hush $ AST.unwrap apply
    fun <- apply.function
    (AST.Inj @H.Expression funExpr) <- pure fun
    (AST.Inj @H.Variable funVar) <- pure funExpr.getExpression
    let funText = funVar.dynNode.nodeText
    guard $ funText == name
    (AST.Inj @H.Expression argExpr) <- pure apply.argument
    pure argExpr
