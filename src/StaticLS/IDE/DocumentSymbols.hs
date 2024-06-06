module StaticLS.IDE.DocumentSymbols (getDocumentSymbols) where

import AST qualified
import AST.Haskell qualified as Haskell
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Sum
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.HIE (astRangeToLspRange)
import StaticLS.Logger
import StaticLS.StaticLsEnv

queryDeclarations :: Haskell.Haskell -> AST.Err [Haskell.Declaration]
queryDeclarations hs = do
  decls <- AST.collapseErr hs.declarations
  decls <- AST.maybeToErr "No declarations found" decls
  decls <- AST.collapseErr decls.children
  let declarations =
        Maybe.mapMaybe
          ( \decl -> case decl of
              Inj @Haskell.Declaration decl -> Just decl
              _ -> Nothing
          )
          (NE.toList decls)
  pure declarations

declarationToSymbol :: Haskell.Declaration -> AST.Err [LSP.DocumentSymbol]
declarationToSymbol decl =
  case decl.getDeclaration of
    Inj @Haskell.Decl decl -> declToSymbol decl
    Inj @Haskell.DataType dataType -> dataTypeToSymbol dataType
    Inj @Haskell.Newtype newtype_ -> newtypeToSymbol newtype_
    Inj @Haskell.Class class_ -> classToSymbol class_
    Inj @Haskell.TypeSynomym typeSynonym -> typeSynonymToSymbol typeSynonym
    Inj @Haskell.TypeFamily typeFamily -> typeFamilyToSymbol typeFamily
    _ -> pure []

typeFamilyToSymbol :: Haskell.TypeFamily -> AST.Err [LSP.DocumentSymbol]
typeFamilyToSymbol typeFamily = do
  name <- AST.collapseErr typeFamily.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkDocumentSymbol
        (nodeToText name)
        LSP.SymbolKind_Struct
        (astRangeToLspRange $ AST.nodeToRange typeFamily)
        (astRangeToLspRange $ AST.nodeToRange name)

typeSynonymToSymbol :: Haskell.TypeSynomym -> AST.Err [LSP.DocumentSymbol]
typeSynonymToSymbol typeSynonym = do
  name <- AST.collapseErr typeSynonym.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkDocumentSymbol
        (nodeToText name)
        LSP.SymbolKind_Struct
        (astRangeToLspRange $ AST.nodeToRange typeSynonym)
        (astRangeToLspRange $ AST.nodeToRange name)

newtypeToSymbol :: Haskell.Newtype -> AST.Err [LSP.DocumentSymbol]
newtypeToSymbol newtype_ = do
  name <- AST.collapseErr newtype_.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkDocumentSymbol
        (nodeToText name)
        LSP.SymbolKind_Struct
        (astRangeToLspRange $ AST.nodeToRange newtype_)
        (astRangeToLspRange $ AST.nodeToRange name)

classToSymbol :: Haskell.Class -> AST.Err [LSP.DocumentSymbol]
classToSymbol class_ = do
  name <- AST.collapseErr class_.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkDocumentSymbol
        (nodeToText name)
        LSP.SymbolKind_Interface
        (astRangeToLspRange $ AST.nodeToRange class_)
        (astRangeToLspRange $ AST.nodeToRange name)

dataTypeToSymbol :: Haskell.DataType -> AST.Err [LSP.DocumentSymbol]
dataTypeToSymbol dataType = do
  name <- AST.collapseErr dataType.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkDocumentSymbol
        (nodeToText name)
        LSP.SymbolKind_Struct
        (astRangeToLspRange $ AST.nodeToRange dataType)
        (astRangeToLspRange $ AST.nodeToRange name)

declToSymbol :: Haskell.Decl -> AST.Err [LSP.DocumentSymbol]
declToSymbol decl =
  case decl.getDecl of
    Inj @Haskell.Bind bind -> do
      name <- AST.collapseErr bind.name
      pure $ Foldable.toList $ do
        name <- name
        Just $
          mkDocumentSymbol
            (nodeToText name)
            LSP.SymbolKind_Function
            (astRangeToLspRange $ AST.nodeToRange decl)
            (astRangeToLspRange $ AST.nodeToRange name)
    Inj @Haskell.Function fun -> do
      name <- AST.collapseErr fun.name
      pure $ Foldable.toList $ do
        name <- name
        Just $
          mkDocumentSymbol
            (nodeToText name)
            LSP.SymbolKind_Function
            (astRangeToLspRange $ AST.nodeToRange decl)
            (astRangeToLspRange $ AST.nodeToRange name)
    _ -> pure []

-- TODO: check invariant that selectionRange is contained in range
mkDocumentSymbol :: Text -> LSP.SymbolKind -> LSP.Range -> LSP.Range -> LSP.DocumentSymbol
mkDocumentSymbol name kind range selectionRange =
  LSP.DocumentSymbol
    { LSP._name = name
    , LSP._detail = Nothing
    , LSP._kind = kind
    , LSP._tags = Nothing
    , LSP._deprecated = Nothing
    , LSP._range = range
    , LSP._selectionRange = selectionRange
    , LSP._children = Nothing
    }

nodeToText :: (AST.HasDynNode n) => n -> Text
nodeToText = AST.nodeText . AST.getDynNode

getDocumentSymbols :: AbsPath -> StaticLsM [LSP.DocumentSymbol]
getDocumentSymbols path = do
  logInfo "get document symbols"
  logInfo $ T.pack $ "uri: " ++ show path
  haskell <- getHaskell path
  let documentSymbolsRes = do
        decls <- queryDeclarations haskell
        let symbols = Maybe.mapMaybe (eitherToMaybe . declarationToSymbol) decls
        symbols <- pure $ concat symbols
        pure symbols
  case documentSymbolsRes of
    Left e -> do
      logError $ "e: " <> e
      pure []
    Right documentSymbols -> do
      pure documentSymbols
