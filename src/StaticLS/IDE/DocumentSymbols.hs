-- TODO: eventually move this into Hir
module StaticLS.IDE.DocumentSymbols (SymbolTree (..), getDocumentSymbols) where


import AST qualified
import AST.Haskell qualified as Haskell
import AST.Sum
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Range (Range)
import Data.Text (Text)
import StaticLS.IDE.Monad
import StaticLS.IDE.SymbolKind (SymbolKind)
import StaticLS.IDE.SymbolKind qualified as SymbolKind
import StaticLS.Logger
import StaticLS.Monad

data SymbolTree = SymbolTree
  { name :: !Text
  , kind :: !SymbolKind
  , range :: !Range
  , selectionRange :: !Range
  , children :: [SymbolTree]
  }
  deriving (Show)

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

declarationToSymbol :: Haskell.Declaration -> AST.Err [SymbolTree]
declarationToSymbol decl =
  case decl.getDeclaration of
    Inj @Haskell.Decl decl -> declToSymbol decl
    Inj @Haskell.DataType dataType -> dataTypeToSymbol dataType
    Inj @Haskell.Newtype newtype_ -> newtypeToSymbol newtype_
    Inj @Haskell.Class class_ -> classToSymbol class_
    Inj @Haskell.TypeSynomym typeSynonym -> typeSynonymToSymbol typeSynonym
    Inj @Haskell.TypeFamily typeFamily -> typeFamilyToSymbol typeFamily
    _ -> pure []

typeFamilyToSymbol :: Haskell.TypeFamily -> AST.Err [SymbolTree]
typeFamilyToSymbol typeFamily = do
  name <- AST.collapseErr typeFamily.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkSymbolTree
        (nodeToText name)
        SymbolKind.Type
        (AST.nodeToRange typeFamily)
        (AST.nodeToRange name)

typeSynonymToSymbol :: Haskell.TypeSynomym -> AST.Err [SymbolTree]
typeSynonymToSymbol typeSynonym = do
  name <- AST.collapseErr typeSynonym.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkSymbolTree
        (nodeToText name)
        SymbolKind.Type
        (AST.nodeToRange typeSynonym)
        (AST.nodeToRange name)

newtypeToSymbol :: Haskell.Newtype -> AST.Err [SymbolTree]
newtypeToSymbol newtype_ = do
  name <- AST.collapseErr newtype_.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkSymbolTree
        (nodeToText name)
        SymbolKind.Type
        (AST.nodeToRange newtype_)
        (AST.nodeToRange name)

classToSymbol :: Haskell.Class -> AST.Err [SymbolTree]
classToSymbol class_ = do
  name <- AST.collapseErr class_.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkSymbolTree
        (nodeToText name)
        SymbolKind.Class
        (AST.nodeToRange class_)
        (AST.nodeToRange name)

dataTypeToSymbol :: Haskell.DataType -> AST.Err [SymbolTree]
dataTypeToSymbol dataType = do
  name <- AST.collapseErr dataType.name
  pure $ Foldable.toList $ do
    name <- name
    Just $
      mkSymbolTree
        (nodeToText name)
        SymbolKind.Type
        (AST.nodeToRange dataType)
        (AST.nodeToRange name)

declToSymbol :: Haskell.Decl -> AST.Err [SymbolTree]
declToSymbol decl =
  case decl.getDecl of
    Inj @Haskell.Bind bind -> do
      name <- AST.collapseErr bind.name
      pure $ Foldable.toList $ do
        name <- name
        Just $
          mkSymbolTree
            (nodeToText name)
            SymbolKind.Function
            (AST.nodeToRange decl)
            (AST.nodeToRange name)
    Inj @Haskell.Function fun -> do
      name <- AST.collapseErr fun.name
      pure $ Foldable.toList $ do
        name <- name
        Just $
          mkSymbolTree
            (nodeToText name)
            SymbolKind.Function
            (AST.nodeToRange decl)
            (AST.nodeToRange name)
    _ -> pure []

-- TODO: check invariant that selectionRange is contained in range
mkSymbolTree :: Text -> SymbolKind -> Range -> Range -> SymbolTree
mkSymbolTree name kind range selectionRange =
  SymbolTree
    { name = name
    , kind = kind
    , range = range
    , selectionRange = selectionRange
    , children = []
    }

nodeToText :: (AST.HasDynNode n) => n -> Text
nodeToText = AST.nodeText . AST.getDynNode

getDocumentSymbols :: AbsPath -> StaticLsM [SymbolTree]
getDocumentSymbols path = do
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
