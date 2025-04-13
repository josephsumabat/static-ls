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

queryDeclarations :: Haskell.HaskellP -> AST.Err [Haskell.DeclarationP]
queryDeclarations hs = do
  decls <- AST.collapseErr hs.declarations
  decls <- AST.maybeToErr "No declarations found" decls
  decls <- AST.collapseErr decls.children
  let declarations =
        Maybe.mapMaybe
          ( \decl -> case decl of
              Inj @Haskell.DeclarationP decl -> Just decl
              _ -> Nothing
          )
          (NE.toList decls)
  pure declarations

declarationToSymbol :: Haskell.DeclarationP -> AST.Err [SymbolTree]
declarationToSymbol decl =
  case decl.getDeclaration of
    Inj @Haskell.DeclP decl -> declToSymbol decl
    Inj @Haskell.DataTypeP dataType -> dataTypeToSymbol dataType
    Inj @Haskell.NewtypeP newtype_ -> newtypeToSymbol newtype_
    Inj @Haskell.ClassP class_ -> classToSymbol class_
    Inj @Haskell.TypeSynomymP typeSynonym -> typeSynonymToSymbol typeSynonym
    Inj @Haskell.TypeFamilyP typeFamily -> typeFamilyToSymbol typeFamily
    _ -> pure []

typeFamilyToSymbol :: Haskell.TypeFamilyP -> AST.Err [SymbolTree]
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

typeSynonymToSymbol :: Haskell.TypeSynomymP -> AST.Err [SymbolTree]
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

newtypeToSymbol :: Haskell.NewtypeP -> AST.Err [SymbolTree]
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

classToSymbol :: Haskell.ClassP -> AST.Err [SymbolTree]
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

dataTypeToSymbol :: Haskell.DataTypeP -> AST.Err [SymbolTree]
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

declToSymbol :: Haskell.DeclP -> AST.Err [SymbolTree]
declToSymbol decl =
  case decl.getDecl of
    Inj @Haskell.BindP bind -> do
      name <- AST.collapseErr bind.name
      pure $ Foldable.toList $ do
        name <- name
        Just $
          mkSymbolTree
            (nodeToText name)
            SymbolKind.Function
            (AST.nodeToRange decl)
            (AST.nodeToRange name)
    Inj @Haskell.FunctionP fun -> do
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
