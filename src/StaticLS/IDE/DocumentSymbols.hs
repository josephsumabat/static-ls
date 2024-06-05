module StaticLS.IDE.DocumentSymbols (getDocumentSymbols) where

import AST qualified
import AST.Haskell qualified as Haskell
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
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
    Inj @Haskell.DataType _dataType -> pure []
    _ -> pure []

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
    { LSP._name = name,
      LSP._detail = Nothing,
      LSP._kind = kind,
      LSP._tags = Nothing,
      LSP._deprecated = Nothing,
      LSP._range = range,
      LSP._selectionRange = selectionRange,
      LSP._children = Nothing
    }

nodeToText :: (AST.HasDynNode n) => n -> Text
nodeToText = AST.nodeText . AST.getDynNode

getDocumentSymbols :: LSP.Uri -> StaticLsM [LSP.DocumentSymbol]
getDocumentSymbols uri = do
  logInfo "get document symbols"
  logInfo $ T.pack $ "uri: " ++ show uri
  haskell <- getHaskell uri
  let documentSymbolsRes = do
        decls <- queryDeclarations haskell
        symbols <- traverse declarationToSymbol decls
        symbols <- pure $ concat symbols
        pure symbols
  case documentSymbolsRes of
    Left e -> do
      logError $ "e: " <> e
      pure []
    Right documentSymbols -> do
      pure documentSymbols
