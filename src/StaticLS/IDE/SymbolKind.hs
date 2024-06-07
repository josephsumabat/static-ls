module StaticLS.IDE.SymbolKind (SymbolKind (..)) where

data SymbolKind
  = Variable
  | Type
  | Class
  | Constructor
  deriving (Show, Eq)
