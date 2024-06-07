module StaticLS.IDE.SymbolKind (SymbolKind (..)) where

data SymbolKind
  = Variable
  | Function
  | Type
  | Class
  | Constructor
  | Field
  deriving (Show, Eq)
