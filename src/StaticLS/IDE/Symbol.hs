module StaticLS.IDE.Symbol (Symbol (..), SymbolKind (..)) where

import Data.Text (Text)
import StaticLS.IDE.FileWith (FileLcRange)

data Symbol = Symbol
  { name :: !Text
  , kind :: !SymbolKind
  , loc :: !FileLcRange
  }
  deriving (Show, Eq)

data SymbolKind
  = Variable
  | Function
  | Type
  | Class
  | Constructor
  | Field
  deriving (Show, Eq)
