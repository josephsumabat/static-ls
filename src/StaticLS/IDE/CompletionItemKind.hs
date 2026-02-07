module StaticLS.IDE.CompletionItemKind (CompletionItemKind (..)) where

data CompletionItemKind
  = Module
  | File
  | Text
  deriving (Show, Eq, Ord)
