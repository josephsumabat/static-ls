module StaticLS.IDE.DocumentSymbols where

import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.StaticEnv

getDocumentSymbols :: LSP.Uri -> StaticLs [LSP.DocumentSymbol]
getDocumentSymbols uri = do
  haskell <- getHaskell uri
  pure []
