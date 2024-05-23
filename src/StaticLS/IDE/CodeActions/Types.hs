{-# LANGUAGE TemplateHaskell #-}

module StaticLS.IDE.CodeActions.Types where

import Data.Text
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import Language.LSP.Protocol.Types (TextDocumentIdentifier(..), Range(..))
import Data.Aeson.TH

data Context = Context {
  textDocument :: !TextDocumentIdentifier,
  range :: !Range
  }

data CodeActionMessageKind
  = GlobalActionMessage !Int
  | AutoImportActionMessage !Text
  
data CodeActionMessage = CodeActionMessage {
  kind :: !CodeActionMessageKind,
  tdi :: !TextDocumentIdentifier
  }


data GlobalCodeAction = GlobalCodeAction {
  name :: !Text,
  run :: Context -> StaticLs (Maybe ())
  }

$(deriveJSON defaultOptions ''CodeActionMessageKind)
$(deriveJSON defaultOptions ''CodeActionMessage)