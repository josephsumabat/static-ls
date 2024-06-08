{-# LANGUAGE TemplateHaskell #-}

module StaticLS.IDE.CodeActions.Types where

import Data.Aeson.TH
import Data.Path (AbsPath)
import Data.Text
import Language.LSP.Protocol.Types (Range (..), TextDocumentIdentifier (..))
import StaticLS.StaticLsEnv

data Context = Context
  { textDocument :: !TextDocumentIdentifier,
    range :: !Range
  }

data CodeActionMessageKind
  = GlobalActionMessage !Int
  | AutoImportActionMessage !Text
  | NoMessage

data CodeActionMessage = CodeActionMessage
  { kind :: !CodeActionMessageKind,
    path :: !AbsPath
  }

data GlobalCodeAction = GlobalCodeAction
  { name :: !Text,
    run :: Context -> StaticLsM (Maybe ())
  }

data CodeAction = CodeAction
  {
  }

$(deriveJSON defaultOptions ''CodeActionMessageKind)
$(deriveJSON defaultOptions ''CodeActionMessage)
