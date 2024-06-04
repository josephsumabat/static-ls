{-# LANGUAGE TemplateHaskell #-}

module StaticLS.IDE.CodeActions.Types where

import Data.Aeson.TH
import Data.Text
import Language.LSP.Protocol.Types (Range (..), TextDocumentIdentifier (..))
import StaticLS.StaticLsEnv

data Context = Context
    { textDocument :: !TextDocumentIdentifier
    , range :: !Range
    }

data CodeActionMessageKind
    = GlobalActionMessage !Int
    | AutoImportActionMessage !Text

data CodeActionMessage = CodeActionMessage
    { kind :: !CodeActionMessageKind
    , tdi :: !TextDocumentIdentifier
    }

data GlobalCodeAction = GlobalCodeAction
    { name :: !Text
    , run :: Context -> StaticLsM (Maybe ())
    }

$(deriveJSON defaultOptions ''CodeActionMessageKind)
$(deriveJSON defaultOptions ''CodeActionMessage)
