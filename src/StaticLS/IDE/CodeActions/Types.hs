{-# LANGUAGE TemplateHaskell #-}

module StaticLS.IDE.CodeActions.Types where

import Data.Aeson.TH
import Data.Path (AbsPath)
import Data.Text
import Language.LSP.Protocol.Types (Range (..), TextDocumentIdentifier (..))
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.StaticLsEnv

data Context = Context
  { textDocument :: !TextDocumentIdentifier
  , range :: !Range
  }

data CodeActionMessageKind
  = AutoImportActionMessage !Text
  | NoMessage

data CodeActionMessage = CodeActionMessage
  { kind :: !CodeActionMessageKind
  , path :: !AbsPath
  }

data GlobalCodeAction = GlobalCodeAction
  { name :: !Text
  , run :: Context -> StaticLsM (Maybe ())
  }

$(deriveJSON defaultOptions ''CodeActionMessageKind)
$(deriveJSON defaultOptions ''CodeActionMessage)

data Assist = Assist
  { label :: !Text
  , sourceEdit :: Either SourceEdit CodeActionMessage
  }

mkAssist :: Text -> SourceEdit -> Assist
mkAssist label sourceEdit =
  Assist
    { label
    , sourceEdit = Left sourceEdit
    }

mkLazyAssist :: Text -> CodeActionMessage -> Assist
mkLazyAssist label sourceEdit =
  Assist
    { label
    , sourceEdit = Right sourceEdit
    }
