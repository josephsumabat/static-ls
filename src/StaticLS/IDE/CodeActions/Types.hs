{-# LANGUAGE TemplateHaskell #-}

module StaticLS.IDE.CodeActions.Types where

import Data.Aeson.TH
import Data.LineCol (LineCol)
import Data.Path (AbsPath)
import Data.Pos (Pos)
import Data.Text
import StaticLS.IDE.SourceEdit (SourceEdit)

data CodeActionMessageKind
  = AutoImportActionMessage !Text
  | NoMessage
  deriving (Show, Eq)

data CodeActionMessage = CodeActionMessage
  { kind :: !CodeActionMessageKind,
    path :: !AbsPath
  }
  deriving (Show, Eq)

data CodeActionContext = CodeActionContext
  { path :: !AbsPath,
    pos :: !Pos,
    lineCol :: !LineCol
  }

$(deriveJSON defaultOptions ''CodeActionMessageKind)
$(deriveJSON defaultOptions ''CodeActionMessage)

data Assist = Assist
  { label :: !Text,
    sourceEdit :: Either SourceEdit CodeActionMessage
  }
  deriving (Show, Eq)

mkAssist :: Text -> SourceEdit -> Assist
mkAssist label sourceEdit =
  Assist
    { label,
      sourceEdit = Left sourceEdit
    }

mkLazyAssist :: Text -> CodeActionMessage -> Assist
mkLazyAssist label sourceEdit =
  Assist
    { label,
      sourceEdit = Right sourceEdit
    }
