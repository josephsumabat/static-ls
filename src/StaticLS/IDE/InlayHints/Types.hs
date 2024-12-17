{- HLINT ignore "Use camelCase" -}

module StaticLS.IDE.InlayHints.Types (
  InlayHint (..),
  InlayHintLabelPart (..),
  Command (..),
  MarkupContent (..),
  InlayHintKind (..),
) where

import Data.Change
import Data.LineCol
import Data.Rope
import Data.Text
import StaticLS.IDE.FileWith

-- data type and constructors
data InlayHint = InlayHint
  { position :: LineCol
  , label :: Either Text [InlayHintLabelPart]
  , kind :: Maybe InlayHintKind
  , -- TODO add kind kind :: ??
    textEdits :: Maybe (Rope, [Change]) -- we need the rope to convert ranges in changes to lineColRanges
    -- TODO add tooltip
  , paddingLeft :: Maybe Bool
  , paddingRight :: Maybe Bool
  }

data InlayHintLabelPart = InlayHintLabelPart
  { value :: Text
  , tooltip :: Maybe (Either Text MarkupContent)
  , location :: Maybe FileLcRange
  , command :: Maybe Command
  }

-- May be implemented later
data Command

-- May be implemented later
data MarkupContent

data InlayHintKind = InlayHintKind_Type | InlayHintKind_Parameter
