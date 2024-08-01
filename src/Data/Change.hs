module Data.Change (
  Change (..),
  insert,
  delete,
  replace,
)
where

import Data.Pos (Pos)
import Data.Range (Range)
import Data.Range qualified as Range
import Data.Text (Text)
import Data.Text qualified as T

data Change = Change
  { insert :: !Text
  , delete :: !Range
  }
  deriving (Show, Eq, Ord)

insert :: Pos -> Text -> Change
insert pos text = Change {insert = text, delete = Range.empty pos}

delete :: Range -> Change
delete r = Change {insert = T.empty, delete = r}

replace :: Range -> Text -> Change
replace r text = Change {insert = text, delete = r}
