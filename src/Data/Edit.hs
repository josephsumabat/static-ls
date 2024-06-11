module Data.Edit (
  Edit,
  insert,
  delete,
  replace,
  changesToEdit,
  getChanges,
)
where

import Data.Change (Change)
import Data.Change qualified as Change
import Data.List qualified as List
import Data.Pos (Pos)
import Data.Range (Range (..))
import Data.Text (Text)

-- Invariant: disjoint, sorted by delete
data Edit = Edit [Change]
  deriving (Show, Eq)

insert :: Pos -> Text -> Edit
insert p t = Edit [Change.insert p t]

delete :: Range -> Edit
delete r = Edit [Change.delete r]

replace :: Range -> Text -> Edit
replace r t = Edit [Change.replace r t]

-- TODO: change if they are disjoint
changesToEdit :: [Change] -> Edit
changesToEdit = Edit . List.sortOn (\c -> (c.delete.start, c.delete.end))

getChanges :: Edit -> [Change]
getChanges (Edit cs) = cs
