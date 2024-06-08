module Data.Rope
  ( Rope,
    fromTextRope,
    toTextRope,
    fromText,
    toText,
    length,
    posToLineCol,
    lineColToPos,
    rangeToLineColRange,
    lineColRangeToRange,
    change,
    edit,
  )
where

import Data.Change (Change (..))
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.Foldable qualified as Foldable
import Data.LineColRange (LineColRange (..))
import Data.Pos (LineCol (..), Pos (..))
import Data.Range (Range (..))
import Data.Text (Text)
import Data.Text.Lines as Rope (Position (..))
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Prelude hiding (length)

newtype Rope = Rope {rope :: Rope.Rope}
  deriving (Show, Eq, Ord)

fromTextRope :: Rope.Rope -> Rope
fromTextRope = Rope

toTextRope :: Rope -> Rope.Rope
toTextRope = (.rope)

fromText :: Text -> Rope
fromText = Rope . Rope.fromText

toText :: Rope -> Text
toText = Rope.toText . (.rope)

length :: Rope -> Int
length = fromIntegral . Rope.charLength . (.rope)

posToLineCol :: Rope -> Pos -> LineCol
posToLineCol r (Pos pos) =
  LineCol
    (fromIntegral (Rope.posLine ropePos))
    (fromIntegral (Rope.posColumn ropePos))
  where
    ropePos = Rope.charLengthAsPosition beforePos
    rope = r.rope
    (beforePos, _afterPos) = Rope.charSplitAt (fromIntegral pos) rope

lineColToPos :: Rope -> LineCol -> Pos
lineColToPos r (LineCol line col) =
  Pos (fromIntegral (Rope.charLength beforeLineCol))
  where
    (beforeLineCol, _) = Rope.charSplitAtPosition ropePos rope
    rope = r.rope
    ropePos = Rope.Position (fromIntegral line) (fromIntegral col)

rangeToLineColRange :: Rope -> Range -> LineColRange
rangeToLineColRange r (Range start end) =
  LineColRange (posToLineCol r start) (posToLineCol r end)

lineColRangeToRange :: Rope -> LineColRange -> Range
lineColRangeToRange r (LineColRange start end) =
  Range (lineColToPos r start) (lineColToPos r end)

change :: Change -> Rope -> Rope
change Change {insert, delete} (Rope rope) =
  Rope (beforeStart <> Rope.fromText insert <> afterEnd)
  where
    (beforeStart, afterStart) = Rope.charSplitAt (fromIntegral delete.start.pos) rope
    (_, afterEnd) = Rope.charSplitAt (fromIntegral delete.end.pos) afterStart
    _ = undefined

edit :: Edit -> Rope -> Rope
edit (Edit.getChanges -> changes) rope = Foldable.foldl' (flip change) rope changes
