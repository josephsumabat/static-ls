module Data.Rope (
  Rope,
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
  splitAt,
  getLine,
  uncheckedGetLine,
  linesLength,
  isValidLineCol,
  isValidLineColEnd,
  indexRange,
)
where

import Data.Change (Change (..))
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.Foldable qualified as Foldable
import Data.LineColRange (LineColRange (..))
import Data.Pos (LineCol (..), Pos (..))
import Data.Range (Range (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Lines as Rope (Position (..))
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Prelude hiding (getLine, length, splitAt)

newtype Rope = Rope {rope :: Rope.Rope}
  deriving (Show, Eq, Ord, Semigroup, Monoid, IsString)

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

-- TODO: this should be tested
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
-- apply changes in reverse order
edit (reverse . Edit.getChanges -> changes) rope = Foldable.foldl' (flip change) rope changes

-- TODO: return a maybe
splitAt :: LineCol -> Rope -> (Rope, Rope)
splitAt (LineCol line col) (Rope rope) = (Rope before, Rope after)
 where
  (before, after) =
    Rope.charSplitAtPosition
      ( Rope.Position
          { posLine = (fromIntegral line)
          , posColumn = (fromIntegral col)
          }
      )
      rope

indexRange :: Rope -> Range -> Maybe Rope
indexRange (Rope r) (Range (Pos start) (Pos end))
  | start < fromIntegral (Rope.charLength r) && end < fromIntegral (Rope.charLength r) =
      Just (Rope indexed)
  | otherwise = Nothing
 where
  (_beforeStart, afterStart) = Rope.charSplitAt (fromIntegral start) r
  (indexed, _) = Rope.charSplitAt (fromIntegral (end - start)) afterStart

isValidLineCol :: Rope -> LineCol -> Bool
isValidLineCol r (LineCol line col) =
  line >= 0
    && col >= 0
    && line < fromIntegral @Word @Int (Rope.lengthInLines rope)
    && col < length (uncheckedGetLine r (Pos line))
 where
  rope = r.rope

-- may point to the end of something in an exclusive range
isValidLineColEnd :: Rope -> LineCol -> Bool
isValidLineColEnd r (LineCol line col) =
  line >= 0
    && col >= 0
    && line < fromIntegral @Word @Int (Rope.lengthInLines rope)
    && col <= length (uncheckedGetLine r (Pos line))
 where
  rope = r.rope
linesLength :: Rope -> Int
linesLength (Rope rope) = fromIntegral . Rope.lengthInLines $ rope

-- | get the line, including the newline!
getLine :: Rope -> Pos -> Maybe Rope
getLine rope line
  | line.pos < fromIntegral (Rope.lengthInLines rope.rope) = Just $! uncheckedGetLine rope line
  | otherwise = Nothing

uncheckedGetLine :: Rope -> Pos -> Rope
uncheckedGetLine (Rope rope) (Pos line) =
  Rope theLine
 where
  (_before, after) = Rope.splitAtLine (fromIntegral line) rope
  (theLine, _) = Rope.splitAtLine 1 after
