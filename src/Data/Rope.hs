module Data.Rope (
  Rope,
  fromTextRope,
  fromTextRopeL,
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
  splitAtLineCol,
  getLine,
  uncheckedGetLine,
  linesLength,
  isValidLineCol,
  isValidLineColEnd,
  indexRange,
  empty,
  splitAt,
)
where

import Data.Change (Change (..))
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.Foldable qualified as Foldable
import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.Maybe
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Utf16.Rope.Mixed qualified as Rope16
import Data.Text.Utf8.Rope qualified as Rope8
import Prelude hiding (getLine, length, splitAt)

newtype Rope = Rope {rope :: Rope8.Rope}
  deriving (Show, Eq, Ord, Semigroup, Monoid, IsString)

empty :: Rope
empty = Rope mempty

fromTextRope :: Rope8.Rope -> Rope
fromTextRope = Rope

fromTextRopeL :: Rope16.Rope -> Rope
fromTextRopeL = fromText . Rope16.toText

toTextRope :: Rope -> Rope8.Rope
toTextRope = (.rope)

fromText :: Text -> Rope
fromText = Rope . Rope8.fromText

toText :: Rope -> Text
toText = Rope8.toText . (.rope)

length :: Rope -> Int
length = fromIntegral . Rope8.length . (.rope)

posToLineCol :: Rope -> Pos -> LineCol
posToLineCol r pos =
  LineCol
    (Pos (fromIntegral (Rope8.posLine ropePos)))
    (Pos (fromIntegral (Rope8.posColumn ropePos)))
 where
  ropePos = Rope8.lengthAsPosition beforePos
  rope = r.rope
  (beforePos, _afterPos) = splitAtR8 pos rope

lineColToPos :: Rope -> LineCol -> Pos
lineColToPos r (LineCol (Pos line) (Pos col)) =
  Pos (fromIntegral (Rope8.length beforeLineCol))
 where
  (beforeLineCol, _) = splitAtPositionR8 ropePos rope
  rope = r.rope
  ropePos = Rope8.Position (fromIntegral line) (fromIntegral col)

rangeToLineColRange :: Rope -> Range -> LineColRange
rangeToLineColRange r (Range start end) =
  LineColRange (posToLineCol r start) (posToLineCol r end)

-- TODO: this should be tested
lineColRangeToRange :: Rope -> LineColRange -> Range
lineColRangeToRange r (LineColRange start end) =
  Range (lineColToPos r start) (lineColToPos r end)

change :: Change -> Rope -> Rope
change Change {insert, delete} (Rope rope) =
  Rope (beforeStart <> Rope8.fromText insert <> afterEnd)
 where
  (beforeStart, afterStart) = splitAtR8 (delete.start) rope
  (_, afterEnd) = splitAtR8 (Pos (delete.end.pos - delete.start.pos)) afterStart

edit :: Edit -> Rope -> Rope
-- apply changes in reverse order
edit (reverse . Edit.getChanges -> changes) rope = Foldable.foldl' (flip change) rope changes

splitAt :: Pos -> Rope -> (Rope, Rope)
splitAt (Pos pos) (Rope rope) = (Rope before, Rope after)
 where
  (before, after) = splitAtR8 (Pos pos) rope

splitAtR8 :: Pos -> Rope8.Rope -> (Rope8.Rope, Rope8.Rope)
splitAtR8 (Pos pos) rope = do
  let initIdx = fromIntegral pos
  let try idx = Rope8.splitAt idx rope
  let candidatePositions = [initIdx, initIdx - 1 .. 0]
  case mapMaybe try candidatePositions of
    x : _ -> x
    [] -> (mempty, rope) -- should be unreachable, as one of the tried positions should split the string neatly

splitAtPositionR8 :: Rope8.Position -> Rope8.Rope -> (Rope8.Rope, Rope8.Rope)
splitAtPositionR8 (Rope8.Position initPL initPC) rope = do
  let positions = [Rope8.Position initPL newPC | newPC <- [initPC, initPC - 1 .. 0]]
  let try position = Rope8.splitAtPosition position rope
  case mapMaybe try positions of
    x : _ -> x
    [] -> (mempty, rope) -- should be unreachable, as one of the tried positions should split the string neatly

-- TODO: return a maybe
splitAtLineCol :: LineCol -> Rope -> (Rope, Rope)
splitAtLineCol (LineCol (Pos line) (Pos col)) (Rope rope) = (Rope before, Rope after)
 where
  (before, after) =
    splitAtPositionR8
      ( Rope8.Position
          { posLine = (fromIntegral line)
          , posColumn = (fromIntegral col)
          }
      )
      rope

indexRange :: Rope -> Range -> Maybe Rope
indexRange (Rope r) (Range (Pos start) (Pos end))
  | start
      < fromIntegral
        ( Rope8.length
            r
        )
      && end
        < fromIntegral
          ( Rope8.length
              r
          ) =
      Just (Rope indexed)
  | otherwise = Nothing
 where
  (_beforeStart, afterStart) = splitAtR8 (Pos start) r
  (indexed, _) = splitAtR8 (Pos (end - start)) afterStart

isValidLineCol :: Rope -> LineCol -> Bool
isValidLineCol r (LineCol (Pos line) (Pos col)) =
  line >= 0
    && col >= 0
    && line < fromIntegral @Word @Int (Rope8.lengthInLines rope)
    && col < length (uncheckedGetLine r (Pos line))
 where
  rope = r.rope

-- may point to the end of something in an exclusive range
isValidLineColEnd :: Rope -> LineCol -> Bool
isValidLineColEnd r (LineCol (Pos line) (Pos col)) =
  line >= 0
    && col >= 0
    && line < fromIntegral @Word @Int (Rope8.lengthInLines rope)
    && col <= length (uncheckedGetLine r (Pos line))
 where
  rope = r.rope
linesLength :: Rope -> Int
linesLength (Rope rope) = fromIntegral . Rope8.lengthInLines $ rope

-- | get the line, including the newline!
getLine :: Rope -> Pos -> Maybe Rope
getLine rope line
  | line.pos < fromIntegral (Rope8.lengthInLines rope.rope) = Just $! uncheckedGetLine rope line
  | otherwise = Nothing

uncheckedGetLine :: Rope -> Pos -> Rope
uncheckedGetLine (Rope rope) (Pos line) =
  Rope theLine
 where
  (_before, after) = Rope8.splitAtLine (fromIntegral line) rope
  (theLine, _) = Rope8.splitAtLine 1 after
