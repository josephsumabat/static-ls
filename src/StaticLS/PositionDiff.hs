{-# LANGUAGE MultiWayIf #-}

module StaticLS.PositionDiff
  ( Token (..),
    mkToken,
    lex,
    diffText,
    updatePositionUsingDiff,
    DiffMap,
    updatePositionMap,
  )
where

import Data.Diff qualified as Diff
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Pos
import Data.Range (Range (..))
import Data.RangeMap (RangeMap)
import Data.RangeMap qualified as RangeMap
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.Lexer qualified as Lexer
import Prelude hiding (lex)

data Token = Token
  { text :: !Text,
    len :: !Int
  }
  deriving (Eq, Show)

mkToken :: Text -> Token
mkToken text = Token {text, len = T.length text}

lex :: String -> [Token]
lex source =
  fmap
    ( \(_tok, (_pos, s)) ->
        let text = T.pack s
         in Token {text, len = T.length text}
    )
    (Lexer.lexerPass0 source)

type TokenDiff = [Diff.Elem Token]

diffText :: Text -> Text -> TokenDiff
diffText x y =
  Diff.diff (lex (T.unpack x)) (lex (T.unpack y))

-- staged with diff
updatePositionUsingDiff :: TokenDiff -> Pos -> Pos
updatePositionUsingDiff diff =
  let dm = getDiffMap diff
   in \pos -> updatePositionMap pos dm

data Delta
  = -- | Any position in the range gets the same delta applied
    SimpleDelta !Int
  | -- | Positions in the range get moved to the start of the range
    -- and then get the delta applied
    DeleteDelta !Int
  deriving (Show, Eq)

getDelta :: Delta -> Int
getDelta (SimpleDelta d) = d
getDelta (DeleteDelta d) = d

applyLastDelta :: Pos -> Range -> Delta -> Pos
applyLastDelta (Pos pos) range delta = case delta of
  SimpleDelta d -> Pos (pos + d)
  DeleteDelta d -> Pos (range.start.pos - 1 + d)

-- invariant: range must contain pos
applyDelta :: Pos -> Range -> Delta -> Pos
applyDelta (Pos pos) range delta = case delta of
  SimpleDelta d -> Pos (pos + d)
  DeleteDelta d -> Pos (range.start.pos + d)

-- invariant: returns ranges that are contiguous
getDeltaList :: TokenDiff -> [(Range, Delta)]
getDeltaList diff = go diff 0 0
  where
    go diff !delta !pos = case diff of
      [] -> []
      (d : ds) -> case d of
        -- insertions are not part of the original text
        Diff.Insert t -> go ds (delta + t.len) pos
        -- keeps are part of the original text
        Diff.Keep t -> (Range (Pos pos) (Pos (pos + t.len)), SimpleDelta delta) : go ds delta (pos + t.len)
        -- See 'Delta' documentation
        Diff.Delete t -> (Range (Pos pos) (Pos (pos + t.len)), DeleteDelta delta) : go ds (delta - t.len) (pos + t.len)

getDiffMap :: TokenDiff -> DiffMap
getDiffMap diff =
  DiffMap
    { map = RangeMap.fromList deltaList,
      last = NE.last <$> NE.nonEmpty deltaList
    }
  where
    deltaList = (getDeltaList diff)

data DiffMap = DiffMap
  { map :: !(RangeMap Delta),
    last :: (Maybe (Range, Delta))
  }

updatePositionMap :: Pos -> DiffMap -> Pos
updatePositionMap pos DiffMap {map, last} =
  case last of
    Nothing -> pos
    Just (finalRange, finalDelta) ->
      if
        | finalRange.end <= pos -> Pos (pos.pos + getDelta finalDelta)
        | finalRange.start <= pos -> applyLastDelta pos finalRange finalDelta
        | otherwise ->
            -- since the ranges are contiguous, there must be a range that contains the position
            -- we need to do the min incase the position was on a delete that was the last diff element
            apply $ Maybe.fromJust $ RangeMap.lookupWith pos map
  where
    apply (r, d) = applyDelta pos r d
