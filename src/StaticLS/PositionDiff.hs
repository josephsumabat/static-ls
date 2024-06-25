{-# LANGUAGE MultiWayIf #-}

module StaticLS.PositionDiff (
  Token (..),
  mkToken,
  lex,
  lexCooked,
  diffText,
  updatePositionUsingDiff,
  DiffMap,
  diffPos,
  diffLineCol,
  diffLineColRange,
  diffRange,
  getDiffMap,
  printDiffSummary,
  getDiffMapFromDiff,
  lexWithErrors,
  concatTokens,
  tokensToRangeMap,
)
where

import Data.Diff qualified as Diff
import Data.LineColRange (LineColRange (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Pos
import Data.Range (Range (..))
import Data.RangeMap (RangeMap)
import Data.RangeMap qualified as RangeMap
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Language.Haskell.Lexer qualified as Lexer
import Prelude hiding (lex)

data TokenKind where
  TokenKind :: forall a. (Show a, Eq a) => a -> TokenKind

deriving instance Show TokenKind

instance Eq TokenKind where
  TokenKind _ == TokenKind _ = True

data Token = Token
  { text :: !Text
  , len :: !Int
  , kind :: TokenKind
  }
  deriving (Eq, Show)

mkToken :: Text -> Token
mkToken text = Token {text, len = T.length text, kind = TokenKind False}

lexWithErrors :: String -> ([Token], [Text])
lexWithErrors s = (res, es)
 where
  es =
    Maybe.mapMaybe
      ( \(tok, (_, s)) -> case tok of
          Lexer.ErrorToken -> Just (T.pack s)
          _ -> Nothing
      )
      ts
  res =
    fmap
      ( \(tok, (_pos, s)) ->
          let text = T.pack s
           in Token {text, len = T.length text, kind = TokenKind tok}
      )
      ts
  ts = (Lexer.lexerPass0 s)

lex :: String -> [Token]
lex source =
  concatMap f ts
 where
  ts = Lexer.lexerPass0 source

  f (t, (p, s)) =
    case t of
      Lexer.ErrorToken -> onError
      Lexer.TheRest -> onError
      _ ->
        let text = T.pack s
         in [Token {text, len = T.length text, kind = TokenKind t}]
   where
    onError =
      case s of
        (c : cs) ->
          Token {text = T.singleton c, len = 1, kind = TokenKind t} : lex cs
        [] -> []

lexCooked :: String -> [Token]
lexCooked source =
  fmap f ts
 where
  ts = Lexer.lexerPass0 source

  f (t, (p, s)) =
    let text = T.pack s
     in Token {text, len = T.length text, kind = TokenKind t}

tokensWithRanges :: [Token] -> [(Range, Token)]
tokensWithRanges = go 0
 where
  go _pos [] = []
  go pos (t : ts) = (Range (Pos pos) (Pos (pos + t.len)), t) : go (pos + t.len) ts

tokensToRangeMap :: [Token] -> RangeMap Token
tokensToRangeMap = RangeMap.fromList . tokensWithRanges

type TokenDiff = [Diff.Elem Token]

printDiffSummary :: TokenDiff -> String
printDiffSummary diff = show $ (fmap . fmap) (.len) diff

concatTokens :: [Token] -> Token
concatTokens ts = Token {text = t, len = T.length t, kind = TokenKind ts}
 where
  t = T.concat $ map (.text) ts

diffText :: Text -> Text -> TokenDiff
diffText x y =
  (fmap . fmap) concatTokens $ Diff.diffMerged (lex (T.unpack x)) (lex (T.unpack y))

-- staged with diff
updatePositionUsingDiff :: TokenDiff -> Pos -> Pos
updatePositionUsingDiff diff =
  let dm = getDiffMapFromDiff diff
   in \pos -> diffPos pos dm

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
  -- We do a max 0 here because if there was only one delete,
  -- then the position would end up at zero, but we don't want it going to -1
  -- We subtract one normally because if it is the last delete,
  -- we want it to be at the end of the actually new source
  DeleteDelta d -> Pos (max 0 (range.start.pos - 1 + d))

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

getDiffMap :: Text -> Text -> DiffMap
getDiffMap x y = getDiffMapFromDiff (diffText x y)

getDiffMapFromDiff :: TokenDiff -> DiffMap
getDiffMapFromDiff diff =
  DiffMap
    { map = RangeMap.fromList deltaList
    , last = NE.last <$> NE.nonEmpty deltaList
    }
 where
  deltaList = (getDeltaList diff)

data DiffMap = DiffMap
  { map :: !(RangeMap Delta)
  , last :: (Maybe (Range, Delta))
  } deriving (Show, Eq)

diffPos :: (HasCallStack) => Pos -> DiffMap -> Pos
diffPos pos DiffMap {map, last} =
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

diffLineCol :: Rope -> DiffMap -> Rope -> LineCol -> LineCol
diffLineCol old diffMap new (LineCol line col) =
  newLineCol
 where
  pos = Rope.lineColToPos old (LineCol line col)
  newPos = diffPos pos diffMap
  newLineCol = Rope.posToLineCol new newPos

diffLineColRange :: Rope -> DiffMap -> Rope -> LineColRange -> LineColRange
diffLineColRange old diffMap new (LineColRange start end) =
  LineColRange (diffLineCol old diffMap new start) (diffLineCol old diffMap new end)

diffRange :: DiffMap -> Range -> Range
diffRange diffMap (Range start end) =
  Range (diffPos start diffMap) (diffPos end diffMap)
