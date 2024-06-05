{-# LANGUAGE PatternSynonyms #-}

module StaticLS.PositionDiff where

import Data.Algorithm.Diff qualified as Diff
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.Lexer qualified as Lexer
import StaticLS.Position
import Prelude hiding (lex)

pattern Insert :: b -> Diff.PolyDiff a b
pattern Insert x = Diff.Second x

pattern Delete :: a -> Diff.PolyDiff a b
pattern Delete x = Diff.First x

pattern Keep :: a -> b -> Diff.PolyDiff a b
pattern Keep a b = Diff.Both a b

{-# COMPLETE Insert, Delete, Keep #-}

data Token = Token
    { text :: !Text
    , len :: !Int
    }
    deriving (Eq, Show)

mkToken :: Text -> Token
mkToken text = Token{text, len = T.length text}

lex :: String -> [Token]
lex source =
    fmap
        ( \(_tok, (_pos, s)) ->
            let text = T.pack s
             in Token{text, len = T.length text}
        )
        (Lexer.lexerPass0 source)

type TokenDiff = [Diff.PolyDiff Token Token]

diffText :: Text -> Text -> TokenDiff
diffText x y =
    Diff.getDiff (lex (T.unpack x)) (lex (T.unpack y))

flipDiff :: TokenDiff -> TokenDiff
flipDiff = fmap $ \case
    Diff.First a -> Diff.Second a
    Diff.Second a -> Diff.First a
    Diff.Both a b -> Diff.Both b a

updatePositionUsingDiff :: Pos -> TokenDiff -> Pos
updatePositionUsingDiff (Pos pos) diff =
    Pos (max (pos + delta) 0)
  where
    diffBeforePos = getDiffBeforePos pos diff
    delta =
        sum $
            fmap
                ( \case
                    Delete t -> negate t.len
                    Insert t -> t.len
                    Keep _ _ -> 0
                )
                diffBeforePos

-- pos comes from the original text
-- diff is a diff between the original text and the new text
-- get the diff before the pos that will actually affect pos in the new text
getDiffBeforePos :: Int -> TokenDiff -> TokenDiff
getDiffBeforePos pos diff = go diff 0
  where
    -- use the original tokens to decide when to stop
    -- pos is part of the original text

    -- if we are past the position, the nothing can affect the position
    go _ at | at > pos = []
    -- no diffs left
    go [] _ = []
    go (d : ds) at =
        case d of
            Delete t ->
                -- position might be contained inside of the diff
                if at + t.len > pos
                    then [Delete (mkToken (T.take (pos - at + 1) t.text))]
                    else d : go ds (at + t.len)
            -- insertions are not part of the original text
            Insert _ -> d : go ds at
            -- keeps are part of the original text
            Keep t _ -> d : go ds (at + t.len)
