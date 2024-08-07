{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module StaticLS.PositionSpec where

import Data.Function ((&))
import Data.LineCol (LineCol (..))
import Data.List.NonEmpty qualified as NE
import Data.Pos
import Data.Text (Text)
import Data.Text qualified as T
import Data.TextUtils
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

data TextAndPos = TextAndPos
  { text :: Text
  , pos :: Pos
  }
  deriving (Show)

instance Arbitrary TextAndPos where
  arbitrary = do
    text <- suchThat arbitrary (not . T.null)
    pos <- choose (0, T.length text - 1)
    pure $ TextAndPos text (Pos pos)

data TextAndLineCol = TextAndLineCol
  { text :: Text
  , lineCol :: LineCol
  }
  deriving (Show)

instance Arbitrary TextAndLineCol where
  arbitrary = do
    text <- suchThat arbitrary (not . T.null)
    line <- choose (0, length (splitLines text) - 1)
    let theLine = NE.toList (splitLinesWithEnd text) !! line
    -- the last line could be empty, ex "a\n"
    col <- choose (0, max 0 (T.length theLine - 1))
    pure $ TextAndLineCol text (LineCol (Pos line) (Pos col))

spec :: Spec
spec = do
  prop "lineColToPos . posToLineCol = id" $ \(TextAndPos {text, pos}) -> do
    let lineCol = posToLineCol text pos
    let pos' = lineColToPos text lineCol
    pos === pos'
  prop "posToLineCol . lineColToPos = id" $ \(TextAndLineCol {text, lineCol}) -> do
    let pos = lineColToPos text lineCol
    let lineCol' = posToLineCol text pos
    lineCol === lineCol'
  it "weird case" $ do
    let text = "=k"
    let pos = Pos 0
    let lineCol = posToLineCol text pos
    lineCol `shouldBe` LineCol (Pos 0) (Pos 0)
    let pos' = lineColToPos text lineCol
    pos `shouldBe` pos'
    pure @IO ()
  pure ()

lineColToPos :: Text -> LineCol -> Pos
lineColToPos source LineCol {line, col} =
  UnsafePos pos
 where
  (before, after) = splitAt line.pos lines
  (beforeCol, _afterCol) = T.splitAt col.pos (T.concat after)
  pos = sum (T.length <$> before) + T.length beforeCol
  lines = splitLinesWithEnd source & NE.toList

posToLineCol :: Text -> Pos -> LineCol
posToLineCol source UnsafePos {pos} =
  LineCol {line, col}
 where
  (beforePos, _afterPos) = T.splitAt pos source
  linesBeforePos = splitLinesWithEnd beforePos
  lastLineBeforePos = NE.last linesBeforePos
  line = Pos (length linesBeforePos - 1)
  col = Pos (T.length lastLineBeforePos)
