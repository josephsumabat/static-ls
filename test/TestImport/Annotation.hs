module TestImport.Annotation (
  parseAnnotations,
)
where

import Control.Exception (SomeException)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.LineColRange (LineColRange (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.ListUtils
import Data.Maybe qualified as Maybe
import Data.LineCol (LineCol (..))
import Data.Pos (Pos (..))
import Data.Range (Range)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.TextUtils qualified as T
import Debug.Trace
import StaticLS.Utils (isJustOrThrowE)
import UnliftIO.Exception qualified

data AnnToken = TextAnn !Text | RangeAnn !Int

getTextAnn :: AnnToken -> Maybe Text
getTextAnn (TextAnn t) = Just t
getTextAnn _ = Nothing

data AnnLineToken = TextLine | AnnLineToken [AnnToken]

type CalculatedAnn = (LineCol, Int, Text)

annLen :: AnnToken -> Int
annLen (TextAnn t) = T.length t
annLen (RangeAnn len) = len

parseAnnTokens :: Text -> [AnnToken]
parseAnnTokens lineWithoutPrefix =
  go lineWithoutPrefix
 where
  go line =
    if "^" `T.isPrefixOf` caretStart
      then TextAnn beforeCaret : RangeAnn (T.length carets) : go afterCarets
      else [TextAnn beforeCaret]
   where
    (beforeCaret, caretStart) = T.breakOn "^" line
    (carets, afterCarets) = T.span (== '^') caretStart

parseAnnotationLineToken :: Text -> NonEmpty AnnLineToken
parseAnnotationLineToken text = parseLine <$> lines
 where
  parseLine line
    | Just suffix <- T.stripPrefix "--" line = AnnLineToken $ TextAnn "--" : parseAnnTokens suffix
    | otherwise = TextLine
  lines = T.splitLinesWithEnd text

calculatePositionsFromLengths :: [(Int, a)] -> [(Int, a)]
calculatePositionsFromLengths = go 0
 where
  go !_pos [] = []
  go !pos ((len, x) : xs) = (pos, x) : go (pos + len) xs

calculateAnns :: Int -> [AnnToken] -> [CalculatedAnn]
calculateAnns line anns =
  go . calculatePositionsFromLengths . map (\ann -> (annLen ann, ann)) $ anns
 where
  go [] = []
  go ((pos, RangeAnn len) : anns) =
    (LineCol (Pos line) (Pos pos), len, text) : go restAnns
   where
    (texts, restAnns) = spanMaybe (getTextAnn . snd) anns
    text = T.concat texts
  go ((_pos, TextAnn _text) : anns) = go anns

calculatedAnnsFromLineTokens :: [AnnLineToken] -> [CalculatedAnn]
calculatedAnnsFromLineTokens annLineTokens =
  concatMap
    ( \(line, annLineToken) -> case annLineToken of
        TextLine -> []
        AnnLineToken annTokens -> calculateAnns line annTokens
    )
    $ zip [0 ..] annLineTokens

-- Annotations point to the last line that actually was long enough for the
-- range, not counting annotations themselves. So overlapping annotations are
-- possible:
-- -- stuff        other stuff
-- -- ^^ 'st'
-- -- ^^^^^ 'stuff'
-- --              ^^^^^^^^^^^ 'other stuff'
--
-- To do this, we sink each annotation by repeatedly testing if the previous line is valid
sinkAnnotation :: Rope -> IntSet -> CalculatedAnn -> Maybe CalculatedAnn
sinkAnnotation rope linesWithAnnotations (LineCol (Pos line) (Pos col), len, text) =
  (\sunkenLine -> (LineCol (Pos sunkenLine) (Pos col), len, text)) <$> mSunkenLine
 where
  trySink !currLine
    | currLine < 0 = Nothing
    | isValidLine currLine = Just currLine
    | otherwise = trySink (currLine - 1)
  isValidLine line =
    not (line `IntSet.member` linesWithAnnotations)
      && Rope.isValidLineCol rope (traceShowId (LineCol (Pos line) (Pos col)))
      -- don't include the newline here, so we don't use isValidLineColEnd
      && Rope.isValidLineCol rope (traceShowId (LineCol (Pos line) (Pos (col + len))))
  mSunkenLine = trySink line

sinkAnnotations :: Rope -> [CalculatedAnn] -> Either SomeException [CalculatedAnn]
sinkAnnotations rope annotations =
  trace
    (show linesWithAnnotations)
    sunkAnnotations
 where
  linesWithAnnotations = IntSet.fromList $ map (\(LineCol (Pos line) (Pos _col), _, _) -> line) annotations
  sunkAnnotations =
    mapM
      ( \ann ->
          isJustOrThrowE (UnliftIO.Exception.stringException $ "couldn't convert annotation: " ++ show ann) $
            sinkAnnotation rope linesWithAnnotations ann
      )
      annotations

data Ann = Ann
  { lineCol :: !LineCol
  , len :: !Int
  , range :: !Range
  , contentText :: !Text
  , annText :: !Text
  }
  deriving (Show, Eq, Ord)

parseAnnotations :: Text -> Either SomeException [Ann]
parseAnnotations t =
  (fmap . fmap) (convertCalculatedAnn rope) sunkenAnns
 where
  rope = Rope.fromText t
  annLineTokens = parseAnnotationLineToken t
  anns = calculatedAnnsFromLineTokens (NE.toList annLineTokens)
  sunkenAnns = sinkAnnotations rope anns

convertCalculatedAnn :: Rope -> CalculatedAnn -> Ann
convertCalculatedAnn rope (lineCol, len, text) =
  Ann
    { lineCol = lineCol
    , len = len
    , range
    , contentText
    , annText = text
    }
 where
  contentText =
    Rope.toText $
      Maybe.fromMaybe (error "BUG: range should be in bounds") $
        Rope.indexRange rope range
  range =
    Rope.lineColRangeToRange
      rope
      ( LineColRange
          lineCol
          ( lineCol
              { col =
                  Pos (lineCol.col.pos + len)
              }
          )
      )
