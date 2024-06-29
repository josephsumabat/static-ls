-- adapted from ghcid's vscode plugin
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.Diagnostics.ParseGHC (
  split,
  parse,
)
where

import Data.Bifunctor (second)
import Data.Char qualified as Char
import Data.Function ((&))
import Data.LineColRange (LineColRange (..))
import Data.LineColRange qualified as LineColRange
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra (minimum1)
import Data.Maybe qualified as Maybe
import Data.Path qualified as Path
import Data.Pos (LineCol (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T.Read
import StaticLS.IDE.Diagnostics (Diagnostic (..))
import StaticLS.IDE.Diagnostics qualified as Diagnostics
import StaticLS.IDE.FileWith (FileWith' (..))
import StaticLS.IDE.FileWith qualified as FileWith
import Text.RawString.QQ
import Text.Regex.TDFA qualified as RE

mkRegex :: Text -> RE.Regex
mkRegex = RE.makeRegex

matches :: Text -> RE.Regex -> Bool
matches s re = RE.matchTest re s

getStuff :: Text -> RE.Regex -> Maybe (Text, Text, Text, [Text])
getStuff s re = RE.matchM re s

getCaptures :: Text -> RE.Regex -> Maybe [Text]
getCaptures s re =
  get <$> captures
 where
  get (_, _, _, captures) = captures
  captures = getStuff s re

type Header = (FileWith' Path.Rel LineColRange, Diagnostics.Severity, Text)

type Message = (Header, [Text])

readInt :: Text -> Maybe Int
readInt t = do
  case T.Read.decimal t of
    Right (i, "") -> Just i
    _ -> Nothing

-- $> -- parseHeader "src/StaticLS/ParseErrors.hs:(10,11)-(5,11): warning: [-Wunused-imports]"

-- $> -- parseHeader "src/StaticLS/ParseErrors.hs:10:5-11: warning: [-Wunused-imports]"

parseSeverity :: Text -> Diagnostics.Severity
parseSeverity t = case T.toLower t of
  "error" -> Diagnostics.Error
  "warning" -> Diagnostics.Warning
  _ -> Diagnostics.Error

dec :: Int -> Int
dec x = max 0 (x - 1)

parseHeader :: Text -> Maybe Header
parseHeader line =
  if
    | Just [file, line, col, sev, rest] <- getCaptures line r1
    , Just (dec -> line) <- readInt line
    , Just (dec -> col) <- readInt col ->
        Just $
          ( FileWith
              { path = Path.filePathToRel (T.unpack file)
              , loc = LineColRange.empty (LineCol line col)
              }
          , parseSeverity sev
          , rest
          )
    | Just [file, line, col1, col2, sev, rest] <- getCaptures line r2
    , Just (dec -> line) <- readInt line
    , Just (dec -> col1) <- readInt col1
    , Just (dec -> col2) <- readInt col2 ->
        Just $
          ( FileWith
              { path = Path.filePathToRel (T.unpack file)
              , loc = LineColRange (LineCol line col1) (LineCol line col2)
              }
          , parseSeverity sev
          , rest
          )
    | Just [file, line1, col1, line2, col2, sev, rest] <- getCaptures line r3
    , Just (dec -> line1) <- readInt line1
    , Just (dec -> col1) <- readInt col1
    , Just (dec -> line2) <- readInt line2
    , Just (dec -> col2) <- readInt col2 ->
        Just $
          ( FileWith
              { path = Path.filePathToRel (T.unpack file)
              , loc = LineColRange (LineCol line1 col1) (LineCol line2 col2)
              }
          , parseSeverity sev
          , rest
          )
    | otherwise -> Nothing
 where
  r1 = mkRegex [r|(..[^:]+):([0-9]+):([0-9]+): ([^:]+): ([^:]+)|]
  r2 = mkRegex [r|(..[^:]+):([0-9]+):([0-9]+)-([0-9]+): ([^:]+): ([^:]+)|]
  r3 = mkRegex [r|(..[^:]+):\(([0-9]+),([0-9]+)\)-\(([0-9]+),([0-9]+)\): ([^:]+): ([^:]+)|]

isMessageBody :: Text -> Bool
isMessageBody t = isIndented || hasNum
 where
  isIndented = T.isPrefixOf " " t
  sep = T.findIndex (== '|') t
  hasNum = case sep of
    Just i -> Maybe.isJust (readInt (T.take i t))
    Nothing -> False

clean :: [Text] -> [Text]
clean = filter (not . shouldDropLine)

shouldDropLine :: Text -> Bool
shouldDropLine t = t == "In the" || t `matches` r1 || t `matches` r2
 where
  r1 = mkRegex [r|^\s*\|$|]
  r2 = mkRegex [r|(\d+)?\s*\||]

split :: Text -> [Message]
split = findHeader . T.lines
 where
  findHeader [] = []
  findHeader (line : lines) = case parseHeader line of
    Just header -> (header, body) : findHeader rest
    Nothing -> findHeader lines
   where
    (body, rest) = List.span isMessageBody lines

toDiagnostic :: (Path.RelPath -> Path.AbsPath) -> Message -> Diagnostic
toDiagnostic toAbs ((range, severity, rest), message) =
  Diagnostic
    { range = FileWith.mapPath toAbs range
    , severity = severity
    , message = T.strip (T.unlines message)
    }

parse :: (Path.RelPath -> Path.AbsPath) -> Text -> [Diagnostic]
parse toAbs = fmap (toDiagnostic toAbs . second (dedent . clean)) . split

dedent :: [Text] -> [Text]
dedent lines =
  map (T.drop indentation) lines
 where
  indentation =
    lines
      & filter (not . T.null)
      & map (T.length . T.takeWhile Char.isSpace)
      & NE.nonEmpty
      & fmap minimum1
      & Maybe.fromMaybe 0
