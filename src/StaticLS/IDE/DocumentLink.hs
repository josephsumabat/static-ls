module StaticLS.IDE.DocumentLink (
  getDocumentLinks,
  DocumentLink (..),
) where

import AST qualified
import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.Path (AbsPath)
import Data.Pos (Pos (..))
import Data.Text (Text)
import Data.Text qualified as T
import StaticLS.IDE.Monad qualified as IDE
import StaticLS.Monad
import StaticLS.StaticEnv.Options (IssueTrackerConfig (..))

data DocumentLink = DocumentLink
  { range :: LineColRange
  , target :: Text
  }

getDocumentLinks :: Maybe IssueTrackerConfig -> AbsPath -> StaticLsM [DocumentLink]
getDocumentLinks Nothing _ = pure []
getDocumentLinks (Just config) path = do
  haskell <- IDE.getHaskell path
  let comments = findAllComments (AST.getDynNode haskell)
      template = T.replace "{org}" (T.pack config.org) (T.pack config.url)
  pure $ concatMap (extractTicketLinks template) comments

commentNodeTypes :: [Text]
commentNodeTypes = ["comment", "haddock"]

isCommentNode :: AST.DynNode -> Bool
isCommentNode node = node.nodeType `elem` commentNodeTypes

findAllComments :: AST.DynNode -> [AST.DynNode]
findAllComments node
  | isCommentNode node = [node]
  | otherwise = concatMap findAllComments node.nodeChildren

extractTicketLinks :: Text -> AST.DynNode -> [DocumentLink]
extractTicketLinks template node =
  findTicketsInText template node.nodeLineColRange.start node.nodeText

findTicketsInText :: Text -> LineCol -> Text -> [DocumentLink]
findTicketsInText template (LineCol startLine startCol) text =
  concatMap (findTicketsInLine template startLine startCol) (zip [0 ..] (T.lines text))

findTicketsInLine :: Text -> Pos -> Pos -> (Int, Text) -> [DocumentLink]
findTicketsInLine template (Pos startLine) (Pos startCol) (lineOffset, lineText) =
  go 0 lineText
 where
  currentLine = Pos (startLine + lineOffset)
  colBase = if lineOffset == 0 then startCol else 0

  -- offset tracks our column position within the line; remaining is the
  -- unprocessed suffix. findNextTicket returns positions relative to remaining,
  -- so we add offset to translate back to line-relative columns.
  go :: Int -> Text -> [DocumentLink]
  go offset remaining
    | T.null remaining = []
    | otherwise =
        case findNextTicket remaining of
          Nothing -> []
          Just (before, ticket, after) ->
            let ticketStart = offset + T.length before
                ticketEnd = ticketStart + T.length ticket
                range =
                  LineColRange
                    (LineCol currentLine (Pos (colBase + ticketStart)))
                    (LineCol currentLine (Pos (colBase + ticketEnd)))
                link =
                  DocumentLink
                    { range
                    , target = T.replace "{ticketId}" ticket template
                    }
             in link : go ticketEnd after

-- Find the next ticket pattern (e.g., ATV-2065) in text.
-- Returns (text before, ticket, text after) or Nothing.
findNextTicket :: Text -> Maybe (Text, Text, Text)
findNextTicket text = go text
 where
  go remaining
    | T.null remaining = Nothing
    | otherwise =
        if isUpperAlpha (T.head remaining)
          then case tryMatchTicket remaining of
            Just (ticket, after) ->
              let before = T.take (T.length text - T.length remaining) text
               in if isInsideUrl before
                    then go after
                    else Just (before, ticket, after)
            Nothing -> go (T.tail remaining)
          else go (T.tail remaining)

  isInsideUrl :: Text -> Bool
  isInsideUrl before =
    let reversed = T.reverse before
        -- Walk backwards: if we hit whitespace before "://", we're not in a URL
        nonSpace = T.takeWhile (\c -> c /= ' ' && c /= '\t' && c /= '\n') reversed
     in T.isInfixOf "://" (T.reverse nonSpace)

  tryMatchTicket :: Text -> Maybe (Text, Text)
  tryMatchTicket t =
    let (prefix, rest1) = T.span isUpperAlpha t
     in if T.length prefix >= 2 && not (T.null rest1) && T.head rest1 == '-'
          then
            let rest2 = T.tail rest1
                (digits, after) = T.span isDigit rest2
             in if not (T.null digits) && not (isContinuation after)
                  then Just (prefix <> "-" <> digits, after)
                  else Nothing
          else Nothing

  -- Check if the character after the ticket is a continuation character
  -- (letter, digit, or hyphen), which means this isn't actually a ticket boundary
  isContinuation :: Text -> Bool
  isContinuation t
    | T.null t = False
    | otherwise =
        let c = T.head t
         in isUpperAlpha c || isLowerAlpha c || isDigit c || c == '-'

isUpperAlpha :: Char -> Bool
isUpperAlpha c = c >= 'A' && c <= 'Z'

isLowerAlpha :: Char -> Bool
isLowerAlpha c = c >= 'a' && c <= 'z'

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
