{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}

module StaticLS.Position
where

import Debug.Trace

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS hiding (drop, take)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Filediff
import Filediff.Types
import qualified GHC.Iface.Ext.Types as GHC
import qualified Language.Haskell.Lexer as Lex
import Safe (atMay)
import StaticLS.HIE (HieDbCoords)

data PositionInfo = PositionInfo
    { position :: (Int, Int)
    , srcToken :: Maybe TokenInfo
    , currToken :: Maybe TokenInfo
    }
    deriving (Show, Eq)

newtype TokenInfo =
  TokenInfo { unTokenInfo :: (Lex.Token, String) }
    deriving newtype (Show, Eq)

posTokenToTokenInfo :: Lex.PosToken -> TokenInfo
posTokenToTokenInfo (tok1, (_, str1)) = TokenInfo (tok1, str1)

{- | Convert from position in an editted file to where it would be in the hie source file
Assumes a 0 indexed posiiton
-}
adjustToHieSrcPosition ::
    (MonadIO m) =>
    HieDbCoords ->
    GHC.HieFile ->
    FilePath ->
    m PositionInfo
adjustToHieSrcPosition currPos hieFile filePath =
    do
        let hieSrc = BS.fromStrict hieFile.hie_hs_src
        currentSrc <- liftIO $ BS.readFile filePath
        pure $ adjustPosition currPos hieSrc currentSrc

-- | Adjust a position to the original location based on edits to a file
adjustPosition :: HieDbCoords -> ByteString -> ByteString -> PositionInfo
adjustPosition (currLineIdx, currCharIdx) orig current =
    let origLines = BS.lines orig
        currLines = BS.lines current
        linesDiff = diffSrcLines origLines currLines
        origLineIdx = traceShow linesDiff $ adjustIndex currLineIdx linesDiff
        lineDiffInfo = traceShow origLineIdx $ diffLineInfo (origLineIdx, origLines) (currLineIdx, currLines)
        origCharIdx = maybe currCharIdx (\(lineDiff, _, _) -> adjustIndex currCharIdx lineDiff) lineDiffInfo
     in PositionInfo
          (origLineIdx, origCharIdx)
          (tokenAt origCharIdx . snd3 =<< lineDiffInfo)
          (tokenAt currCharIdx . thrd =<< lineDiffInfo)
  where
    snd3 (_, t, _) = t
    thrd (_, _, t) = t

-- | Given a destination index and a diff list, reverse engineer to get the index that the source would be at before the diffs were applied
adjustIndex :: (Show a) => Int -> ListDiff a -> Int
adjustIndex currIdx diff =
    let applicableAdds = filter (\(addIdx, _) -> addIdx < currIdx) diff.adds
        idxAfterRemovedAdditions = currIdx - length applicableAdds
     in addDeletions idxAfterRemovedAdditions diff.dels
  where
    -- Replay adding deletions
    addDeletions :: Int -> [(Int, a)] -> Int
    addDeletions idx [] = idx
    addDeletions idx (x : xs) = if idx <= fst x then idx else addDeletions (idx + 1) xs

tokenAt :: Int -> [Lex.PosToken] -> Maybe TokenInfo
tokenAt idx (posTok : posTok2@(_, (pos2, _)) : xs) = do
    if idx < pos2.char then
      Just $ posTokenToTokenInfo posTok
    else
      tokenAt idx (posTok2 : xs)
tokenAt _ [posTok] =
    Just $ posTokenToTokenInfo posTok
tokenAt _ [] = Nothing

diffSrcLines :: [ByteString] -> [ByteString] -> ListDiff ByteString
diffSrcLines = diffLists

-- | Return a diff of two lines and their lexed output
diffLineInfo :: (Int, [ByteString]) -> (Int, [ByteString]) -> Maybe (ListDiff Char, [Lex.PosToken], [Lex.PosToken])
diffLineInfo (origLineNum, origLines) (currLineNum, currLines) =
    let origAsStr = BS.toString <$> atMay origLines origLineNum
        currAsStr = BS.toString <$> atMay currLines currLineNum
     in (,,)
            <$> (diffLists <$> origAsStr <*> currAsStr)
            <*> (Lex.lexerPass1 <$> origAsStr)
            <*> (Lex.lexerPass1 <$> currAsStr)
