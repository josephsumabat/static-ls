module TestImport.Placeholder where

import Control.Exception (SomeException)
import Control.Monad qualified as Monad
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Either.Extra (mapLeft)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Pos (Pos (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T.Read
import UnliftIO.Exception qualified as Exception

parseM :: (MonadThrow m) => Text -> m (Text, IntMap Pos)
parseM = either throwM pure . parsePlaceholders

parsePlaceholders :: Text -> Either SomeException (Text, IntMap Pos)
parsePlaceholders text =
  go text
 where
  go text =
    case T.stripPrefix "@" withAt of
      Nothing -> pure (text, mempty)
      Just afterAt -> do
        let res = T.Read.decimal afterAt
        let res' = mapLeft (Exception.toException . Exception.stringException) res
        (num, rest) <- res'
        (otherText, otherMap) <- go rest
        Monad.when (num `IntMap.member` otherMap) do
          throwM $ Exception.stringException $ "Duplicate placeholder number: " <> show num
        pure
          ( beforeAt <> otherText
          , (\(Pos p) -> Pos (p + T.length beforeAt))
              <$> IntMap.insert num (Pos 0) otherMap
          )
   where
    (beforeAt, withAt) = T.breakOn "@" text
