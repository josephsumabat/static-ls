module TestImport.Assert where

import Data.Maybe (listToMaybe)
import GHC.Stack (HasCallStack)

assertHead :: (HasCallStack, MonadFail m) => String -> [a] -> m a
assertHead msg = maybe (fail $ assertionFailureMsg msg) pure . listToMaybe

assertJust :: (MonadFail m) => String -> Maybe a -> m a
assertJust msg =
    \case
        Just a -> pure a
        Nothing -> fail $ assertionFailureMsg msg

assertLeft :: (MonadFail m) => String -> Either a b -> m a
assertLeft msg =
    \case
        Right _ ->
            fail $ assertionFailureMsg msg
        Left e -> pure e

assertRight :: (MonadFail m) => String -> Either a b -> m b
assertRight msg =
    \case
        Left _ -> fail $ assertionFailureMsg msg
        Right v -> pure v

assertionFailureMsg :: String -> String
assertionFailureMsg msg =
    "An assertion failed with msg: " <> msg
