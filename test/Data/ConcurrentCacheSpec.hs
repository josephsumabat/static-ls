module Data.ConcurrentCacheSpec (spec) where

import Control.Exception (Exception)
import Control.Exception.Base (throw)
import Data.ConcurrentCache qualified as ConcurrentCache
import Data.IORef qualified as IORef
import Data.Typeable (Typeable)
import Test.Hspec

yieldList :: [a] -> IO (IO (Maybe a))
yieldList xs = do
  ref <- IORef.newIORef xs
  pure do
    res <-
      IORef.atomicModifyIORef' ref \xs ->
        case xs of
          [] -> ([], Nothing)
          x : xs -> (xs, Just x)
    case res of
      Nothing -> pure Nothing
      Just !x -> pure $ Just x

data Exn = Exn
  deriving (Show, Eq, Typeable)

instance Exception Exn

spec :: Spec
spec = do
  it "multiple" do
    cache <- ConcurrentCache.new
    act <- yieldList [1 :: Int .. 10000000]
    res1 <- ConcurrentCache.insert 'a' act cache
    res1 `shouldBe` Just 1
    res3 <- ConcurrentCache.insert 'b' act cache
    res3 `shouldBe` Just 2
    res2 <- ConcurrentCache.insert 'a' act cache
    res2 `shouldBe` Just 1
    pure @IO ()
  it "exception" do
    cache <- ConcurrentCache.new
    act <- yieldList [throw Exn, 1 :: Int, throw Exn, 2 :: Int]
    ConcurrentCache.insert 'a' act cache `shouldThrow` (== Exn)
    res1 <- ConcurrentCache.insert 'a' act cache
    res1 `shouldBe` Just 1
    res2 <- ConcurrentCache.insert 'a' act cache
    res2 `shouldBe` Just 1
    ConcurrentCache.insert 'b' act cache `shouldThrow` (== Exn)
    res3 <- ConcurrentCache.insert 'b' act cache
    res3 `shouldBe` Just 2
    res4 <- ConcurrentCache.insert 'b' act cache
    res4 `shouldBe` Just 2
    pure @IO ()
  pure ()
