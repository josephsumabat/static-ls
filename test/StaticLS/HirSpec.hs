{-# LANGUAGE QuasiQuotes #-}

module StaticLS.HirSpec (spec) where

import AST.Haskell qualified as H
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T.Read
import NeatInterpolation (trimming)
import StaticLS.Hir qualified as Hir
import StaticLS.Utils (isJustOrThrowS, isRightOrThrowS)
import Test.Hspec
import TestImport.Annotation qualified as Annotation
import UnliftIO.Exception qualified as Exception

spec :: Spec
spec = do
  describe "find persistent model" do
    check
      "first"
      [trimming|
        module First where

        $(mkModel $(discoverEntities) $(modelFile "server_session"))
        -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 1

        $(mkModel $(discoverEntities) $(modelFile "another_one"))
        -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 2

        $(mkModel $(discoverEntities))
        -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 3

        $(mkAnotherOne $(discoverEntities) $(modelFile "anotherone"))
        -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 4

        $(unrelated)
        -- ^^^^^^^^^ 5
        |]
      [ (1, Just "server_session")
      , (2, Just "another_one")
      , (3, Nothing)
      , (4, Nothing)
      , (5, Nothing)
      ]

check :: String -> Text -> [(Int, Maybe Text)] -> Spec
check name src expected = it name do
  let hs = H.parse src
  let annotations = Annotation.parseAnnotations src
  annotations <- case annotations of
    Left err -> Exception.throwIO err
    Right annotations -> pure annotations
  for_ annotations \ann -> do
    let range = ann.range
    let annText = T.strip ann.annText
    (expectedIndex :: Int, _) <- T.Read.decimal annText & isRightOrThrowS
    let modelName = Hir.getPersistentModelAtPoint range hs
    expected <- lookup expectedIndex expected & isJustOrThrowS "could not find index in expected"
    modelName `shouldBe` expected
  pure @IO ()
