{-# LANGUAGE QuasiQuotes #-}

module TestImport.AnnotationSpec where

import Control.Exception (throw)
import NeatInterpolation
import Test.Hspec
import TestImport.Annotation

spec :: Spec
spec = do
  xit "nested annotations" do
    let s =
          [trimming|
    -- stuff        other stuff
    -- ^^ 'st'
    -- ^^^^^ 'stuff'      ^^^ multiple
    --              ^^^^^^^^^^^ 'other stuff'
    --                ^ single
    -- let testing = 1242324
    let another = 13241234
    --  ^^^^^^^ the testing identifier
    |]
    let annotations = parseAnnotations s
    case annotations of
      Left e -> throw e
      Right annotations -> do
        annotations `shouldBe` []
    True `shouldBe` False
    pure @IO ()
  pure ()
