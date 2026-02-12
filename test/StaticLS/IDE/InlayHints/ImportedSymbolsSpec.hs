module StaticLS.IDE.InlayHints.ImportedSymbolsSpec (spec) where

import Data.Map.Strict qualified as Map
import StaticLS.IDE.InlayHints.ImportedSymbols
import Test.Hspec

spec :: Spec
spec = do
  describe "cleanOccName" $ do
    describe "strips type prefixes" $ do
      it "strips v: prefix from values" $ do
        cleanOccName "v:myFunction" `shouldBe` "myFunction"

      it "strips t: prefix from types" $ do
        cleanOccName "t:MyType" `shouldBe` "MyType"

      it "strips c: prefix from constructors" $ do
        cleanOccName "c:MyConstructor" `shouldBe` "MyConstructor"

    describe "handles record field selectors" $ do
      it "converts v: prefixed field selector to Type(..)" $ do
        cleanOccName "v:fMyType:fieldName" `shouldBe` "MyType(..)"

      it "converts bare field selector to Type(..)" $ do
        cleanOccName "fSkipActivationItemRequest:skipActivationItemName" `shouldBe` "SkipActivationItemRequest(..)"

      it "does not convert names without colon" $ do
        cleanOccName "v:fMyFunction" `shouldBe` "fMyFunction"

      it "does not convert names starting with lowercase after f" $ do
        cleanOccName "v:foo" `shouldBe` "foo"

      it "does not convert bare f followed by lowercase" $ do
        cleanOccName "foobar" `shouldBe` "foobar"

    describe "preserves regular names" $ do
      it "preserves names without prefix" $ do
        cleanOccName "regularName" `shouldBe` "regularName"

    describe "handles operators" $ do
      it "strips module suffix after pipe" $ do
        cleanOccName "v:==|GHC.Classes" `shouldBe` "=="

      it "leaves operators without suffix untouched" $ do
        cleanOccName "v:=." `shouldBe` "=."

  describe "groupByModule" $ do
    it "groups symbols by module" $ do
      let symbols =
            [ ImportedSymbol "v:foo" "Module.A"
            , ImportedSymbol "v:bar" "Module.B"
            , ImportedSymbol "t:Baz" "Module.A"
            ]
          result = groupByModule symbols
      Map.lookup "Module.A" result `shouldBe` Just ["Baz", "foo"]
      Map.lookup "Module.B" result `shouldBe` Just ["bar"]

    it "deduplicates field selectors from same type" $ do
      let symbols =
            [ ImportedSymbol "fMyRecord:field1" "Module.A"
            , ImportedSymbol "fMyRecord:field2" "Module.A"
            ]
          result = groupByModule symbols
      -- Both field selectors should collapse to one MyRecord(..)
      Map.lookup "Module.A" result `shouldBe` Just ["MyRecord(..)"]

    it "normalizes module names with unit ids" $ do
      let symbols =
            [ ImportedSymbol "v:foo" "base-4.19.1.0:Data.List"
            , ImportedSymbol "v:bar" "Data.List"
            ]
          result = groupByModule symbols
      Map.lookup "Data.List" result `shouldBe` Just ["bar", "foo"]
