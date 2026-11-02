{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.CodeActions.ParsingSpec (spec) where

import Language.LSP.Protocol.Types qualified as LSP
import NeatInterpolation
import StaticLS.IDE.CodeActions.InsertFields
import StaticLS.IDE.CodeActions.Parse
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parsing to support code actions" do
    it "parses fields not initialized" do
      let result =
            missingFields . normalize $
              [trimming|
                • Fields of ‘Person’ not initialised:
                    firstName :: String
                    middleName :: Maybe String
                    lastName :: String
                    parents :: [Person]
                • In the expression: Person {}
                In an equation for ‘person’: person = Person {}
            |]
      result `shouldBe` Just (normalize "Person", Nothing, fmap normalize ["firstName", "middleName", "lastName", "parents"])
    it "parses missing strict fields" do
      let result =
            missingFields . normalize $
              [trimming|
                • Constructor ‘Person’ does not have the required strict field(s):
                    firstName :: String
                    middleName :: Maybe String
                    lastName :: String
                    parents :: [Person]
                • In the second argument of ‘($)’, namely
                    ‘Person {}’
            |]
      result `shouldBe` Just (normalize "Person", Nothing, fmap normalize ["firstName", "middleName", "lastName", "parents"])
    it "handles partially incomplete records" do
      let result =
            missingFields . normalize $
              [trimming|
                • Fields of ‘Person’ not initialised:
                    middleName :: Maybe String
                    lastName :: String
                • In the expression: Person {firstName = _, parents = _}
                  In an equation for ‘a’: a = Person {firstName = _, parents = _}
            |]
      case result of
        Just (constructor, existingFields, missingFields) ->
          renderFields (getNormalText constructor) (fmap getNormalText existingFields) (fmap getNormalText missingFields) 0
            `shouldBe` [trimming|
            Person
              { middleName = _
              , lastName = _
              , firstName = _
              , parents = _
              }
          |]
        Nothing -> fail "failed to parse"
    it "parses missing cases" do
      let result =
            nonExhaustivePatterns . normalize $
              [trimming|
                Pattern match(es) are non-exhaustive
                In a \case alternative:
                    Patterns of type ‘Maybe a’ not matched:
                        Nothing
                        Just _
            |]
      result `shouldBe` (Just $ fmap normalize ["Nothing", "Just _"])
    it "parses valid hole fits" do
      let result =
            validHoleFits . normalize $
              [trimming|
              • Found hole: _ :: Int -> Int
              • In an equation for ‘a’: a = _
              • Relevant bindings include
                  a :: Int -> Int
                    (bound at src/StaticLS/IDE/CodeActions/Parse.hs:115:1)
                Valid hole fits include
                  a :: Int -> Int
                    (bound at src/StaticLS/IDE/CodeActions/Parse.hs:115:1)
                  negate :: forall a. Num a => a -> a
                    with negate @Int
                    (imported from ‘Prelude’ at src/StaticLS/IDE/CodeActions/Parse.hs:1:8-37
                    (and originally defined in ‘GHC.Internal.Num’))
                  fromIntegral :: forall a b. (Integral a, Num b) => a -> b
                    with fromIntegral @Int @Int
                    (imported from ‘Prelude’ at src/StaticLS/IDE/CodeActions/Parse.hs:1:8-37
                    (and originally defined in ‘GHC.Internal.Real’))
                  id :: forall a. a -> a
                    with id @Int
                    (imported from ‘Prelude’ at src/StaticLS/IDE/CodeActions/Parse.hs:1:8-37
                    (and originally defined in ‘GHC.Internal.Base’))
                  fromEnum :: forall a. Enum a => a -> Int
                    with fromEnum @Int
                    (imported from ‘Prelude’ at src/StaticLS/IDE/CodeActions/Parse.hs:1:8-37
                    (and originally defined in ‘GHC.Internal.Enum’))
                  pred :: forall a. Enum a => a -> a
                    with pred @Int
                    (imported from ‘Prelude’ at src/StaticLS/IDE/CodeActions/Parse.hs:1:8-37
                    (and originally defined in ‘GHC.Internal.Enum’))
                  (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)
            |]
      result `shouldBe` (Just $ fmap normalize ["a", "negate", "fromIntegral", "id", "fromEnum", "pred"])
    it "parses missing methods" do
      let result =
            missingMethods . normalize $
              [trimming|
              • No explicit implementation for
                  ‘StaticLS.IDE.CodeActions.Parse.print’, ‘+++’, and ‘unit’
              • In the instance declaration for ‘A Int’
            |]
      result `shouldBe` (Just $ fmap normalize ["print", "(+++)", "unit"])
    it "parses missing associated type" do
      let result =
            missingAssociatedType . normalize $
              [trimming|
              • No explicit associated type or default declaration for ‘AssocType’
              • In the instance declaration for ‘A Int’
            |]
      result `shouldBe` (Just $ normalize "AssocType")
    describe "variable not in scope" do
      it "parses a single spelling suggestion" do
        let result =
              variableNotInScope . normalize $
                [trimming|
                Variable not in scope: summ :: [Int] -> Int
                Suggested fix: Perhaps use ‘sum’ (imported from Prelude)
              |]
        result `shouldBe` Just [MissSpelled "summ" "sum"]
      it "parses adding to the import list of a single module" do
        let result =
              variableNotInScope . normalize $
                [trimming|
                Variable not in scope: asum :: [Maybe Int] -> Maybe Int
                Suggested fix:
                  Perhaps you want to add ‘asum’ to the import list
                  in the import of ‘Data.Foldable’
                  (src/StaticLS/IDE/CodeActions/Parse.hs:20:1-34).
              |]
        result `shouldBe` Just [AddToImportList "asum" "Data.Foldable"]
      it "parses adding to one of several import lists" do
        let result =
              variableNotInScope . normalize $
                [trimming|
                Variable not in scope: asum :: [Maybe Int] -> Maybe Int
                Suggested fix:
                  Perhaps you want to add ‘asum’ to one of these import lists:
                    ‘Control.Applicative’ (src/StaticLS/IDE/CodeActions/Parse.hs:18:1-37)
                    ‘Data.Foldable’ (src/StaticLS/IDE/CodeActions/Parse.hs:20:1-34)
              |]
        result
          `shouldBe` Just
            [ AddToImportList "asum" "Control.Applicative"
            , AddToImportList "asum" "Data.Foldable"
            ]
      it "parses multiple suggested fixes" do
        let result =
              variableNotInScope . normalize $
                [trimming|
                Variable not in scope: asum :: [Maybe Int] -> Maybe Int
                Suggested fixes:
                  • Perhaps use ‘sum’ (imported from Prelude)
                  • Perhaps you want to add ‘asum’ to the import list
                    in the import of ‘Data.Foldable’
                    (src/StaticLS/IDE/CodeActions/Parse.hs:20:1-34).
              |]
        result
          `shouldBe` Just
            [ MissSpelled "asum" "sum"
            , AddToImportList "asum" "Data.Foldable"
            ]
      it "does not parse when there are no suggested fixes" do
        let result =
              variableNotInScope . normalize $
                [trimming|
                Variable not in scope: foo :: Int
              |]
        result `shouldBe` Nothing
      it "does not parse other diagnostics" do
        let result =
              variableNotInScope . normalize $
                [trimming|
                • No explicit associated type or default declaration for ‘AssocType’
                • In the instance declaration for ‘A Int’
              |]
        result `shouldBe` Nothing
      it "is recognised as an actionable issue" do
        let pos = LSP.Position 3 8
            diag =
              LSP.Diagnostic
                { _range = LSP.Range pos pos
                , _severity = Just LSP.DiagnosticSeverity_Error
                , _code = Nothing
                , _codeDescription = Nothing
                , _source = Just "haskell"
                , _message =
                    [trimming|
                    Variable not in scope: summ :: [Int] -> Int
                    Suggested fix: Perhaps use ‘sum’ (imported from Prelude)
                  |]
                , _tags = Nothing
                , _relatedInformation = Nothing
                , _data_ = Nothing
                }
        actionableIssue diag `shouldBe` Just (VariableNotInScope (Ignored diag) [MissSpelled "summ" "sum"])
