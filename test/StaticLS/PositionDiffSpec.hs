{-# LANGUAGE BlockArguments #-}

module StaticLS.PositionDiffSpec where

import Data.Diff qualified as Diff
import Data.Pos
import Data.Text qualified as T
import Language.Haskell.Lexer (Token (..))
import NeatInterpolation
import StaticLS.PositionDiff
import StaticLS.PositionDiff qualified as PositionDiff
import Test.Hspec

spec :: Spec
spec = do
  let check diff name pos pos' = it name do
        updatePositionUsingDiff diff pos `shouldBe` pos'

  describe "simple diff" do
    let diff =
          [ Diff.Keep (mkToken "module")
          , Diff.Delete (mkToken "ca")
          , Diff.Keep (mkToken "da")
          ]

    let check' = check diff

    check' "" (Pos 0) (Pos 0)
    check' "" (Pos 1) (Pos 1)
    check' "clipped 1" (Pos 6) (Pos 6)
    check' "clipped 2" (Pos 7) (Pos 6)

  describe "last diff is delete" do
    let diff =
          [ Diff.Keep (mkToken "module")
          , Diff.Delete (mkToken "ca")
          ]
    let check' = check diff
    check' "" (Pos 6) (Pos 5)
    check' "" (Pos 7) (Pos 5)
    check' "out of bounds" (Pos 8) (Pos 8)
    pure ()

  describe "more complex diff" do
    let diff =
          [ Diff.Keep (mkToken "module")
          , Diff.Delete (mkToken "ca")
          , Diff.Keep (mkToken "da")
          , Diff.Insert (mkToken "hello")
          , Diff.Delete (mkToken "hela")
          ]
    let check' = check diff
    check' "" (Pos 8) (Pos 6)
    check' "" (Pos 12) (Pos 12)
    pure ()

  xit "smoke" do
    let x = "first second third fourth"
    let y = "third second third fourth"
    let diff = PositionDiff.diffText x y
    diff `shouldBe` []

  fdescribe "resilient lexing" do
    let checkResilientLen name s ex = it name do
          length (PositionDiff.lex s) `shouldBe` ex

        checkResilient name s ex = it name do
          (PositionDiff.lex s) `shouldBe` ex

    let checkIncorrectLen name s ex = it name do
          length (PositionDiff.lexNonResilient s) `shouldBe` ex

    checkIncorrectLen
      "Expect the default lexer to fail against single quotes and return fewer tokens"
      "'Message adfadsf adfpaoidfu  adpofiuasdfpoi aspdfoiuasfpo adsf poiasduf ' 'adf'asdf a"
      2

    checkResilientLen
      "Resilient lexer Lexes correctly against tokens with single quotes returning full number of tokens"
      "'Message adfadsf adfpaoidfu  adpofiuasdfpoi aspdfoiuasfpo adsf poiasduf ' 'adf'asdf a"
      21

    -- TODO: convert to a proper golden testing framework
    checkResilient
      "Works on template haskell splice"
      "$(deriveJSON defaultOptions ''CodeActionMessage)"
      [ Token {text = "$", len = 1, kind = TokenKind Varsym}
      , Token {text = "(", len = 1, kind = TokenKind Special}
      , Token {text = "deriveJSON", len = 10, kind = TokenKind Varid}
      , Token {text = " ", len = 1, kind = TokenKind Whitespace}
      , Token {text = "defaultOptions", len = 14, kind = TokenKind Varid}
      , Token {text = " ", len = 1, kind = TokenKind Whitespace}
      , Token {text = "'", len = 1, kind = TokenKind ErrorToken}
      , Token {text = "'", len = 1, kind = TokenKind TheRest}
      , Token {text = "CodeActionMessage", len = 17, kind = TokenKind Conid}
      , Token {text = ")", len = 1, kind = TokenKind Special}
      ]

    pure ()

  pure ()
