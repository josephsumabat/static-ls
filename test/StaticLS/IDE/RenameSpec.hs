{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.RenameSpec where

import AST qualified
import AST.Haskell qualified as Haskell
import Data.Edit qualified as Edit
import Data.Rope qualified as Rope
import NeatInterpolation
import StaticLS.IDE.Rename
import Test.Hspec

getEverything :: (AST.Cast b) => AST.DynNode -> [b]
getEverything node = case AST.cast node of
  Just n -> n : concatMap getEverything node.nodeChildren
  Nothing -> concatMap getEverything node.nodeChildren

spec :: Spec
spec = do
  let check name source oldName newName expected = it name do
        let hs = Haskell.parse (id, id) source
        let splice = head $ getEverything @Haskell.TopSplice hs.dynNode
        let changes = renameSplice splice.dynNode oldName newName
        let sourceRope = Rope.fromText source
        let newSourceRope = Rope.edit (Edit.changesToEdit changes) sourceRope
        let newSource = Rope.toText newSourceRope
        newSource `shouldBe` expected
        pure @IO ()
        pure @IO ()
  check
    "simple"
    [trimming|
    module Testing where

    fold
      [ deriveJSON (jsonDeriveWithAffix "ZendeskTicketPriority" jsonDeriveOptionsSnakeCase) ''Hello
      , deriveJSON (jsonDeriveWithAffix "zendeskTicketTextCommentData" jsonDeriveOptionsSnakeCase) ''ZendeskTicketTextCommentData
      , deriveJSON (jsonDeriveWithAffix "zendeskTicketHtmlCommentData" jsonDeriveOptionsSnakeCase) ''ZendeskTicketHtmlCommentData
      , deriveJSON (jsonDeriveOptionsDropTags defaultOptions) ''ZendeskTicketComment
      , deriveJSON (jsonDeriveWithAffix "zendeskTicket" jsonDeriveOptionsSnakeCase) ''ZendeskTicket
      , deriveJSON (jsonDeriveWithAffix "zendeskTicketResponse" jsonDeriveOptionsSnakeCase) ''ZendeskTicketResponse
      , deriveJSON (jsonDeriveWithAffix "createZendeskTicketData" jsonDeriveOptionsSnakeCase) ''CreateZendeskTicketData
      , deriveJSON (jsonDeriveWithAffix "zendeskTicketParams" jsonDeriveOptionsSnakeCase) ''ZendeskTicketParams
      ]
    |]
    "Hello"
    "AnotherOne"
    [trimming|
    module Testing where

    fold
      [ deriveJSON (jsonDeriveWithAffix "ZendeskTicketPriority" jsonDeriveOptionsSnakeCase) ''AnotherOne
      , deriveJSON (jsonDeriveWithAffix "zendeskTicketTextCommentData" jsonDeriveOptionsSnakeCase) ''ZendeskTicketTextCommentData
      , deriveJSON (jsonDeriveWithAffix "zendeskTicketHtmlCommentData" jsonDeriveOptionsSnakeCase) ''ZendeskTicketHtmlCommentData
      , deriveJSON (jsonDeriveOptionsDropTags defaultOptions) ''ZendeskTicketComment
      , deriveJSON (jsonDeriveWithAffix "zendeskTicket" jsonDeriveOptionsSnakeCase) ''ZendeskTicket
      , deriveJSON (jsonDeriveWithAffix "zendeskTicketResponse" jsonDeriveOptionsSnakeCase) ''ZendeskTicketResponse
      , deriveJSON (jsonDeriveWithAffix "createZendeskTicketData" jsonDeriveOptionsSnakeCase) ''CreateZendeskTicketData
      , deriveJSON (jsonDeriveWithAffix "zendeskTicketParams" jsonDeriveOptionsSnakeCase) ''ZendeskTicketParams
      ]
    |]

  pure ()
