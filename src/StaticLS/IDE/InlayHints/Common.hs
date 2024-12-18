{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.IDE.InlayHints.Common where

import AST.Cast
import AST.Haskell.Generated qualified as Haskell
import AST.Node
import Data.LineCol
import Data.List
import Data.Maybe
import Data.Pos
import Data.Text (Text)
import Data.Text qualified as Text
import StaticLS.IDE.InlayHints.Types

defaultInlayHint :: InlayHint
defaultInlayHint = InlayHint {position = LineCol (Pos 0) (Pos 0), kind = Nothing, label = Left "", textEdits = Nothing, paddingLeft = Nothing, paddingRight = Nothing}

mkInlayText :: LineCol -> Text -> InlayHint
mkInlayText lineCol text = defaultInlayHint {position = lineCol, label = Left text}

mkTypedefInlay :: Maybe Int -> LineCol -> Text -> InlayHint
mkTypedefInlay maxLen lineCol text = (mkInlayText lineCol (truncateInlay maxLen (formatInlayText text))) {kind = Just InlayHintKind_Type, paddingLeft = Just True}

formatInlayText :: Text -> Text
formatInlayText = normalizeWhitespace
 where
  normalizeWhitespace = Text.unwords . Text.words

truncateInlay :: Maybe Int -> Text -> Text
truncateInlay Nothing text = text
truncateInlay (Just maxLen) text
  | Text.length text <= maxLen = text
  | otherwise = Text.take maxLen text <> "\x2026"

-- AST locations encoded in a manner similar to zippers.
data ASTLoc = ASTLoc {root :: DynNode, path :: [(Int, DynNode)]}

rootToASTLoc :: DynNode -> ASTLoc
rootToASTLoc hs = ASTLoc hs []

nodeAtLoc :: ASTLoc -> DynNode
nodeAtLoc ASTLoc {root, path} = case path of
  [] -> getDynNode root
  (_, x) : _ -> x

parent :: ASTLoc -> Maybe ASTLoc
parent ASTLoc {..} = case path of
  [] -> Nothing
  _ : xs -> Just ASTLoc {root, path = xs}

children :: ASTLoc -> [ASTLoc]
children loc@ASTLoc {..} = do
  let curNode = nodeAtLoc loc
  let toChildLoc ix node = loc {root, path = (ix, node) : path}
  zipWith toChildLoc [0 ..] curNode.nodeChildren

ancestors :: ASTLoc -> [ASTLoc]
ancestors ASTLoc {..} = ASTLoc root <$> tails path

childIndex :: ASTLoc -> Maybe Int
childIndex astLoc = fst <$> listToMaybe astLoc.path

descendants :: ASTLoc -> [ASTLoc]
descendants astLoc = astLoc : (children astLoc >>= descendants)

leaves :: ASTLoc -> [ASTLoc]
leaves loc = case children loc of
  [] -> pure loc
  children -> children >>= leaves

findAncestor :: (ASTLoc -> Bool) -> ASTLoc -> Maybe ASTLoc
findAncestor pred astLoc = find pred (ancestors astLoc)

nthChildOf :: Int -> (DynNode -> Bool) -> ASTLoc -> Bool
nthChildOf n test astLoc = childIndex astLoc == Just n && maybe False (test . nodeAtLoc) (parent astLoc)
