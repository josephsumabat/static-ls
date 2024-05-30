{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FieldSelectors #-}

module StaticLS.TreeSitter.Query where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (MonadPlus (..), guard)
import Data.Text (Text)
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import TreeSitter.Api (Node (..), Symbol (..))

newtype NodeQuery a = NodeQuery {runNodeQuery :: Tree Node -> Maybe a}

newtype ChildQuery a = ChildQuery {runChildQuery :: [Tree Node] -> Maybe (a, [Tree Node])}

child :: NodeQuery a -> ChildQuery a
child (NodeQuery f) = ChildQuery \children -> case children of
  child : rest -> case f child of
    Just x -> Just (x, rest)
    Nothing -> Nothing
  [] -> Nothing

namedChild :: Text -> NodeQuery a -> ChildQuery a
namedChild name (NodeQuery f) = ChildQuery \children -> case dropWhile (\(Tree.Node node _) -> node.nodeFieldName /= Just name) children of
  node@(Tree.Node nodeHead _) : rest | nodeHead.nodeFieldName == Just name -> case f node of
    Just x -> Just (x, rest)
    Nothing -> Nothing
  _ -> Nothing

(#=) :: Text -> NodeQuery a -> ChildQuery a
(#=) = namedChild

instance Functor NodeQuery where
  fmap f (NodeQuery g) = NodeQuery \tree -> f <$> g tree

-- The Applicative and Monad instance for NodeQuery make sure the **both** queries succeeed on the same tree and combine the results
instance Applicative NodeQuery where
  pure x = NodeQuery \_ -> Just x
  NodeQuery f <*> NodeQuery g = NodeQuery \tree -> f tree <*> g tree
  liftA2 f (NodeQuery g) (NodeQuery h) = NodeQuery \tree -> liftA2 f (g tree) (h tree)

instance Monad NodeQuery where
  NodeQuery f >>= g = NodeQuery \tree -> case f tree of
    Just x -> runNodeQuery (g x) tree
    Nothing -> Nothing

instance Alternative NodeQuery where
  empty = NodeQuery \_ -> Nothing
  NodeQuery f <|> NodeQuery g = NodeQuery \tree -> f tree <|> g tree

instance MonadPlus NodeQuery where
  mzero = empty
  mplus = (<|>)

instance Functor ChildQuery where
  fmap f (ChildQuery g) = ChildQuery \children -> case g children of
    Just (x, rest) -> Just (f x, rest)
    Nothing -> Nothing

instance Applicative ChildQuery where
  pure x = ChildQuery \rest -> Just (x, rest)
  ChildQuery f <*> ChildQuery g = ChildQuery \children -> case f children of
    Just (h, rest) -> case g rest of
      Just (x, rest') -> Just (h x, rest')
      Nothing -> Nothing
    Nothing -> Nothing

instance Monad ChildQuery where
  ChildQuery f >>= g = ChildQuery \children -> case f children of
    Just (x, rest) -> runChildQuery (g x) rest
    Nothing -> Nothing

instance Alternative ChildQuery where
  empty = ChildQuery \_ -> Nothing
  ChildQuery f <|> ChildQuery g = ChildQuery \children -> f children <|> g children

instance MonadPlus ChildQuery where
  mzero = empty
  mplus = (<|>)

withNode :: (Node -> Maybe a) -> (a -> ChildQuery b) -> NodeQuery b
withNode f g = NodeQuery \(Tree.Node node children) -> case f node of
  Just x -> case runChildQuery (g x) children of
    Just (y, []) -> Just y
    _ -> Nothing
  Nothing -> Nothing

node :: (Node -> Maybe a) -> ChildQuery b -> NodeQuery (a, b)
node f g = withNode f \x -> do y <- g; pure (x, y)

node_ :: (Node -> Maybe a) -> ChildQuery b -> NodeQuery b
node_ f g = fmap snd (node f g)

nodeName :: Text -> ChildQuery b -> NodeQuery (Node, b)
nodeName name = node (\node -> do guard (node.nodeSymbol.symbolName == name); pure node)

nodeName_ :: Text -> ChildQuery b -> NodeQuery b
nodeName_ name child = fmap snd (nodeName name child)

query :: Tree Node -> NodeQuery a -> Maybe a
query tree (NodeQuery f) = f tree