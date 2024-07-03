module StaticLS.Tree (
  getImports,
  Imports (..),
  getHeader,
  byteToPos,
  Qualified (..),
  parseQualified,
  getQualifiedAtPoint,
)
where

import AST qualified
import AST.Haskell qualified as Haskell
import Data.Either.Extra qualified as Either.Extra
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe qualified as Maybe
import Data.Pos (Pos (..))
import Data.Range (Range)
import Data.Rope (Rope)

byteToPos :: Rope -> Int -> Pos
byteToPos _rope byte = Pos byte

getHeader :: Haskell.Haskell -> AST.Err (Maybe Haskell.Header)
getHeader haskell = do
  header <- AST.collapseErr haskell.children
  pure header

data Imports = Imports
  { dynNode :: AST.DynNode
  , imports :: [Haskell.Import]
  }

filterErr :: (Foldable f) => f (AST.Err a) -> [a]
filterErr = Maybe.mapMaybe Either.Extra.eitherToMaybe . Foldable.toList

getImports :: Haskell.Haskell -> AST.Err (Maybe Imports)
getImports hs = do
  imports <- AST.collapseErr hs.imports
  case imports of
    Nothing -> pure Nothing
    Just imports -> do
      import' <- imports.import'
      let importsFiltered = filterErr import'
      pure $
        Just
          Imports
            { imports = importsFiltered
            , dynNode = imports.dynNode
            }

data Qualified = Qualified
  { modIds :: NonEmpty AST.DynNode
  , id :: AST.DynNode
  }

parseQualified :: Haskell.Qualified -> AST.Err Qualified
parseQualified qualified = do
  module' <- qualified.module'
  modIds <- AST.collapseErr module'.children
  modIds <- pure $ fmap AST.getDynNode modIds
  id <- qualified.id
  id <- pure $ AST.getDynNode id
  pure Qualified {modIds, id}

getQualifiedAtPoint :: Haskell.Haskell -> Range -> AST.Err (Maybe Qualified)
getQualifiedAtPoint hs pos = do
  let node = AST.getDeepestContaining @Haskell.Qualified pos (AST.getDynNode hs)
  case node of
    Nothing -> pure Nothing
    Just node -> do
      qualified <- parseQualified node
      pure $ Just qualified
