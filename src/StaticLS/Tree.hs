module StaticLS.Tree (
  getImports,
  Imports (..),
  getHeader,
  byteToPos,
)
where

import AST qualified
import AST.Haskell qualified as Haskell
import Data.Either.Extra qualified as Either.Extra
import Data.Foldable qualified as Foldable
import Data.Maybe qualified as Maybe
import Data.Pos (Pos (..))
import Data.Rope (Rope)

byteToPos :: Rope -> Int -> Pos
byteToPos rope byte = Pos byte

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
            , dynNode = imports.dynNode.unDynNode
            }
