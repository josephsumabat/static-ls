{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.Workspace.Symbol (
  Symbol (..),
  symbolInfo,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Parallel.Strategies qualified as Parallel
import Data.ConcurrentCache qualified as ConcurrentCache
import Data.LineColRange (LineColRange (..))
import Data.Maybe (catMaybes)
import Data.Path qualified as Path
import Data.Text qualified as T
import GHC.Plugins qualified as GHC
import HieDb qualified
import StaticLS.HIE.File (hieFilePathToSrcFilePath)
import StaticLS.HIE.Position
import StaticLS.IDE.FileWith (FileWith' (..))
import StaticLS.IDE.Monad (IdeEnv (..), MonadIde, getIdeEnv)
import StaticLS.IDE.Symbol (Symbol (..))
import StaticLS.IDE.Symbol qualified as Symbol
import StaticLS.Maybe
import StaticLS.StaticEnv (HasStaticEnv, runHieDbMaybeT)
import StaticLS.Utils (isJustOrThrowS)
import Text.Fuzzy qualified as Fuzzy

symbolInfo :: (MonadIde m) => T.Text -> m [Symbol]
symbolInfo query = do
  symbols <- getSymbols
  let fuzzySymbols = fmap (\t -> Fuzzy.match query t "" "" (.name) True) symbols `Parallel.using` Parallel.parListChunk 1000 Parallel.rseq
  let fuzzySymbols' = catMaybes fuzzySymbols
  pure $ Fuzzy.original <$> fuzzySymbols'

strictMapMaybe :: (a -> Maybe b) -> [a] -> [b]
strictMapMaybe f = go []
 where
  go acc xs = case xs of
    [] -> acc
    (x : xs) -> case f x of
      Just y -> go (y : acc) xs
      Nothing -> go acc xs

getSymbols :: (MonadIde m) => m [Symbol]
getSymbols = do
  env <- getIdeEnv
  ConcurrentCache.insert
    ()
    ( do
        mHiedbDefs <- runMaybeT . runHieDbMaybeT $ \hieDb -> HieDb.searchDef hieDb ("")
        hiedbDefs <- isJustOrThrowS "could not get symbols" mHiedbDefs
        symbols <- mapM defRowToSymbolInfo hiedbDefs
        symbols' <- pure $ catMaybes symbols
        pure symbols'
    )
    env.workspaceSymbolCache

-- Copy from https://github.com/haskell/haskell-language-server/blob/c126332850d27abc8efa519f8437ff7ea28d4049/ghcide/src/Development/IDE/Spans/AtPoint.hs#L392
-- With following modification
-- a. instead of replying on `modInfoSrcFile` (which is only present when hiedb index with `--src-base-dir`)
--    we could find src file path from `hieFilePathToSrcFilePath`
defRowToSymbolInfo :: (HasStaticEnv m, MonadIO m) => HieDb.Res HieDb.DefRow -> m (Maybe Symbol)
defRowToSymbolInfo (HieDb.DefRow {..} HieDb.:. _) = runMaybeT $ do
  do
    defSrc <- Path.filePathToAbs defSrc
    srcFile <- hieFilePathToSrcFilePath defSrc
    let file = srcFile
        loc = FileWith file range
    kind <- toAlt mKind
    pure $
      Symbol
        { name = T.pack $ GHC.occNameString defNameOcc
        , kind = kind
        , loc = loc
        }
 where
  mKind
    | GHC.isVarOcc defNameOcc = Just Symbol.Variable
    | GHC.isDataOcc defNameOcc = Just Symbol.Constructor
    | GHC.isTcOcc defNameOcc = Just Symbol.Type
    | otherwise = Nothing
  range = LineColRange start end
  start = hiedbCoordsToLineCol (defSLine, defSCol)
  end = hiedbCoordsToLineCol (defELine, defECol)
