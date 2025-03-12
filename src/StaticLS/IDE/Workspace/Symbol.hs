{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.Workspace.Symbol (
  Symbol (..),
  symbolInfo,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.LineColRange (LineColRange (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Path qualified as Path
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Plugins qualified as GHC
import GHC.Stack (HasCallStack)
import HieDb qualified
import StaticLS.HIE.File (hieFilePathToSrcFilePath)
import StaticLS.HIE.Position
import StaticLS.IDE.FileWith (FileLcRange, FileWith' (..))
import StaticLS.IDE.SymbolKind (SymbolKind (..))
import StaticLS.IDE.SymbolKind qualified as SymbolKind
import StaticLS.Logger (HasLogger)
import StaticLS.Maybe
import StaticLS.StaticEnv (HasStaticEnv, runHieDbMaybeT)

data Symbol = Symbol
  { name :: !Text
  , kind :: !SymbolKind
  , loc :: !FileLcRange
  }
  deriving (Show, Eq)

symbolInfo :: (HasCallStack, HasStaticEnv m, HasLogger m, MonadIO m) => T.Text -> m [Symbol]
symbolInfo query = do
  mHiedbDefs <- runMaybeT . runHieDbMaybeT $ \hieDb -> HieDb.searchDef hieDb (T.unpack query)
  let hiedbDefs = fromMaybe [] mHiedbDefs
  symbols <- mapM defRowToSymbolInfo hiedbDefs
  pure (catMaybes symbols)

-- Copy from https://github.com/haskell/haskell-language-server/blob/c126332850d27abc8efa519f8437ff7ea28d4049/ghcide/src/Development/IDE/Spans/AtPoint.hs#L392
-- With following modification
-- a. instead of replying on `modInfoSrcFile` (which is only present when hiedb index with `--src-base-dir`)
--    we could find src file path from `hieFilePathToSrcFilePath`
defRowToSymbolInfo :: (HasStaticEnv m, HasLogger m, MonadIO m) => HieDb.Res HieDb.DefRow -> m (Maybe Symbol)
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
    | GHC.isVarOcc defNameOcc = Just SymbolKind.Variable
    | GHC.isDataOcc defNameOcc = Just SymbolKind.Constructor
    | GHC.isTcOcc defNameOcc = Just SymbolKind.Type
    | otherwise = Nothing
  range = LineColRange start end
  start = hiedbCoordsToLineCol (defSLine, defSCol)
  end = hiedbCoordsToLineCol (defELine, defECol)
