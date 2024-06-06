{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.Workspace.Symbol where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Path qualified as Path
import Data.Text qualified as T
import Development.IDE.GHC.Util (printOutputable)
import Development.IDE.Types.Location
import GHC.Plugins hiding ((<>))
import HieDb qualified
import Language.LSP.Protocol.Types
import StaticLS.HIE.File (hieFilePathToSrcFilePath)
import StaticLS.Maybe
import StaticLS.StaticEnv (HasStaticEnv, runHieDbMaybeT)
import qualified StaticLS.ProtoLSP as ProtoLSP

symbolInfo :: (HasCallStack, HasStaticEnv m, MonadIO m) => T.Text -> m [SymbolInformation]
symbolInfo query = do
  mHiedbDefs <- runMaybeT . runHieDbMaybeT $ \hieDb -> HieDb.searchDef hieDb (T.unpack query)
  let hiedbDefs = fromMaybe [] mHiedbDefs
  symbols <- mapM defRowToSymbolInfo hiedbDefs
  pure (catMaybes symbols)

-- Copy from https://github.com/haskell/haskell-language-server/blob/c126332850d27abc8efa519f8437ff7ea28d4049/ghcide/src/Development/IDE/Spans/AtPoint.hs#L392
-- With following modification
-- a. instead of replying on `modInfoSrcFile` (which is only present when hiedb index with `--src-base-dir`)
--    we could find src file path from `hieFilePathToSrcFilePath`
defRowToSymbolInfo :: (HasStaticEnv m, MonadIO m) => HieDb.Res HieDb.DefRow -> m (Maybe SymbolInformation)
defRowToSymbolInfo (HieDb.DefRow {..} HieDb.:. _) = runMaybeT $ do
  do
    defSrc <- Path.filePathToAbs defSrc
    srcFile <- hieFilePathToSrcFilePath defSrc
    let file = ProtoLSP.absPathToUri srcFile
        loc = Location file range
    kind <- toAlt mKind
    pure $
      SymbolInformation
        { _name = printOutputable defNameOcc
        , _kind = kind
        , _tags = Nothing
        , _containerName = Nothing
        , _deprecated = Nothing
        , _location = loc
        }
 where
  mKind
    | isVarOcc defNameOcc = Just SymbolKind_Variable
    | isDataOcc defNameOcc = Just SymbolKind_Constructor
    | isTcOcc defNameOcc = Just SymbolKind_Struct
    | otherwise = Nothing
  range = Range start end
  start = Position (fromIntegral $ defSLine - 1) (fromIntegral $ defSCol - 1)
  end = Position (fromIntegral $ defELine - 1) (fromIntegral $ defECol - 1)

toUri :: FilePath -> Uri
toUri = fromNormalizedUri . filePathToUri' . toNormalizedFilePath'
