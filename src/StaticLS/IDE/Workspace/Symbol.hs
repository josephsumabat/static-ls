{-# LANGUAGE RecordWildCards #-}

module StaticLS.IDE.Workspace.Symbol where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Development.IDE.GHC.Util (printOutputable)
import Development.IDE.Types.Location
import GHC.Plugins hiding ((<>))
import qualified HieDb
import Language.LSP.Types
import StaticLS.HIE.File (hieFilePathToSrcFilePath)
import StaticLS.StaticEnv (HasStaticEnv, runHieDbMaybeT)

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
defRowToSymbolInfo (HieDb.DefRow{..} HieDb.:. _) = runMaybeT $ do
    do
        srcFile <- hieFilePathToSrcFilePath defSrc
        let file = toUri srcFile
            loc = Location file range
        pure $ SymbolInformation (printOutputable defNameOcc) kind Nothing Nothing loc Nothing
  where
    kind
        | isVarOcc defNameOcc = SkVariable
        | isDataOcc defNameOcc = SkConstructor
        | isTcOcc defNameOcc = SkStruct
        | otherwise = SkUnknown 1
    range = Range start end
    start = Position (fromIntegral $ defSLine - 1) (fromIntegral $ defSCol - 1)
    end = Position (fromIntegral $ defELine - 1) (fromIntegral $ defECol - 1)

toUri :: FilePath -> Uri
toUri = fromNormalizedUri . filePathToUri' . toNormalizedFilePath'
