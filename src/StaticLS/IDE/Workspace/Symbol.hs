module StaticLS.IDE.Workspace.Symbol where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe
import qualified Data.Text as T
import qualified Development.IDE.Spans.AtPoint as AtPoint
import GHC.Plugins hiding ((<>))
import qualified HieDb
import Language.LSP.Types
import StaticLS.StaticEnv

symbolInfo :: (HasCallStack, HasStaticEnv m, MonadIO m) => T.Text -> m [SymbolInformation]
symbolInfo query = do
    mHiedbDefs <- runMaybeT . runHieDbMaybeT $ \hieDb -> HieDb.searchDef hieDb (T.unpack query)
    let hiedbDefs = fromMaybe [] mHiedbDefs
        symbols = map AtPoint.defRowToSymbolInfo hiedbDefs
    pure (catMaybes symbols)

