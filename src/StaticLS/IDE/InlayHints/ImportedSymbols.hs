module StaticLS.IDE.InlayHints.ImportedSymbols
  ( ImportedSymbol (..)
  , getImportedSymbols
  , cleanOccName
  , groupByModule
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQL
import HieDb qualified
import StaticLS.StaticEnv (HasStaticEnv, runHieDbMaybeT)

-- | Result row for imported symbols
data ImportedSymbol = ImportedSymbol
  { isOcc :: Text
  -- ^ occurrence name (with prefix like "v:", "t:", "c:")
  , isMod :: Text
  -- ^ source module
  }
  deriving (Show, Eq)

instance SQL.FromRow ImportedSymbol where
  fromRow = ImportedSymbol <$> SQL.field <*> SQL.field

-- | Get all symbols referenced in a file that aren't locally declared.
-- These are the imported symbols.
getImportedSymbols :: (HasStaticEnv m, MonadIO m) => FilePath -> MaybeT m [ImportedSymbol]
getImportedSymbols hieFilePath =
  runHieDbMaybeT \hieDb ->
    SQL.queryNamed
      (HieDb.getConn hieDb)
      "SELECT DISTINCT r.occ, r.mod \
      \FROM refs r \
      \WHERE r.hieFile = :hieFile \
      \  AND NOT EXISTS ( \
      \    SELECT 1 FROM decls d \
      \    WHERE d.hieFile = r.hieFile \
      \      AND d.occ = r.occ \
      \  ) \
      \  AND r.occ NOT LIKE 'v:$%'"
      [":hieFile" SQL.:= hieFilePath]

-- | Clean the occ name by stripping the type prefix (v:, t:, c:)
cleanOccName :: Text -> Text
cleanOccName occ
  | "t:" `T.isPrefixOf` occ = T.drop 2 occ
  | "v:" `T.isPrefixOf` occ = T.drop 2 occ
  | "c:" `T.isPrefixOf` occ = T.drop 2 occ
  | otherwise = occ

-- | Group imported symbols by their source module
groupByModule :: [ImportedSymbol] -> Map Text [Text]
groupByModule = foldr (\sym -> Map.insertWith (<>) sym.isMod [cleanOccName sym.isOcc]) Map.empty
