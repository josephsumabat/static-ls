module StaticLS.IDE.InlayHints.ImportedSymbols
  ( ImportedSymbol (..)
  , getImportedSymbols
  , cleanOccName
  , groupByModule
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
-- and converting record field selectors to Type(..) syntax
cleanOccName :: Text -> Text
cleanOccName occ = case T.stripPrefix "v:" occ of
  Just rest -> handleFieldSelector rest
  Nothing -> case T.stripPrefix "t:" occ of
    Just rest -> rest
    Nothing -> case T.stripPrefix "c:" occ of
      Just rest -> rest
      -- No prefix - might be a bare field selector like "fTypeName:fieldName"
      Nothing -> handleFieldSelector occ
  where
    -- Record field selectors look like "fTypeName:fieldName"
    -- We want to convert these to "TypeName(..)"
    handleFieldSelector :: Text -> Text
    handleFieldSelector name
      | Just rest <- T.stripPrefix "f" name
      , not (T.null rest)
      , isUpper (T.head rest) -- ensure it's fUppercase...
      , (typePart, colonRest) <- T.breakOn ":" rest
      , not (T.null colonRest) -- has a colon
      = typePart <> "(..)"
      | otherwise = name

    isUpper c = c >= 'A' && c <= 'Z'

-- | Group imported symbols by their source module,
-- collapsing record field selectors into Type(..) syntax
groupByModule :: [ImportedSymbol] -> Map Text [Text]
groupByModule symbols =
  let cleaned = [(sym.isMod, cleanOccName sym.isOcc) | sym <- symbols]
      -- Use nub to deduplicate (multiple fields from same type -> one Type(..))
   in Map.map nub $ foldr (\(mod', sym) -> Map.insertWith (<>) mod' [sym]) Map.empty cleaned
