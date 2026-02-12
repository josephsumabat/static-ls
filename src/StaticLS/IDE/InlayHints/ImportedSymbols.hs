module StaticLS.IDE.InlayHints.ImportedSymbols (
  ImportedSymbol (..),
  getImportedSymbols,
  cleanOccName,
  groupByModule,
)
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Char (isUpper)
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
  -- ^ module that provides the symbol (after resolving re-exports)
  }
  deriving (Show, Eq)

instance SQL.FromRow ImportedSymbol where
  fromRow = ImportedSymbol <$> SQL.field <*> SQL.field

getImportedSymbols :: (HasStaticEnv m, MonadIO m) => Text -> MaybeT m [ImportedSymbol]
getImportedSymbols moduleName =
  runHieDbMaybeT \hieDb ->
    SQL.queryNamed
      (HieDb.getConn hieDb)
      "SELECT DISTINCT r.occ, exportMods.mod \
      \FROM mods cur \
      \JOIN refs r ON r.hieFile = cur.hieFile \
      \JOIN exports e ON e.occ = r.occ AND (e.mod = r.mod OR e.mod IS NULL) \
      \JOIN mods exportMods ON exportMods.hieFile = e.hieFile \
      \WHERE cur.mod = :module \
      \  AND (r.mod IS NULL OR r.mod <> cur.mod)"
      [":module" SQL.:= moduleName]

-- | Clean the occ name by stripping the type prefix (v:, t:, c:)
-- and converting record field selectors to Type(..) syntax
cleanOccName :: Text -> Text
cleanOccName occ =
  -- Try stripping v: prefix (values may be field selectors)
  -- Then try t: or c: prefixes (types/constructors, just strip prefix)
  -- Finally try handling as bare field selector and strip operator modules
  stripOperatorModule $
    case T.stripPrefix "v:" occ of
      Just rest -> handleFieldSelector rest
      Nothing ->
        fromMaybe (handleFieldSelector occ) $
          asum [T.stripPrefix "t:" occ, T.stripPrefix "c:" occ]
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
      =
        typePart <> "(..)"
    | otherwise = name

  -- Operators get mangled as op|Module; drop the module suffix
  stripOperatorModule :: Text -> Text
  stripOperatorModule name =
    case T.breakOn "|" name of
      (op, rest) | not (T.null rest) -> op
      (op, rest)
        | Just moduleSuffix <- T.stripPrefix "|" rest
        , not (T.null moduleSuffix) ->
            op
      _ -> name

-- | Group imported symbols by their source module,
-- collapsing record field selectors into Type(..) syntax
groupByModule :: [ImportedSymbol] -> Map Text [Text]
groupByModule symbols =
  let cleaned = [(normalizeModule sym.isMod, cleanOccName sym.isOcc) | sym <- symbols]
   in -- Use Set for O(n log n) deduplication; toList returns sorted elements
      Map.map Set.toList $ foldr (\(mod', sym) -> Map.insertWith (<>) mod' (Set.singleton sym)) Map.empty cleaned

-- | Drop the unit-id prefix that HieDb stores for cross-package refs.
-- Local modules don't include a prefix so they are returned unchanged.
normalizeModule :: Text -> Text
normalizeModule modName =
  case T.breakOnEnd ":" modName of
    (_prefix, suffix) | not (T.null suffix) -> suffix
    _ -> modName
