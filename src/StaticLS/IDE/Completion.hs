{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

module StaticLS.IDE.Completion (
  getCompletion,
  Context (..),
  TriggerKind (..),
  Completion (..),
  CompletionKind (..),
  CompletionMessage (..),
  resolveCompletionEdit,
)
where

import AST qualified
import AST.Haskell qualified as H
import AST.Haskell qualified as Haskell
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson qualified as Aeson
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.Containers.ListUtils (nubOrd)
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.Function ((&))
import Data.Functor.Identity qualified as Identity
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.LineCol (LineCol (..))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Pos (Pos (..))
import Data.Range qualified as Range
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.TextUtils qualified as TextUtils
import Data.Traversable (for)
import Database.SQLite.Simple qualified as SQL
import GHC.Generics (Generic)
import HieDb (HieDb)
import HieDb qualified
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.CodeActions.AutoImport qualified as IDE.CodeActions.AutoImport
import StaticLS.IDE.Monad
import StaticLS.IDE.Utils qualified as IDE.Utils
import StaticLS.Logger (logInfo)
import StaticLS.Monad
import StaticLS.StaticEnv
import StaticLS.Tree qualified as Tree
import StaticLS.Utils (isRightOrThrowT)

stripNameSpacePrefix :: Text -> Text
stripNameSpacePrefix t = snd $ T.breakOnEnd ":" t

getExportsForMod :: HieDb -> Text -> IO [Text]
getExportsForMod (HieDb.getConn -> conn) mod = do
  res <-
    SQL.query @_ @(SQL.Only Text)
      conn
      "SELECT DISTINCT exports.occ \
      \FROM exports \
      \JOIN mods using (hieFile) \
      \WHERE mods.mod = ?"
      (SQL.Only mod)
  pure $ fmap stripNameSpacePrefix $ coerce res

getExportsForModWithPrefix :: HieDb -> Text -> Text -> IO [(Text)]
getExportsForModWithPrefix (HieDb.getConn -> conn) mod prefix = do
  res <-
    SQL.query @_ @(SQL.Only Text)
      conn
      "SELECT DISTINCT exports.occ \
      \FROM exports \
      \JOIN mods using (hieFile) \
      \WHERE mods.mod = ? AND exports.occ LIKE ?"
      (mod, "_:" <> prefix <> "%")
  pure $ fmap stripNameSpacePrefix $ coerce res

getModules :: StaticLsM [Text]
getModules = do
  mods <- runMaybeT $ runHieDbMaybeT \(HieDb.getConn -> conn) -> do
    res <-
      SQL.query_
        @(SQL.Only Text)
        conn
        "SELECT DISTINCT mod FROM mods"
    pure $ coerce res
  pure $ Maybe.fromMaybe [] mods

getCompletionsForMod :: Text -> StaticLsM [Text]
getCompletionsForMod mod = do
  res <- runMaybeT $ runHieDbMaybeT \hiedb -> do
    getExportsForMod hiedb mod
  res <- pure $ Maybe.fromMaybe [] res
  pure res

getCompletionsForModWithPrefix :: Text -> Text -> StaticLsM [Text]
getCompletionsForModWithPrefix mod prefix = do
  res <- runMaybeT $ runHieDbMaybeT \hiedb -> do
    getExportsForModWithPrefix hiedb mod prefix
  res <- pure $ Maybe.fromMaybe [] res
  pure res

getCompletionsForMods :: [Text] -> StaticLsM [Text]
getCompletionsForMods mods = do
  importCompletions <- for mods \mod -> do
    getCompletionsForMod mod
  pure $ concat importCompletions

getFileCompletions :: Context -> StaticLsM [Completion]
getFileCompletions cx = do
  let path = cx.path
  fileCompletions <-
    runMaybeT $ do
      hieView <- getHieView path
      let symbols = fmap HieView.Name.toText $ HieView.Query.fileSymbolsList hieView
      logInfo $ T.pack $ "file symbols: " <> show symbols
      let symbolsNubbed = nubOrd symbols
      let completions = fmap textCompletion symbolsNubbed
      pure completions
  fileCompletions <- pure $ Maybe.fromMaybe [] fileCompletions
  pure fileCompletions

getUnqualifiedImportCompletions :: Context -> StaticLsM [Completion]
getUnqualifiedImportCompletions cx = do
  let path = cx.path
  prog <- getHir path
  let imports = prog.imports
  let unqualifiedImports = filter (\imp -> not imp.qualified) imports
  completions <- getCompletionsForMods $ (.mod.text) <$> unqualifiedImports
  pure $ fmap textCompletion completions

data CompletionMode
  = ImportMode !(Maybe Text)
  | HeaderMode !Text
  | QualifiedMode !Text !Text
  | UnqualifiedMode
  deriving (Show, Eq)

getModulePrefix :: Context -> Rope -> Maybe (Text, Text)
getModulePrefix cx sourceRope = do
  let lineCol = cx.lineCol
  let line = Rope.toText $ Maybe.fromMaybe "" $ Rope.getLine sourceRope lineCol.line
  let (beforeCol, _afterCol) = T.splitAt lineCol.col.pos line
  let (_, prefix) = Identity.runIdentity $ T.spanEndM (pure . not . Char.isSpace) beforeCol
  let firstChar = fst <$> T.uncons prefix
  let firstIsUpper = case firstChar of
        Just c -> Char.isUpper c
        Nothing -> False
  if
    | firstIsUpper, Just (mod, match) <- TextUtils.splitOnceEnd "." prefix -> Just (mod, match)
    | otherwise -> Nothing

getImportPrefix :: Context -> Rope -> H.Haskell -> Maybe (Maybe Text)
getImportPrefix cx sourceRope hs = do
  let lineCol = cx.lineCol
  let pos = cx.pos
  let line = Rope.toText $ Maybe.fromMaybe "" $ Rope.getLine sourceRope lineCol.line
  let imports = AST.getDeepestContaining @Haskell.Imports (Range.empty pos) (AST.getDynNode hs)
  case "import" `T.stripPrefix` line of
    Just rest | Maybe.isJust imports -> do
      let mod = T.dropWhile Char.isSpace rest
      let modPrefix = TextUtils.splitOnceEnd "." mod
      Just $ fst <$> modPrefix
    _ -> Nothing

getCompletionMode :: Context -> StaticLsM CompletionMode
getCompletionMode cx = do
  let path = cx.path
  haskell <- getHaskell path
  header <- Tree.getHeader haskell & isRightOrThrowT
  sourceRope <- getSourceRope path
  mod <- IDE.Utils.pathToModule path
  if
    | (Nothing, Just mod) <- (header, mod) -> pure $ HeaderMode mod.text
    | Just modPrefix <- getImportPrefix cx sourceRope haskell -> do
        pure $ ImportMode modPrefix
    | Just (mod, match) <- getModulePrefix cx sourceRope -> do
        pure $ QualifiedMode mod match
    | otherwise -> do
        pure UnqualifiedMode

defaultAlias :: Text -> Maybe Text
defaultAlias = \case
  "T" -> Just "Data.Text"
  "TE" -> Just "Data.Text.Encoding"
  "B" -> Just "Data.ByteString"
  "BL" -> Just "Data.ByteString.Lazy"
  "TL" -> Just "Data.Text.Lazy"
  "TIO" -> Just "Data.Text.IO"
  _ -> Nothing

bootModules :: [Text]
bootModules =
  [ "Data.Text"
  , "Data.ByteString"
  , "Data.Map"
  , "Data.Set"
  , "Data.IntMap"
  , "Data.IntSet"
  , "Data.Sequence"
  ]

_isModSubseqOf :: Text -> Text -> Bool
_isModSubseqOf sub mod = List.isSubsequenceOf sub' mod' || sub == mod
 where
  sub' = T.splitOn "." sub
  mod' = T.splitOn "." mod

isModSuffixOf :: Text -> Text -> Bool
isModSuffixOf sub mod = sub' `List.isSuffixOf` mod'
 where
  sub' = T.splitOn "." sub
  mod' = T.splitOn "." mod

isBootModule :: Text -> Bool
isBootModule mod = any (mod `isModSuffixOf`) bootModules

formatQualifiedAs :: Text -> Text -> Text
formatQualifiedAs mod alias = "import qualified " <> mod <> " as " <> alias

getFlyImports :: Context -> HashSet Text -> Text -> Text -> StaticLsM [Completion]
getFlyImports cx qualifiedCompletions prefix match = do
  let expandedPrefix = Maybe.fromMaybe prefix (defaultAlias prefix)
  let bootCompletions = [mkBootCompletion expandedPrefix prefix match cx.path | isBootModule expandedPrefix]
  mods <- getModules
  mods <- pure $ filter (expandedPrefix `isModSuffixOf`) mods
  completions <- for mods \mod -> do
    modCompletions <- getCompletionsForModWithPrefix mod match
    -- do some filtering
    modCompletions <-
      pure $
        filter
          ( \completion ->
              not (HashSet.member completion qualifiedCompletions)
          )
          modCompletions
    pure $
      fmap
        ( \completion ->
            (textCompletion completion)
              { description = Just $ formatQualifiedAs mod prefix
              , msg = Just $ CompletionMessage {path = cx.path, kind = FlyImportCompletionKind mod prefix}
              }
        )
        modCompletions
  completions <- pure $ concat completions
  pure $ bootCompletions ++ completions

getCompletion :: Context -> StaticLsM (Bool, [Completion])
getCompletion cx = do
  logInfo $ "triggerKind: " <> T.pack (show cx.triggerKind)
  mode <- getCompletionMode cx
  logInfo $ "mode: " <> T.pack (show mode)
  case mode of
    ImportMode modPrefix -> do
      mods <- getModules
      let modsWithoutPrefix = case modPrefix of
            Just prefix -> Maybe.mapMaybe (T.stripPrefix (prefix <> ".")) mods
            Nothing -> mods
      pure (False, textCompletion <$> modsWithoutPrefix)
    HeaderMode mod -> do
      let label = "module " <> mod <> " where"
      pure
        ( False
        ,
          [ (mkCompletion label (label <> "\n$0"))
              { isSnippet = True
              }
          ]
        )
    UnqualifiedMode -> do
      fileCompletions <- getFileCompletions cx
      importCompletions <- getUnqualifiedImportCompletions cx
      pure (False, nubOrd $ importCompletions ++ fileCompletions)
    QualifiedMode mod match -> do
      prog <- getHir cx.path
      let imports = prog.imports
      let importsWithAlias = filter (\imp -> fmap (.text) imp.alias == Just mod) imports
      -- TODO: append both flyimports and normal ones
      qualifiedCompletions <- nubOrd <$> getCompletionsForMods ((.mod.text) <$> importsWithAlias)
      flyImports <- case match of
        "" -> pure []
        _ -> getFlyImports cx (HashSet.fromList qualifiedCompletions) mod match
      pure (match == "", (textCompletion <$> qualifiedCompletions) ++ flyImports)

resolveCompletionEdit :: CompletionMessage -> StaticLsM Edit
resolveCompletionEdit msg = do
  let path = msg.path
  case msg.kind of
    FlyImportCompletionKind mod alias -> do
      sourceRope <- getSourceRope path
      haskell <- getHaskell path
      change <-
        IDE.CodeActions.AutoImport.insertImportChange haskell sourceRope (formatQualifiedAs mod alias)
          & isRightOrThrowT
      pure $ Edit.singleton change

data CompletionMessage = CompletionMessage
  { path :: AbsPath
  , kind :: CompletionKind
  }
  deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CompletionMessage

instance Aeson.FromJSON CompletionMessage

data CompletionKind
  = FlyImportCompletionKind
      -- | The module to import
      !Text
      -- | The alias to use for the import
      !Text
  deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CompletionKind

instance Aeson.FromJSON CompletionKind

data Completion = Completion
  { label :: !Text
  , insertText :: !Text
  , labelDetail :: Maybe Text
  , description :: Maybe Text
  , detail :: Maybe Text
  , edit :: !Edit
  , msg :: Maybe CompletionMessage
  , isSnippet :: !Bool
  }
  deriving (Show, Eq, Ord)

mkBootCompletion :: Text -> Text -> Text -> AbsPath -> Completion
mkBootCompletion mod alias match path =
  (mkCompletion match "")
    { description = Just $ formatQualifiedAs mod alias
    , msg =
        Just $
          CompletionMessage
            { path
            , kind = FlyImportCompletionKind mod alias
            }
    }

textCompletion :: Text -> Completion
textCompletion text = mkCompletion text text

mkCompletion :: Text -> Text -> Completion
mkCompletion label insertText =
  Completion
    { label
    , detail = Nothing
    , labelDetail = Nothing
    , description = Nothing
    , insertText
    , edit = Edit.empty
    , msg = Nothing
    , isSnippet = False
    }

data TriggerKind = TriggerCharacter | TriggerUnknown
  deriving (Show, Eq)

data Context = Context
  { path :: AbsPath
  , pos :: !Pos
  , lineCol :: !LineCol
  , triggerKind :: !TriggerKind
  }
  deriving (Show, Eq)
