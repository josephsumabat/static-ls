{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

module StaticLS.IDE.Completion
  ( getCompletion,
    Context (..),
    TriggerKind (..),
    Completion (..),
  )
where

import AST qualified
import AST.Haskell qualified as H
import AST.Haskell qualified as Haskell
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.Functor.Identity qualified as Identity
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (LineCol (..), Pos (..))
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.TextUtils qualified as TextUtils
import Data.Traversable (for)
import Database.SQLite.Simple qualified as SQL
import HieDb (HieDb)
import HieDb qualified
import StaticLS.HIE.Queries (allGlobalSymbols)
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.Monad
import StaticLS.Logger (logInfo)
import StaticLS.Monad
import StaticLS.Semantic.Position qualified as Semantic.Position
import StaticLS.StaticEnv
import StaticLS.Tree qualified as Tree
import StaticLS.Utils (isRightOrThrowT)
import System.FilePath

makeRelativeMaybe :: FilePath -> FilePath -> Maybe FilePath
makeRelativeMaybe base path = do
  let rel = makeRelative base path
  guard $ path /= rel
  pure rel

pathToModule :: AbsPath -> StaticLsM (Maybe Text)
pathToModule absPath = do
  let fp = Path.toFilePath absPath
  staticEnv <- getStaticEnv
  let srcDirs = staticEnv.srcDirs
  let wsRoot = staticEnv.wsRoot
  pure $ do
    modPath <- asum ((\srcDir -> makeRelativeMaybe (Path.toFilePath srcDir) fp) <$> srcDirs)
    let (modPathWithoutExt, ext) = splitExtension modPath
    guard $ ext == ".hs"
    let modText = T.replace (T.pack [pathSeparator]) "." (T.pack modPathWithoutExt)
    pure modText

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

getModules :: HieDb -> IO [Text]
getModules (HieDb.getConn -> conn) = do
  res <-
    SQL.query_
      @(SQL.Only Text)
      conn
      "SELECT DISTINCT mod FROM mods"
  pure $ coerce res

getCompletionsForImport :: Hir.Import -> StaticLsM [Completion]
getCompletionsForImport imp = do
  res <- runMaybeT $ runHieDbMaybeT \hiedb -> do
    getExportsForMod hiedb imp.mod.text
  res <- pure $ Maybe.fromMaybe [] res
  pure $ fmap (\text -> Completion {label = text, insertText = text}) res

getCompletionsForImports :: [Hir.Import] -> StaticLsM [Completion]
getCompletionsForImports imports = do
  importCompletions <- for imports \imp -> do
    getCompletionsForImport imp
  pure $ concat importCompletions

-- getExportsForMod
--   pure []
getFileCompletions :: Context -> StaticLsM [Completion]
getFileCompletions cx = do
  let path = cx.path
  fileCompletions <-
    runMaybeT $ do
      hieFile <- getHieFile path
      let symbols = allGlobalSymbols hieFile
      let symbolsNubbed = nubOrd symbols
      -- logInfo $ "symbols: " <> T.pack (show symbols)
      let completions = fmap (\symbol -> Completion {label = symbol, insertText = symbol}) symbolsNubbed
      pure completions
  fileCompletions <- pure $ Maybe.fromMaybe [] fileCompletions
  pure fileCompletions

getUnqualifiedImportCompletions :: Context -> StaticLsM [Completion]
getUnqualifiedImportCompletions cx = do
  let path = cx.path
  haskell <- getHaskell path
  let prog = Hir.parseHaskell haskell
  (_errs, prog) <- isRightOrThrowT prog
  let imports = prog.imports
  let unqualifiedImports = filter (\imp -> not imp.qualified) imports
  getCompletionsForImports unqualifiedImports

data CompletionMode
  = ImportMode !Text
  | HeaderMode !Text
  | QualifiedMode !Text
  | UnqualifiedMode
  deriving (Show, Eq)

getModulePrefix :: Context -> Rope -> Maybe Text
getModulePrefix cx sourceRope = do
  let lineCol = cx.lineCol
  let line = Rope.toText $ Maybe.fromMaybe "" $ Rope.getLine sourceRope (Pos lineCol.line)
  let (beforeCol, _afterCol) = T.splitAt lineCol.col line
  let (_, prefix) = Identity.runIdentity $ T.spanEndM (pure . not . Char.isSpace) beforeCol
  let firstChar = fst <$> T.uncons prefix
  let firstIsUpper = case firstChar of
        Just c -> Char.isUpper c
        Nothing -> False
  let containsDot = T.unpack prefix & elem '.'
  let (mod, _) = T.breakOn "." prefix
  if firstIsUpper && containsDot
    then Just mod
    else Nothing

getImportPrefix :: Context -> Rope -> H.Haskell -> Maybe Text
getImportPrefix cx sourceRope hs = do
  let lineCol = cx.lineCol
  let line = Rope.toText $ Maybe.fromMaybe "" $ Rope.getLine sourceRope (Pos lineCol.line)
  let astPoint = Semantic.Position.lineColToAstPoint cx.lineCol
  let imports = AST.getDeepestContaining @Haskell.Imports astPoint (AST.getDynNode hs)
  case "imports" `T.stripPrefix` line of
    Just rest | Maybe.isJust imports -> do
      let mod = T.dropWhile Char.isSpace rest
      let modPrefix = TextUtils.splitOnceEnd "." mod
      Just $ Maybe.fromMaybe "" $ fst <$> modPrefix
    _ -> Nothing

getCompletionMode :: Context -> StaticLsM CompletionMode
getCompletionMode cx = do
  let path = cx.path
  haskell <- getHaskell path
  header <- Tree.getHeader haskell & isRightOrThrowT
  sourceRope <- getSourceRope path
  mod <- pathToModule path
  if
    | (Nothing, Just mod) <- (header, mod) -> pure $ HeaderMode mod
    | Just modPrefix <- getImportPrefix cx sourceRope haskell -> do
        pure $ ImportMode modPrefix
    | Just mod <- getModulePrefix cx sourceRope -> do
        pure $ QualifiedMode mod
    | otherwise -> do
        pure UnqualifiedMode

getCompletion :: Context -> StaticLsM [Completion]
getCompletion cx = do
  logInfo $ "triggerKind: " <> T.pack (show cx.triggerKind)
  mode <- getCompletionMode cx
  logInfo $ "mode: " <> T.pack (show mode)
  case mode of
    ImportMode modPrefix -> do
      res <- runMaybeT $ runHieDbMaybeT \hiedb -> getModules hiedb
      res <- pure $ Maybe.fromMaybe [] res
      pure $ textCompletion <$> res
    HeaderMode mod -> do
      let label = "module " <> mod <> " where"
      pure [Completion {label, insertText = label <> "\n$0"}]
    UnqualifiedMode -> do
      fileCompletions <- getFileCompletions cx
      importCompletions <- getUnqualifiedImportCompletions cx
      pure $ nubOrd $ importCompletions ++ fileCompletions
    QualifiedMode mod -> do
      let path = cx.path
      haskell <- getHaskell path
      let prog = Hir.parseHaskell haskell
      (_errs, prog) <- isRightOrThrowT prog
      let imports = prog.imports
      let importsWithAlias = filter (\imp -> fmap (.text) imp.alias == Just mod) imports
      logInfo $ "importsWithAlias: " <> T.pack (show importsWithAlias)
      nubOrd <$> getCompletionsForImports importsWithAlias

data Completion = Completion
  { label :: !Text,
    insertText :: !Text
  }
  deriving (Show, Eq, Ord)

textCompletion :: Text -> Completion
textCompletion text = Completion {label = text, insertText = text}

data TriggerKind = TriggerCharacter | TriggerUnknown
  deriving (Show, Eq)

data Context = Context
  { path :: AbsPath,
    pos :: !Pos,
    lineCol :: !LineCol,
    triggerKind :: !TriggerKind
  }
  deriving (Show, Eq)
