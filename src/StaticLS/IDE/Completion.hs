{-# LANGUAGE BlockArguments #-}

module StaticLS.IDE.Completion (
  getCompletion,
  Context (..),
  TriggerKind (..),
  Completion (..),
)
where

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
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Database.SQLite.Simple qualified as SQL
import HieDb (HieDb)
import HieDb qualified
import StaticLS.HIE.Queries (allGlobalSymbols)
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.Monad
import StaticLS.Logger (logInfo)
import StaticLS.Monad
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
  = HeaderMode !Text
  | QualifiedMode !Text
  | UnqualifiedMode
  deriving (Show, Eq)

getCompletionMode :: Context -> StaticLsM CompletionMode
getCompletionMode cx = do
  let path = cx.path
  haskell <- getHaskell path
  header <- Tree.getHeader haskell & isRightOrThrowT
  sourceRope <- getSourceRope path
  mod <- pathToModule path
  case (header, mod) of
    (Nothing, Just mod) -> pure $ HeaderMode mod
    (_, _) -> do
      let lineCol = cx.lineCol
      let line = Rope.toText $ Maybe.fromMaybe "" $ Rope.getLine sourceRope (Pos lineCol.line)
      let (beforeCol, afterCol) = T.splitAt lineCol.col line
      let (_, prefix) = Identity.runIdentity $ T.spanEndM (pure . not . Char.isSpace) beforeCol
      logInfo $ "prefix: " <> prefix
      let firstChar = fst <$> T.uncons prefix
      let firstIsUpper = case firstChar of
            Just c -> Char.isUpper c
            Nothing -> False
      let containsDot = T.unpack prefix & any (== '.')
      if firstIsUpper && containsDot
        then do
          let (mod, _) = T.breakOn "." prefix
          pure $ QualifiedMode mod
        else do
          pure UnqualifiedMode

getCompletion :: Context -> StaticLsM [Completion]
getCompletion cx = do
  logInfo $ "triggerKind: " <> T.pack (show cx.triggerKind)
  mode <- getCompletionMode cx
  logInfo $ "mode: " <> T.pack (show mode)
  case mode of
    HeaderMode mod -> do
      let label = "module " <> mod <> " where"
      pure [Completion {label, insertText = label <> "\n$0"}]
    UnqualifiedMode -> do
      fileCompletions <- getFileCompletions cx
      importCompletions <- getUnqualifiedImportCompletions cx
      pure $ importCompletions ++ fileCompletions
    QualifiedMode mod -> do
      let path = cx.path
      haskell <- getHaskell path
      let prog = Hir.parseHaskell haskell
      (_errs, prog) <- isRightOrThrowT prog
      let imports = prog.imports
      let importsWithAlias = filter (\imp -> fmap (.text) imp.alias == Just mod) imports
      logInfo $ "importsWithAlias: " <> T.pack (show importsWithAlias)
      let importMods = fmap (.mod.text) imports
      getCompletionsForImports importsWithAlias

data Completion = Completion
  { label :: !Text
  , insertText :: !Text
  }
  deriving (Show, Eq)

data TriggerKind = TriggerCharacter | TriggerUnknown
  deriving (Show, Eq)

data Context = Context
  { path :: AbsPath
  , pos :: !Pos
  , lineCol :: !LineCol
  , triggerKind :: !TriggerKind
  }
  deriving (Show, Eq)
