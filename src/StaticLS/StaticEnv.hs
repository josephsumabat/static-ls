{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.StaticEnv (
  initStaticEnv,
  getStaticEnv,
  runHieDbExceptT,
  runHieDbMaybeT,
  StaticEnv (..),
  HieDbPath,
  HieFilePath,
  HiFilePath,
  HasStaticEnv,
  HasCallStack,
  runStaticEnv,
  loadTHSpliceLocs,
)
where

import Control.Exception (Exception, IOException, SomeException, catch)
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Database.SQLite.Simple (SQLError)
import HieDb qualified
import StaticLS.Logger
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import Data.LineColRange (LineColRange (..))
import Data.Map (Map)
import Data.Text (Text)
import System.Directory (getDirectoryContents)
import qualified System.Directory as Directory
import Data.Foldable (for_)
import System.FilePath ((</>))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Traversable (for)
import qualified Data.Map as Map
import Text.Parsec.Text (Parser)
import qualified Text.Parsec as Parsec
import Text.Parsec (ParseError)
import Data.LineCol (LineCol(..))
import Control.Monad (void)
import qualified Data.Either as Either
import Text.Read (readMaybe)
import Data.Pos (Pos(..))
import GHC (load)

type HieDbPath = FilePath

type HieFilePath = FilePath

type HiFilePath = FilePath

data HieDbException
  = HieDbIOException IOException
  | HieDbSqlException SQLError
  | HieDbNoHieDbSourceException
  | HieDbOtherException SomeException
  deriving (Show)

instance Exception HieDbException

type ThPath = FilePath

-- Helps maps from the source containing template haskell to the dump
-- TODO temporary data structure
type THSpliceLoc = Map (AbsPath, LineColRange) THInfo

data THInfo = THInfo
  { -- | The original template haskell source code
    --  e.g. deriveJSON defaultOptions ''D
    declaration :: Text
    -- | Path to the original template haskell source
  , origPath :: FilePath
    -- | Location of the dump
  , spliceDumpPath :: ThPath
    -- | Range of original template haskell source
  , lineColRange :: LineColRange
  -- , generatedCode :: Text -- TODO I don't think we actually need this information
  } deriving (Show, Eq)

parseTHInfos :: FilePath -> Text -> Either ParseError [THInfo]
parseTHInfos spliceDumpPath content = Parsec.parse (do
  -- break if there's a path in the first column followed by :
  parseTHInfo spliceDumpPath `Parsec.sepBy` (Parsec.try $ do
    _ <- Parsec.many (Parsec.noneOf "\n")
    _ <- Parsec.char ':'
    _ <- Parsec.many (Parsec.noneOf "\n")
    _ <- Parsec.newline
    pure ())) spliceDumpPath content


parseTHInfo :: FilePath -> Parser THInfo
parseTHInfo spliceDumpPath = do
  -- parse until :
  Parsec.skipMany (Parsec.noneOf ":")
  origPath <- Parsec.many (Parsec.noneOf ":")
  _ <- Parsec.char ':'
  lineColRange <- parseLineColRange
  _ <- Parsec.string ": Splicing declarations\n"
  declaration <- parseDeclaration
  -- now just ignore all text until next path
  pure $ THInfo {
    declaration = declaration
  , origPath = origPath
  , lineColRange = lineColRange
  , spliceDumpPath = spliceDumpPath
  }


parseDeclaration :: Parser Text
parseDeclaration = Text.pack <$> Parsec.manyTill Parsec.anyChar (Parsec.try $ do
    _ <- Parsec.newline
    _ <- Parsec.string "=======>"
    _ <- Parsec.newline
    pure ())

-- Possible formats
-- 27:2-32
-- (27,2)-(30,12)
-- etc?
parseLineColRange :: Parser LineColRange
parseLineColRange = parseSingleRow -- TODO <|> parseMultiRow
  where
    parseSingleRow = do
      row <- parseDigit
      void $ Parsec.char ':'
      columnStart <- parseDigit
      void $ Parsec.char '-'
      columnEnd <- parseDigit
      let lineColStart = LineCol (Pos  row) (Pos columnStart)
          lineColEnd = LineCol (Pos  row) (Pos columnEnd)
      pure $ LineColRange lineColStart lineColEnd

parseDigit :: Parser Int
parseDigit = do
  raw <- Parsec.many Parsec.digit
  case readMaybe @Int raw of
    Nothing -> fail $ "Expected a digit but got: " <> raw
    Just n -> pure n

-- TODO
mkThSpliceLoc :: [THInfo] -> THSpliceLoc
mkThSpliceLoc = undefined

data BuildSystem = Cabal | Stack | Buck2 deriving (Show, Eq)

loadTHSpliceLocs :: IO THSpliceLoc
loadTHSpliceLocs = do
    -- Examples 
    -- /Users/mac/dev/haskell-playground/dist-newstyle/build/aarch64-osx/ghc-9.8.4/haskell-playground-0.1.0.0/build/src/StaticLs.dump-splices
    buildSystem <- getBuildSystem
    project <- getProjectName
    -- let baseDir = case buildSystem of
    --       Cabal -> "dist-newstyle/build/aarch64-osx/ghc-*/" <> project <> "-*/build/src/"
    --       Stack -> undefined --TODO
    --       Buck2 -> undefined --TODO
    let baseDir = "/Users/mac/dev/haskell-playground/dist-newstyle/build/aarch64-osx/ghc-9.8.4/haskell-playground-0.1.0.0/build/src/StaticLs.dump-splices"
    spliceDumpFiles <- Directory.getDirectoryContents $ Text.unpack baseDir
    thInfoEithers <- for spliceDumpFiles $ \file -> do
      text <- TextIO.readFile file
      pure $ parseTHInfos file text
    let thInfos = (Either.fromRight []) =<< thInfoEithers
    pure $ mkThSpliceLoc thInfos
  where
    -- TODO just hard coding these for stubs
    getBuildSystem :: IO BuildSystem
    getBuildSystem = pure Cabal
    -- TODO
    getProjectName :: IO Text
    getProjectName = pure "haskell-playground"


-- | Imuttable references to "static sources" of language information. Should
--     be low overhead and should be sources for language information only.
--
-- Functions that make use of this should ensure that they are robust against
-- exceptions i.e. that the language server does not crash if something goes
-- wrong with fetching information from a static source
data StaticEnv = StaticEnv
  { hieDbPath :: AbsPath
  -- ^ Path to the hiedb file
  , hieDirs :: [AbsPath]
  , hiFilesPath :: AbsPath
  , wsRoot :: AbsPath
  , modelsFilesDir :: AbsPath
  -- ^ workspace root
  , mutableSrcDirs :: [AbsPath]
  -- ^ directories to search for source code in order of priority (mutable directories)
  , immutableSrcDirs :: [AbsPath]
  -- ^ directories to search for source code in order of priority (immutable directories)
  , allSrcDirs :: [AbsPath]
  -- ^ directories to search for source code in order of priority (muttable and imuttable directories)
  , thSpliceLoc :: THSpliceLoc
  -- ^ Map from template haskell source to generated code dump
  -- TODO eventually we might want to load this on demand or something but this might be sufficent for a POC
  , fourmoluCommand :: Maybe FilePath
  -- ^ path to fourmolu binary
  }

class (Monad m) => HasStaticEnv m where
  getStaticEnv :: m StaticEnv

instance (Monad m) => HasStaticEnv (ReaderT StaticEnv m) where
  getStaticEnv = ask

instance (HasStaticEnv m) => HasStaticEnv (MaybeT m) where
  getStaticEnv = lift getStaticEnv

instance (HasStaticEnv m) => HasStaticEnv (ExceptT e m) where
  getStaticEnv = lift getStaticEnv

runStaticEnv :: StaticEnv -> ReaderT StaticEnv IO a -> IO a
runStaticEnv = flip runReaderT

initStaticEnv :: AbsPath -> StaticEnvOptions -> IO StaticEnv
initStaticEnv wsRoot staticEnvOptions = do
  let databasePath = wsRoot Path.</> (Path.filePathToRel staticEnvOptions.optionHieDbPath)
      hieDirs = fmap ((wsRoot Path.</>) . Path.filePathToRel) (staticEnvOptions.optionHieDirs)
      mutableSrcDirs = fmap ((wsRoot Path.</>) . Path.filePathToRel) (staticEnvOptions.optionSrcDirs)
      immutableSrcDirs = fmap ((wsRoot Path.</>) . Path.filePathToRel) (staticEnvOptions.optionImmutableSrcDirs)
      allSrcDirs = mutableSrcDirs <> immutableSrcDirs
      hiFilesPath = wsRoot Path.</> (Path.filePathToRel staticEnvOptions.optionHiFilesPath)
  thSpliceLoc <- loadTHSpliceLocs `catch` (\(e :: SomeException) -> pure Map.empty) -- TODO log the exception
  let serverStaticEnv =
        StaticEnv
          { hieDbPath = databasePath
          , hieDirs = hieDirs
          , hiFilesPath = hiFilesPath
          , wsRoot = wsRoot
          , modelsFilesDir = wsRoot Path.</> "config" Path.</> "modelsFiles"
          , mutableSrcDirs = mutableSrcDirs
          , immutableSrcDirs = immutableSrcDirs
          , allSrcDirs = allSrcDirs
          , thSpliceLoc = thSpliceLoc
          , fourmoluCommand = staticEnvOptions.fourmoluCommand
          }
  pure serverStaticEnv

-- | Run an hiedb action in an exceptT
runHieDbExceptT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> ExceptT HieDbException m a
runHieDbExceptT hieDbFn =
  getStaticEnv
    >>= \staticEnv ->
      ( \hiedbPath ->
          ExceptT . liftIO $
            HieDb.withHieDb (Path.toFilePath hiedbPath) (fmap Right . hieDbFn)
              `catch` (pure . Left . HieDbIOException)
              `catch` (pure . Left . HieDbSqlException)
              `catch` (\(e :: SomeException) -> pure . Left $ HieDbOtherException e)
      )
        staticEnv.hieDbPath

-- | Run an hiedb action with the MaybeT Monad
runHieDbMaybeT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> MaybeT m a
runHieDbMaybeT = exceptToMaybeT . runHieDbExceptT
