{-# LANGUAGE BlockArguments #-}

module StaticLS.IDE.Completion (
  getCompletion,
  Completion (..),
)
where

import AST qualified
import AST.Haskell qualified as Haskell
import Control.Applicative
import Control.Monad
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.Logger (logInfo)
import StaticLS.StaticEnv
import StaticLS.StaticLsEnv
import StaticLS.Utils (isJustOrThrow, isRightOrThrowT)
import System.FilePath

makeRelativeMaybe :: FilePath -> FilePath -> Maybe FilePath
makeRelativeMaybe base path = do
  let rel = makeRelative base path
  guard $ path /= rel
  pure rel

uriToModule :: LSP.Uri -> StaticLsM (Maybe Text)
uriToModule uri = do
  let fp = LSP.uriToFilePath uri
  staticEnv <- getStaticEnv
  let srcDirs = staticEnv.srcDirs
  let wsRoot = staticEnv.wsRoot
  logInfo $ T.pack $ "fp: " <> show fp
  logInfo $ T.pack $ "srcDirs: " <> show srcDirs
  logInfo $ T.pack $ "wsRoot: " <> show wsRoot
  pure $ do
    fp <- fp
    modPath <- asum ((\srcDir -> makeRelativeMaybe srcDir fp) <$> srcDirs)
    let (modPathWithoutExt, ext) = splitExtension modPath
    guard $ ext == ".hs"
    let modText = T.replace (T.pack [pathSeparator]) "." (T.pack modPathWithoutExt)
    pure modText

getHeader :: Haskell.Haskell -> AST.Err (Maybe Haskell.Header)
getHeader haskell = do
  header <- AST.collapseErr haskell.children
  pure header

getCompletion :: LSP.Uri -> StaticLsM [Completion]
getCompletion uri = do
  haskell <- getHaskell uri
  header <- getHeader haskell & isRightOrThrowT
  mod <- uriToModule uri
  case (header, mod) of
    (Nothing, Just mod) -> do
      let label = "module " <> mod <> " where"
      pure [Completion {label, insertText = label <> "\n$0"}]
    (_, _) -> pure []

data Completion = Completion
  { label :: !Text
  , insertText :: !Text
  }
  deriving (Show, Eq)
