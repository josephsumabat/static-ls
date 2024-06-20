{-# LANGUAGE BlockArguments #-}

module StaticLS.IDE.Completion (
  getCompletion,
  Completion (..),
)
where

import Control.Applicative
import Control.Monad
import Data.Function ((&))
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Text (Text)
import Data.Text qualified as T
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

getCompletion :: AbsPath -> StaticLsM [Completion]
getCompletion path = do
  haskell <- getHaskell path
  header <- Tree.getHeader haskell & isRightOrThrowT
  mod <- pathToModule path
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
