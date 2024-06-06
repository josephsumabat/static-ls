module Data.Path (
  Path (Path),
  KnownPathKind (sPathKind),
  AbsPath,
  RelPath,
  osPathToAbs,
  unsafeOsPathToAbs,
  filePathToAbs,
  unsafeFilePathToAbs,
  filePathToRel,
  toFilePath,
) where

import Control.Exception qualified as Exception
import Control.Monad ((<=<))
import Control.Monad.Catch
import Control.Monad.IO.Class
import GHC.Stack (HasCallStack)
import System.Directory.OsPath qualified as Dir
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

data PathKind = Rel | Abs

data SPathKind p where
  SRel :: SPathKind Rel
  SAbs :: SPathKind Abs

class KnownPathKind p where
  sPathKind :: SPathKind p

instance KnownPathKind Abs where
  sPathKind = SAbs

instance KnownPathKind Rel where
  sPathKind = SRel

newtype Path p = UncheckedPath {path :: OsPath}
  deriving (Show, Eq, Ord)

pattern Path :: OsPath -> Path p
pattern Path p <- UncheckedPath p

type AbsPath = Path Abs

type RelPath = Path Rel

toOsPath :: Path p -> OsPath
toOsPath = (.path)

toFilePath :: (HasCallStack) => Path p -> FilePath
toFilePath = either Exception.throw id . OsPath.decodeUtf . toOsPath

filePathToRel :: (HasCallStack) => FilePath -> RelPath
filePathToRel = UncheckedPath . either Exception.throw id . OsPath.encodeUtf

filePathToAbs :: (HasCallStack, MonadIO m, MonadThrow m) => FilePath -> m AbsPath
filePathToAbs = osPathToAbs <=< OsPath.encodeUtf

unsafeFilePathToAbs :: (HasCallStack) => FilePath -> AbsPath
unsafeFilePathToAbs = unsafeOsPathToAbs . either Exception.throw id . OsPath.encodeUtf

osPathToAbs :: (MonadIO m) => OsPath -> m AbsPath
osPathToAbs p = do
  absPath <- liftIO $ Dir.makeAbsolute p
  pure $ UncheckedPath absPath

unsafeOsPathToAbs :: (HasCallStack) => OsPath -> AbsPath
unsafeOsPathToAbs p =
  if OsPath.isAbsolute p
    then UncheckedPath p
    else error "unsafeOsPathToAbs: path is not absolute"
