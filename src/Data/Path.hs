module Data.Path (
  Path (path, Path),
  KnownPathKind (sPathKind),
  AbsPath,
  RelPath,
  filePathToAbs,
  unsafeFilePathToAbs,
  filePathToRel,
  toFilePath,
  (</>),
  makeRelative,
  absToRel,
  (<.>),
  (-<.>),
  filePathToAbsThrow,
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import GHC.Stack (HasCallStack)
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath

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

newtype Path p = UncheckedPath {path :: FilePath}
  deriving (Show, Eq, Ord)

pattern Path :: FilePath -> Path p
pattern Path p <- UncheckedPath p

type AbsPath = Path Abs

type RelPath = Path Rel

toFilePath :: (HasCallStack) => Path p -> FilePath
toFilePath = (.path)

filePathToRel :: (HasCallStack) => FilePath -> RelPath
filePathToRel = UncheckedPath

filePathToAbs :: (HasCallStack, MonadIO m, MonadThrow m) => FilePath -> m AbsPath
filePathToAbs p = do
  absPath <- liftIO $ Dir.makeAbsolute p
  pure $ UncheckedPath absPath

unsafeFilePathToAbs :: (HasCallStack) => FilePath -> AbsPath
unsafeFilePathToAbs p =
  if FilePath.isAbsolute p
    then UncheckedPath p
    else error "unsafeOsPathToAbs: path is not absolute"

filePathToAbsThrow :: (MonadThrow m, HasCallStack) => FilePath -> m AbsPath
filePathToAbsThrow p = pure $ UncheckedPath p

(</>) :: Path p -> Path Rel -> Path p
(UncheckedPath p) </> (UncheckedPath p') = UncheckedPath (p FilePath.</> p')

infixr 5 </>

(<.>) :: Path p -> String -> Path p
(UncheckedPath p) <.> ext = UncheckedPath (p FilePath.<.> ext)

(-<.>) :: Path p -> String -> Path p
(UncheckedPath p) -<.> ext = UncheckedPath (p FilePath.-<.> ext)

absToRel :: AbsPath -> RelPath
absToRel (UncheckedPath p) = UncheckedPath p

makeRelative :: Path p -> Path q -> Path Rel
makeRelative (UncheckedPath p) (UncheckedPath q) = UncheckedPath (FilePath.makeRelative p q)
