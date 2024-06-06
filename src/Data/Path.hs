module Data.Path (
  Path (path, Path),
  OsString,
  KnownPathKind (sPathKind),
  AbsPath,
  RelPath,
  osPathToAbs,
  unsafeOsPathToAbs,
  filePathToAbs,
  unsafeFilePathToAbs,
  filePathToRel,
  toFilePath,
  (</>),
  makeRelative,
  absToRel,
  (<.>),
  stringToOs,
  (-<.>),
  filePathToAbsThrow,
) where

import Control.Exception qualified as Exception
import Control.Monad ((<=<))
import Control.Monad.Catch
import Control.Monad.IO.Class
import GHC.Stack (HasCallStack)
import System.Directory.OsPath qualified as Dir
import System.OsPath (OsPath, OsString)
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

filePathToAbsThrow :: (MonadThrow m, HasCallStack) => FilePath -> m AbsPath
filePathToAbsThrow p = UncheckedPath <$> OsPath.encodeUtf p

osPathToAbs :: (MonadIO m) => OsPath -> m AbsPath
osPathToAbs p = do
  absPath <- liftIO $ Dir.makeAbsolute p
  pure $ UncheckedPath absPath

unsafeOsPathToAbs :: (HasCallStack) => OsPath -> AbsPath
unsafeOsPathToAbs p =
  if OsPath.isAbsolute p
    then UncheckedPath p
    else error "unsafeOsPathToAbs: path is not absolute"

-- TODO: use unsafeEncodeUtf when on the right version
stringToOs :: (HasCallStack) => String -> OsString
stringToOs = either Exception.throw id . OsPath.encodeUtf

(</>) :: Path p -> Path Rel -> Path p
(UncheckedPath p) </> (UncheckedPath p') = UncheckedPath (p OsPath.</> p')

infixr 5 </>

(<.>) :: Path p -> OsString -> Path p
(UncheckedPath p) <.> ext = UncheckedPath (p OsPath.<.> ext)

(-<.>) :: Path p -> OsString -> Path p
(UncheckedPath p) -<.> ext = UncheckedPath (p OsPath.-<.> ext)

absToRel :: AbsPath -> RelPath
absToRel (UncheckedPath p) = UncheckedPath p

makeRelative :: Path p -> Path q -> Path Rel
makeRelative (UncheckedPath p) (UncheckedPath q) = UncheckedPath (OsPath.makeRelative p q)
