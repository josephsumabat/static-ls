module StaticLS.Arborist where

import AST qualified
import AST.Haskell qualified as H
import Arborist.Files
import Arborist.Haddock
import Arborist.ProgramIndex
import Arborist.Renamer
import Arborist.Scope.Types
import Control.Error
import Control.Monad qualified as Monad
import Control.Monad.IO.Class
import Data.HashMap.Lazy qualified as HashMap
import Data.HashMap.Lazy qualified as Map
import Data.LineCol (LineCol (..))
import Data.LineColRange
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Path qualified as Path
import Data.Set.NonEmpty qualified as NESet
import Data.Text
import Data.Text qualified as T
import Data.Time
import Debug.Trace
import Hir.Types qualified as Hir
import Language.LSP.Protocol.Types
import StaticLS.IDE.FileWith
import StaticLS.IDE.Monad
import StaticLS.ProtoLSP qualified as ProtoLSP
import System.Directory (doesFileExist)

time :: (MonadIO m) => [Char] -> m a -> m a
time label fn = do
  start <- liftIO getCurrentTime
  res <- fn
  end <- liftIO getCurrentTime
  traceShowM $ "Time to run " <> label <> ": " ++ show (diffUTCTime end start)
  pure res

getResolvedVar :: (MonadIO m, MonadIde m) => Hir.Program -> LineCol -> m (Maybe (H.Variable RenamePhase))
getResolvedVar target lc = fst <$> getResolvedVarAndPrgs target lc

getResolvedVarAndPrgs :: (MonadIO m, MonadIde m) => Hir.Program -> LineCol -> m (Maybe (H.Variable RenamePhase), ProgramIndex)
getResolvedVarAndPrgs target lc = do
  time "resolvedVar" $ do
    modFileMap <- getModFileMap
    prgIndex <- getPrgIndex
    (requiredPrograms) <- liftIO $ time "gather" $ gatherScopeDeps prgIndex target modFileMap (Just 2)
    tryWritePrgIndex (\_ -> requiredPrograms)
    let renameTree = renamePrg requiredPrograms HashMap.empty target
    let resolvedVar = (AST.getDeepestContainingLineCol @(H.Variable RenamePhase) (point lc)) . (.dynNode) =<< renameTree
    pure (resolvedVar, requiredPrograms)

getRequiredHaddock :: ProgramIndex -> GlblVarInfo -> Maybe HaddockInfo
getRequiredHaddock prgIndex varInfo =
  let mPrg = Map.lookup varInfo.originatingMod prgIndex
      haddockIndex = maybe Map.empty (indexPrgHaddocks Map.empty) mPrg
      qualName = glblVarInfoToQualified varInfo
   in Map.lookup qualName haddockIndex

-------------------
-- Definition
-------------------
modLocToFileLcRange :: (MonadIO m) => ModFileMap -> (Hir.ModuleText, LineColRange) -> m [FileLcRange]
modLocToFileLcRange modFileMap (modText, lcRange) = do
  let paths = maybe [] List.singleton (Map.lookup modText modFileMap)
  pathList <- fmap catMaybes $
    Monad.forM paths $ \path -> do
      fileExists <- liftIO $ doesFileExist path
      if fileExists
        then do
          absPath <- Path.filePathToAbs path
          pure $
            Just $
              FileWith
                { path = absPath
                , loc = lcRange
                }
        else pure Nothing
  pure pathList

varToFileLcRange :: (MonadIO m) => ModFileMap -> Hir.ModuleText -> (H.Variable RenamePhase) -> m [FileLcRange]
varToFileLcRange modFileMap thisMod varNode = do
  let locLst = maybe [] (resolvedLocs thisMod) varNode.ext
  fileLcRanges <- Monad.join <$> (mapM (modLocToFileLcRange modFileMap) locLst)
  pure fileLcRanges

-------------------
-- Hover
-------------------

varToHover :: ProgramIndex -> (H.Variable RenamePhase) -> Maybe Hover
varToHover prgIndex varNode =
  let mResolvedVar = varNode.ext
      range = ProtoLSP.lineColRangeToProto varNode.dynNode.nodeLineColRange
      mContents = (resolvedVarToContents prgIndex =<< mResolvedVar)
   in ( \contents ->
          Hover
            { _range = Just range
            , _contents = InL $ MarkupContent MarkupKind_Markdown contents
            }
      )
        <$> mContents

resolvedVarToContents :: ProgramIndex -> ResolvedVariable -> Maybe Text
resolvedVarToContents prgIndex resolvedVar =
  case resolvedVar of
    ResolvedVariable (ResolvedGlobal glblVarInfo) ->
      let mHover = getRequiredHaddock prgIndex glblVarInfo
       in Just $ renderGlblVarInfo mHover glblVarInfo
    _ -> Nothing

renderGlblVarInfo :: Maybe HaddockInfo -> GlblVarInfo -> Text
renderGlblVarInfo mHaddock glblVarInfo =
  wrapHaskell
    ( T.intercalate
        "\n"
        [ haddock
        , tySig
        ]
    )
    <> "  \n\nimported from: *"
    <> T.intercalate ", " (NE.toList $ (.mod.text) <$> NESet.toList glblVarInfo.importedFrom)
    <> "*"
    <> "  \noriginates from: *"
    <> glblVarInfo.originatingMod.text
    <> "*"
 where
  haddock =
    maybe "" (.text) mHaddock
  tySig =
    maybe "" (.node.dynNode.nodeText) glblVarInfo.sig
  wrapHaskell x = "\n```haskell\n" <> x <> "\n```\n"
