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
import Data.HashMap.Lazy qualified as Map
import Data.HashMap.Lazy qualified as HashMap
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
import Hir.Types 
import Language.LSP.Protocol.Types
import StaticLS.IDE.FileWith
import StaticLS.IDE.Monad
import StaticLS.ProtoLSP qualified as ProtoLSP
import System.Directory (doesFileExist)
import AST.Sum (pattern Inj)

type ResolveableRename = Resolveable RenamePhase


time :: (MonadIO m) => [Char] -> m a -> m a
time label fn = do
  start <- liftIO getCurrentTime
  res <- fn
  end <- liftIO getCurrentTime
  traceShowM $ "Time to run " <> label <> ": " ++ show (diffUTCTime end start)
  pure res

getResolved :: (MonadIO m, MonadIde m) => Hir.Program -> LineCol -> m (Maybe (ResolveableRename))
getResolved target lc = fst <$> getResolvedTermAndPrgs target lc

getResolvedTermAndPrgs :: (MonadIO m, MonadIde m) => Hir.Program -> LineCol -> m (Maybe (ResolveableRename), ProgramIndex)
getResolvedTermAndPrgs target lc = do
  time "resolved" $ do
    modFileMap <- getModFileMap
    prgIndex <- getPrgIndex
    requiredPrograms <- liftIO $ time "gather" $ gatherScopeDeps prgIndex target modFileMap (Just 2)
    tryWritePrgIndex (\_ -> requiredPrograms)
    let renameTree = renamePrg requiredPrograms HashMap.empty target
        mResolved = (AST.getDeepestContainingLineCol @ResolveableRename (point lc)) . (.dynNode) =<< renameTree
    pure (mResolved, requiredPrograms)

getRequiredHaddockVar :: ProgramIndex -> GlblVarInfo -> Maybe HaddockInfo
getRequiredHaddockVar prgIndex varInfo =
  let mPrg = Map.lookup varInfo.originatingMod prgIndex
      haddockIndex = maybe Map.empty (indexPrgHaddocks Map.empty) mPrg
      qualName = glblVarInfoToQualified varInfo
   in Map.lookup qualName haddockIndex

getRequiredHaddockName :: ProgramIndex -> GlblNameInfo -> Maybe HaddockInfo
getRequiredHaddockName prgIndex nameInfo =
  let mPrg = Map.lookup nameInfo.originatingMod prgIndex
      haddockIndex = maybe Map.empty (indexPrgHaddocks Map.empty) mPrg
      qualName = glblNameInfoToQualified nameInfo
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

resolvedToFileLcRange :: (MonadIO m) => ModFileMap -> Hir.ModuleText -> ResolveableRename -> m [FileLcRange]
resolvedToFileLcRange modFileMap thisMod resolved = do
  let locLst = case resolved of
        Inj @(H.Variable RenamePhase) var -> maybe [] (resolvedLocs thisMod) var.ext
        Inj @(H.Name RenamePhase) name -> maybe [] resolvedNameLocs name.ext
        Inj @(H.Constructor RenamePhase) constructor -> maybe [] resolvedConstructorLocs constructor.ext
        _ -> []
  fileLcRanges <- Monad.join <$> mapM (modLocToFileLcRange modFileMap) locLst
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
      let mHover = getRequiredHaddockVar prgIndex glblVarInfo
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

nameToHover :: ProgramIndex -> (H.Name RenamePhase) -> Maybe Hover
nameToHover prgIndex nameNode =
 let mResolvedName = nameNode.ext
     range = ProtoLSP.lineColRangeToProto nameNode.dynNode.nodeLineColRange
     mContents = (resolvedNameToContents prgIndex =<< mResolvedName)
  in ( \contents ->
         Hover
           { _range = Just range
           , _contents = InL $ MarkupContent MarkupKind_Markdown contents
           }
     )
       <$> mContents

resolvedNameToContents :: ProgramIndex -> ResolvedName -> Maybe Text
resolvedNameToContents prgIndex resolvedName =
 case resolvedName of
   ResolvedName nameInfo _ ->
     let mHover = getRequiredHaddockName prgIndex nameInfo
      in Just $ renderNameInfo mHover nameInfo
   _ -> Nothing


renderNameInfo :: Maybe HaddockInfo -> GlblNameInfo -> Text
renderNameInfo mHaddock nameInfo =
  wrapHaskell
    ( T.intercalate
        "\n"
        [ haddock,
          declText
        ]
    )
    <> "  \n\nimporated from: *"
    <> T.intercalate ", " (NE.toList $ (.mod.text) <$> NESet.toList nameInfo.importedFrom)
    <> "*"
    <> "  \noriginates from: *"
    <> nameInfo.originatingMod.text
    <> "*"
 where
  haddock =
    maybe "" (.text) mHaddock
  declText = case nameInfo.decl of
    DeclData decl -> decl.node.dynNode.nodeText
    DeclNewtype decl -> decl.node.dynNode.nodeText
    DeclClass decl -> decl.node.dynNode.nodeText
    DeclTypeSynonym decl -> decl.node.dynNode.nodeText
    _ -> ""
  wrapHaskell x = "\n```haskell\n" <> x <> "\n```\n"


