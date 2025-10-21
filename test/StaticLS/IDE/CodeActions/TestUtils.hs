module StaticLS.IDE.CodeActions.TestUtils where

import AST.Haskell qualified as AST
import Arborist.ProgramIndex
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Function ((&))
import Data.HashMap.Internal qualified as HM
import Data.HashMap.Strict qualified as HashMap
import Data.Path (AbsPath)
import Data.Pos (Pos)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text.IO qualified as T
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import StaticLS.IDE.CodeActions qualified as CodeActions
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Monad
import StaticLS.IDE.SourceEdit (SourceEdit (..))
import StaticLS.Monad
import StaticLS.Utils (isJustOrThrowS)
import Test.Hspec

checkCodeAction ::
  (HasCallStack) =>
  AbsPath ->
  Pos ->
  (CodeActionContext -> StaticLsM [Assist]) ->
  -- set the below to Nothing to make sure there are no assists
  Maybe (Text, [Assist] -> Maybe Assist) ->
  StaticLsM ()
checkCodeAction path pos codeAction findAssist = do
  rope <- getSourceRope path
  let lineCol = Rope.posToLineCol rope pos
  let cx = CodeActionContext {path, pos, lineCol}
  sourceEdit <- do
    assists <- codeAction cx
    case findAssist of
      Nothing -> do
        liftIO $ assists `shouldBe` []
        pure Nothing
      Just (expected, findAssist) -> do
        assist <- findAssist assists & isJustOrThrowS ("couldn't find assist ")
        case assist.sourceEdit of
          Left sourceEdit -> do
            pure $ Just (expected, sourceEdit)
          Right msg -> do
            sourceEdit <- CodeActions.resolveLazyAssist msg
            pure $ Just (expected, sourceEdit)
  case sourceEdit of
    Nothing -> pure ()
    Just (expected, sourceEdit) -> do
      edit <- sourceEdit.fileEdits & HashMap.lookup path & isJustOrThrowS "couldn't find edit for path"
      let rope' = Rope.edit edit rope
      liftIO $ Rope.toText rope' `shouldBe` expected
      pure ()
  pure ()

getPrg :: [FilePath] -> IO [Hir.Program Hir.HirRead]
getPrg hsFiles =
  forM hsFiles $ \file -> do
    fileContents <- T.readFile file
    let v = Hir.parseHaskell (AST.parse fileContents)
    pure $ snd v

updatePrgIndex :: [FilePath] -> ReaderT Env IO ()
updatePrgIndex file = do
  programs <- liftIO $ getPrg file
  let prog = head programs
  modFileMap <- getModFileMap

  -- get sccope
  scopeDeps <- liftIO $ gatherScopeDeps HM.empty prog modFileMap Nothing

  -- write new prg index
  tryWritePrgIndex (const scopeDeps)
