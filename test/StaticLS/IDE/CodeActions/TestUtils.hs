module StaticLS.IDE.CodeActions.TestUtils where

import Control.Monad.IO.Class
import Data.Function ((&))
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap qualified as IntMap
import Data.Path qualified as Path
import Data.Rope qualified as Rope
import Data.Text (Text)
import StaticLS.IDE.CodeActions qualified as CodeActions
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.SourceEdit (SourceEdit (..))
import StaticLS.Server qualified as Server
import StaticLS.StaticLsEnv
import StaticLS.Utils (isJustOrThrowS, isRightOrThrow)
import Test.Hspec
import TestImport.Placeholder qualified as Placeholder

checkCodeAction ::
  (HasCallStack) =>
  Text ->
  (CodeActionContext -> StaticLsM [Assist]) ->
  (forall a. StaticLsM a -> IO a) ->
  -- set the below to Nothing to make sure there are no assists
  Maybe (Text, [Assist] -> Maybe Assist) ->
  IO ()
checkCodeAction before codeAction run findAssist = do
  (source, places) <- Placeholder.parsePlaceholders before & isRightOrThrow
  let rope = Rope.fromText source
  pos <- places IntMap.!? 0 & isJustOrThrowS "couldn't find placeholder 0"
  let lineCol = Rope.posToLineCol rope pos
  path <- Path.filePathToAbs "CodeActionTest.hs"
  let cx = CodeActionContext {path, pos, lineCol}
  sourceEdit <- run do
    Server.updateFileState path rope
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
      Rope.toText rope' `shouldBe` expected
      pure ()
  pure ()
