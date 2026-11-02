{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.CodeActions.BringVariableInScopeSpec (spec) where

import Data.Edit qualified as Edit
import Data.Function ((&))
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Path qualified as Path
import Data.Pos (Pos (..))
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Language.LSP.Protocol.Types qualified as LSP
import NeatInterpolation (trimming)
import StaticLS.IDE.CodeActions.BringVariableInScope qualified as BringVariableInScope
import StaticLS.IDE.CodeActions.Parse (VariableNotInScopeFixes (..))
import StaticLS.IDE.Monad (getSourceRope)
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.Utils (isJustOrThrowS)
import Test.Hspec
import TestImport.Compilation qualified

spec :: Spec
spec = do
  describe "fixing a misspelled variable" do
    check
      "replaces the name at the diagnostic"
      (MissSpelled "summ" "sum")
      [trimming|
      module Test where

      total :: Int
      total = @0summ [1, 2, 3]
      |]
      "Change spelling to sum"
      [trimming|
      module Test where

      total :: Int
      total = sum [1, 2, 3]
      |]

    check
      "only replaces the occurrence the diagnostic points at"
      (MissSpelled "summ" "sum")
      [trimming|
      module Test where

      summ :: Int
      summ = 1

      total :: Int
      total = summ + @0summ [1, 2, 3]
      |]
      "Change spelling to sum"
      [trimming|
      module Test where

      summ :: Int
      summ = 1

      total :: Int
      total = summ + sum [1, 2, 3]
      |]

    check
      "can replace with a longer name"
      (MissSpelled "lenght" "length")
      [trimming|
      module Test where

      size :: Int
      size = @0lenght "abc" + 1
      |]
      "Change spelling to length"
      [trimming|
      module Test where

      size :: Int
      size = length "abc" + 1
      |]

  describe "adding to an import list" do
    check
      "appends to a non-empty import list"
      (AddToImportList "asum" "Data.Foldable")
      [trimming|
      module Test where

      import Data.Foldable (for_)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]
      "Add asum to the import list of Data.Foldable"
      [trimming|
      module Test where

      import Data.Foldable (for_, asum)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]

    check
      "fills an empty import list"
      (AddToImportList "asum" "Data.Foldable")
      [trimming|
      module Test where

      import Data.Foldable ()

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]
      "Add asum to the import list of Data.Foldable"
      [trimming|
      module Test where

      import Data.Foldable (asum)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]

    check
      "wraps operators in parentheses"
      (AddToImportList "<|>" "Control.Applicative")
      [trimming|
      module Test where

      import Control.Applicative (Alternative)

      x :: Maybe Int
      x = Nothing <|> Just 1
      |]
      "Add <|> to the import list of Control.Applicative"
      [trimming|
      module Test where

      import Control.Applicative (Alternative, (<|>))

      x :: Maybe Int
      x = Nothing <|> Just 1
      |]

    check
      "picks the import of the requested module"
      (AddToImportList "asum" "Data.Foldable")
      [trimming|
      module Test where

      import Control.Applicative (Alternative)
      import Data.Foldable (for_)
      import Data.Maybe (fromMaybe)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]
      "Add asum to the import list of Data.Foldable"
      [trimming|
      module Test where

      import Control.Applicative (Alternative)
      import Data.Foldable (for_, asum)
      import Data.Maybe (fromMaybe)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]

    check
      "skips hiding imports of the module"
      (AddToImportList "asum" "Data.Foldable")
      [trimming|
      module Test where

      import Data.Foldable hiding (for_)
      import Data.Foldable (traverse_)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]
      "Add asum to the import list of Data.Foldable"
      [trimming|
      module Test where

      import Data.Foldable hiding (for_)
      import Data.Foldable (traverse_, asum)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]

    check
      "appends to a multiline import list"
      (AddToImportList "asum" "Data.Foldable")
      [trimming|
      module Test where

      import Data.Foldable
        ( for_
        , traverse_
        )

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]
      "Add asum to the import list of Data.Foldable"
      [trimming|
      module Test where

      import Data.Foldable
        ( for_
        , traverse_
        , asum)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]

    checkNot
      "does nothing when the module is not imported"
      (AddToImportList "asum" "Data.Foldable")
      [trimming|
      module Test where

      import Data.Maybe (fromMaybe)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]

    checkNot
      "does nothing when the import has no import list"
      (AddToImportList "asum" "Data.Foldable")
      [trimming|
      module Test where

      import Data.Foldable qualified as Foldable

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]

    checkNot
      "does nothing when the module is only imported hiding"
      (AddToImportList "asum" "Data.Foldable")
      [trimming|
      module Test where

      import Data.Foldable hiding (for_)

      x :: Maybe Int
      x = asum [Nothing, Just 1]
      |]
 where
  check name fix src expectedTitle expected =
    it name do
      (rope, action) <- runCodeAction fix src
      case action of
        Nothing -> expectationFailure "expected a code action"
        Just quickFix -> do
          quickFix._title `shouldBe` expectedTitle
          quickFix._kind `shouldBe` Just LSP.CodeActionKind_QuickFix
          applyCodeAction rope quickFix `shouldBe` Just expected

  checkNot name fix src =
    it name do
      (_, action) <- runCodeAction fix src
      action `shouldBe` Nothing

-- | Runs the code action on the source. The diagnostic is a zero-width point
-- at placeholder 0, or at the start of the file if there is no placeholder.
runCodeAction :: VariableNotInScopeFixes -> Text -> IO (Rope, Maybe LSP.CodeAction)
runCodeAction fix src = do
  let testFp = "CodeActionTest.hs"
  TestImport.Compilation.setupWithoutCompilation [(testFp, src)] \dir sources -> do
    let path = dir Path.</> testFp
    (_, placeholders) <- Map.lookup path sources & isJustOrThrowS "source not found"
    rope <- getSourceRope path
    let pos = fromMaybe (Pos 0) (IntMap.lookup 0 placeholders)
        lspPos = ProtoLSP.posToLSPPosition rope pos
        tdi = LSP.TextDocumentIdentifier (ProtoLSP.absPathToUri path)
    action <- BringVariableInScope.codeAction path tdi (mkDiagnostic (LSP.Range lspPos lspPos)) fix
    pure (rope, action)

mkDiagnostic :: LSP.Range -> LSP.Diagnostic
mkDiagnostic range =
  LSP.Diagnostic
    { _range = range
    , _severity = Just LSP.DiagnosticSeverity_Error
    , _code = Nothing
    , _codeDescription = Nothing
    , _source = Just "haskell"
    , _message = "Variable not in scope"
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }

-- | Applies the single text edit of a quick fix to the source.
applyCodeAction :: Rope -> LSP.CodeAction -> Maybe Text
applyCodeAction rope action = do
  LSP.WorkspaceEdit _ (Just [LSP.InL (LSP.TextDocumentEdit _ [LSP.InL (LSP.TextEdit range newText)])]) _ <- action._edit
  let range' = Rope.lineColRangeToRange rope (ProtoLSP.lineColRangeFromProto range)
  pure $ Rope.toText $ Rope.edit (Edit.replace range' newText) rope
