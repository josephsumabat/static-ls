{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.Diagnostics.ParseGHCSpec where

import Data.Path qualified as Path
import NeatInterpolation
import StaticLS.IDE.Diagnostics.ParseGHC
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  xdescribe "smoke" do
    let msg =
          [trimming|
            src/Mercury/Expenses/Sql.hs:(98,23)-(99,14): error: [GHC-83865] [-Wdeferred-type-errors, Werror=deferred-type-errors]
                • Couldn't match expected type: t0 -> b0
                              with actual type: From (SqlExpr (Entity ExpenseData))
                • The function ‘table’ is applied to one value argument,
                    but its type ‘From (SqlExpr (Entity ExpenseData))’ has none
                  In the first argument of ‘on’, namely ‘table @ExpenseData asfd’
                  In the second argument of ‘innerJoin’, namely
                    ‘table @ExpenseData asfd
                      `on`
                        (\ (expense :& expenseData)
                            -> expense.latestDataId ==. expenseData.id)’
              |
            98 |           `innerJoin` table @ExpenseData
              |                       ^^^^^^^^^^^^^^^^^^...
            src/Mercury/Expenses/Sql.hs:99:11-14: error: [GHC-88464] [-Wdeferred-out-of-scope-variables, Werror=deferred-out-of-scope-variables]
                Variable not in scope: asfd
              |
            99 |             `on` (\(expense :& expenseData) -> expense.latestDataId ==. expenseData.id)
              |           ^^^^
            |]

    it "split smoke" do
      (split msg) `shouldBe` []
      pure @IO ()

    -- it "smoke" do
    --   parse
    --     Path.uncheckedCoercePath
    --     `shouldBe` []

    it "smoke2" do
      parse
        Path.uncheckedCoercePath
        [trimming|
        All good (60 modules)
        src/StaticLS/IDE/Diagnostics/ParseGHC.hs:18:1: warning: [-Wunused-imports]
          The import of ‘Data.Int’ is redundant
              except perhaps to import instances from ‘Data.Int’
          To import instances alone, use: import Data.Int()
          |
        18 | import Data.Int
          | ^^^^^^^^^^^^^^^
        src/StaticLS/IDE/Diagnostics/ParseGHC.hs:19:1: warning: [-Wunused-imports]
          The import of ‘Data.Either’ is redundant
              except perhaps to import instances from ‘Data.Either’
          To import instances alone, use: import Data.Either()
          |
        19 | import Data.Either
          | ^^^^^^^^^^^^^^^^^^
        src/StaticLS/IDE/Diagnostics/ParseGHC.hs:146:49: warning: [-Wunused-matches]
          Defined but not used: ‘body’
          |
        146 | toDiagnostic toAbs ((range, severity, message), body) =
          |                                                 ^^^^
        |]
        `shouldBe` []

    pure ()
