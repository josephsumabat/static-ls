{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.Diagnostics.ParseGHCSpec where

import Data.Path qualified as Path
import NeatInterpolation
import StaticLS.IDE.Diagnostics.ParseGHC
import Test.Hspec

spec :: Spec
spec = do
  xdescribe "smoke" do
    let msg =
          [trimming|
            src/MyFile/Test/Sql.hs:(98,23)-(99,14): error: [GHC-83865] [-Wdeferred-type-errors, Werror=deferred-type-errors]
                • Couldn't match expected type: t0 -> b0
                              with actual type: From (SqlExpr (Entity MyData))
                • The function ‘table’ is applied to one value argument,
                    but its type ‘From (SqlExpr (Entity MyData))’ has none
                  In the first argument of ‘on’, namely ‘table @MyData asfd’
                  In the second argument of ‘innerJoin’, namely
                    ‘table @MyData asfd
                      `on`
                        (\ (val :& myData)
                            -> val.latestDataId ==. myData.id)’
              |
           98 |           `innerJoin` table @MyData
              |                       ^^^^^^^^^^^^^^^^^^...
            src/MyFile/Test/Sql.hs:99:11-14: error: [GHC-88464] [-Wdeferred-out-of-scope-variables, Werror=deferred-out-of-scope-variables]
                Variable not in scope: asfd
              |
           99 |             `on` (\(val :& myData) -> val.latestDataId ==. myData.id)
              |           ^^^^
            |]

    it "split smoke" do
      (split msg) `shouldBe` []
      pure @IO ()

    -- it "smoke" do
    it "weird case" do
      parse
        Path.uncheckedCoercePath
        msg
        `shouldBe` []

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

    it "smoke2" do
      parseErrorInfo "[GHC-1234]" `shouldBe` (ErrorInfo [] (Just "GHC-1234"))

      parse
        Path.uncheckedCoercePath
        [trimming|
        src/StaticLS/PositionDiff.hs:19:3: warning: [GHC-47854] [-Wduplicate-exports]
            ‘getDiffMapFromDiff’ is exported by ‘getDiffMapFromDiff’ and ‘getDiffMapFromDiff’
           |
        19 |   getDiffMapFromDiff,
           |   ^^^^^^^^^^^^^^^^^^
        |]
        `shouldBe` []

    pure ()
