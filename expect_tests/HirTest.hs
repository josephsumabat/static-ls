{-# LANGUAGE QuasiQuotes #-}

module HirTest (tests) where

import AST.Haskell qualified as H
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import NeatInterpolation
import StaticLS.Hir qualified as Hir
import Test.Tasty
import Test.Tasty.Expect
import Text.Pretty.Simple qualified as Pretty

checkParse :: String -> T.Text -> Expect -> TestTree
checkParse name source ex = test name ex $ do
  let tree = H.parse source
  pure $ TL.toStrict $ Pretty.pShowNoColor tree

checkHir :: String -> T.Text -> Expect -> TestTree
checkHir name source ex = test name ex $ do
  let tree = H.parse source
  let (es, hir) = Hir.parseHaskell tree
  pure $ TL.toStrict $ Pretty.pShowNoColor hir

src1 =
  [trimming|
  module First where

  import First (First, C(type Ty, (:+:), .., first, (+++)))
  import Second (First, C(.., first, type Another, (+++))) as Another
  |]

src2 =
  [trimming|
  module Second
    (
      name1,
      name2,
    )
  where

  |]
tests =
  testGroup
    "HirTest"
    [ ( test "first" [expect|hello world|] do
          pure $ T.pack "hello world"
      ),
      (
        checkHir "exports" src2 [expect|Program
    { imports = []
    , exports =
        [ ExportItem
            { namespace = NameSpaceValue
            , name = Qualified
                { mod = Nothing
                , name = Name
                    { node = "variable@(22 - 27)"
                    , isOperator = False
                    , isConstructor = False
                    }
                }
            , children = []
            }
        , ExportItem
            { namespace = NameSpaceValue
            , name = Qualified
                { mod = Nothing
                , name = Name
                    { node = "variable@(33 - 38)"
                    , isOperator = False
                    , isConstructor = False
                    }
                }
            , children = []
            }
        ]
    , decls = []
    }|]
      )
    , ( checkHir
          "imports"
          src1
          [expect|Program
    { imports =
        [ Import
            { mod = ModuleText
                { parts = "First" :| []
                , text = "First"
                }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList =
                [ ImportItem
                    { namespace = NameSpaceValue
                    , name = Name
                        { node = "name@(34 - 39)"
                        , isOperator = False
                        , isConstructor = False
                        }
                    , children = []
                    }
                , ImportItem
                    { namespace = NameSpaceValue
                    , name = Name
                        { node = "name@(41 - 42)"
                        , isOperator = False
                        , isConstructor = False
                        }
                    , children =
                        [ ImportChild NameSpaceType
                            ( Name
                                { node = "name@(48 - 50)"
                                , isOperator = False
                                , isConstructor = False
                                }
                            )
                        , ImportChild NameSpaceValue
                            ( Name
                                { node = "constructor_operator@(53 - 56)"
                                , isOperator = True
                                , isConstructor = True
                                }
                            )
                        , ImportAllChildren
                        , ImportChild NameSpaceValue
                            ( Name
                                { node = "variable@(63 - 68)"
                                , isOperator = False
                                , isConstructor = False
                                }
                            )
                        , ImportChild NameSpaceValue
                            ( Name
                                { node = "operator@(71 - 74)"
                                , isOperator = True
                                , isConstructor = False
                                }
                            )
                        ]
                    }
                ]
            }
        , Import
            { mod = ModuleText
                { parts = "Second" :| []
                , text = "Second"
                }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList =
                [ ImportItem
                    { namespace = NameSpaceValue
                    , name = Name
                        { node = "name@(93 - 98)"
                        , isOperator = False
                        , isConstructor = False
                        }
                    , children = []
                    }
                , ImportItem
                    { namespace = NameSpaceValue
                    , name = Name
                        { node = "name@(100 - 101)"
                        , isOperator = False
                        , isConstructor = False
                        }
                    , children =
                        [ ImportAllChildren
                        , ImportChild NameSpaceValue
                            ( Name
                                { node = "variable@(106 - 111)"
                                , isOperator = False
                                , isConstructor = False
                                }
                            )
                        , ImportChild NameSpaceType
                            ( Name
                                { node = "name@(118 - 125)"
                                , isOperator = False
                                , isConstructor = False
                                }
                            )
                        , ImportChild NameSpaceValue
                            ( Name
                                { node = "operator@(128 - 131)"
                                , isOperator = True
                                , isConstructor = False
                                }
                            )
                        ]
                    }
                ]
            }
        ]
    , exports = []
    , decls = []
    }|]
      )
    ]
