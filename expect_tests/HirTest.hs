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
      pattern First,
      type (++),
    )
  where

  |]

src3 =
  [trimming|
  module Third where
  
  data First = First
  
  pattern First, Second, Third :: Int -> First
  
  class FirstClass a b c | a b -> c, c -> a b where
    abc :: a -> b -> c
  
  data () = ()
  
  pattern APattern x = Just x
  
  data [] a = Nil
  
  data (:+:) a b = a :+: b
  
  type Result a b = Either a b
  
  newtype Thingy = Thingy Int
  
  type family Testing where
  
  data family MyDataFamily
  
  class MyClass2 a where
    first :: a -> a
  |]

tests =
  testGroup
    "HirTest"
    [ ( test "first" [expect|hello world|] do
          pure $ T.pack "hello world"
      )
    , ( checkHir
          "decls"
          src3
          [expect|Program
    { imports = []
    , exports = []
    , decls =
        [ DeclData
            ( DataDecl
                { name = NameShow
                    { name = "First"
                    , node = "name@(25 - 30)"
                    }
                , node = "data_type@(20 - 38)"
                }
            )
        , DeclPatternSig
            ( PatternSigDecl
                { name = NameShow
                    { name = "First"
                    , node = "constructor@(48 - 53)"
                    }
                }
            )
        , DeclPatternSig
            ( PatternSigDecl
                { name = NameShow
                    { name = "Second"
                    , node = "constructor@(55 - 61)"
                    }
                }
            )
        , DeclPatternSig
            ( PatternSigDecl
                { name = NameShow
                    { name = "Third"
                    , node = "constructor@(63 - 68)"
                    }
                }
            )
        , DeclClass
            ( ClassDecl
                { name = NameShow
                    { name = "FirstClass"
                    , node = "name@(92 - 102)"
                    }
                , node = "class@(86 - 156)"
                }
            )
        , DeclData
            ( DataDecl
                { name = NameShow
                    { name = "()"
                    , node = "unit@(163 - 165)"
                    }
                , node = "data_type@(158 - 170)"
                }
            )
        , DeclPattern
            ( PatternDecl
                { name = NameShow
                    { name = "APattern"
                    , node = "constructor@(180 - 188)"
                    }
                , node = "equation@(180 - 199)"
                }
            )
        , DeclData
            ( DataDecl
                { name = NameShow
                    { name = "[]"
                    , node = "prefix_list@(206 - 208)"
                    }
                , node = "data_type@(201 - 216)"
                }
            )
        , DeclData
            ( DataDecl
                { name = NameShow
                    { name = ":+:"
                    , node = "constructor_operator@(224 - 227)"
                    }
                , node = "data_type@(218 - 242)"
                }
            )
        , DeclTypeSynonym
            ( TypeSynonymDecl
                { name = NameShow
                    { name = "Result"
                    , node = "name@(249 - 255)"
                    }
                , node = "type_synomym@(244 - 272)"
                }
            )
        , DeclNewtype
            ( NewtypeDecl
                { name = NameShow
                    { name = "Thingy"
                    , node = "name@(282 - 288)"
                    }
                , node = "newtype@(274 - 301)"
                }
            )
        , DeclTypeFamily
            ( TypeFamilyDecl
                { name = NameShow
                    { name = "Testing"
                    , node = "name@(315 - 322)"
                    }
                , node = "type_family@(303 - 328)"
                }
            )
        , DeclDataFamily
            ( DataFamilyDecl
                { name = NameShow
                    { name = "MyDataFamily"
                    , node = "name@(342 - 354)"
                    }
                , node = "data_family@(330 - 354)"
                }
            )
        , DeclClass
            ( ClassDecl
                { name = NameShow
                    { name = "MyClass2"
                    , node = "name@(362 - 370)"
                    }
                , node = "class@(356 - 396)"
                }
            )
        ]
    }|]
      )
    , ( checkHir
          "exports"
          src2
          [expect|Program
    { imports = []
    , exports =
        [ ExportItem
            { namespace = NameSpaceValue
            , name = Qualified
                { mod = Nothing
                , name = NameShow
                    { name = "name1"
                    , node = "variable@(22 - 27)"
                    }
                }
            , children = []
            }
        , ExportItem
            { namespace = NameSpaceValue
            , name = Qualified
                { mod = Nothing
                , name = NameShow
                    { name = "name2"
                    , node = "variable@(33 - 38)"
                    }
                }
            , children = []
            }
        , ExportItem
            { namespace = NameSpacePattern
            , name = Qualified
                { mod = Nothing
                , name = NameShow
                    { name = "First"
                    , node = "name@(52 - 57)"
                    }
                }
            , children = []
            }
        , ExportItem
            { namespace = NameSpaceType
            , name = Qualified
                { mod = Nothing
                , name = NameShow
                    { name = "++"
                    , node = "operator@(69 - 71)"
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
                    , name = NameShow
                        { name = "First"
                        , node = "name@(34 - 39)"
                        }
                    , children = []
                    }
                , ImportItem
                    { namespace = NameSpaceValue
                    , name = NameShow
                        { name = "C"
                        , node = "name@(41 - 42)"
                        }
                    , children =
                        [ ImportChild NameSpaceType NameShow
                            { name = "Ty"
                            , node = "name@(48 - 50)"
                            }
                        , ImportChild NameSpaceValue NameShow
                            { name = ":+:"
                            , node = "constructor_operator@(53 - 56)"
                            }
                        , ImportAllChildren
                        , ImportChild NameSpaceValue NameShow
                            { name = "first"
                            , node = "variable@(63 - 68)"
                            }
                        , ImportChild NameSpaceValue NameShow
                            { name = "+++"
                            , node = "operator@(71 - 74)"
                            }
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
                    , name = NameShow
                        { name = "First"
                        , node = "name@(93 - 98)"
                        }
                    , children = []
                    }
                , ImportItem
                    { namespace = NameSpaceValue
                    , name = NameShow
                        { name = "C"
                        , node = "name@(100 - 101)"
                        }
                    , children =
                        [ ImportAllChildren
                        , ImportChild NameSpaceValue NameShow
                            { name = "first"
                            , node = "variable@(106 - 111)"
                            }
                        , ImportChild NameSpaceType NameShow
                            { name = "Another"
                            , node = "name@(118 - 125)"
                            }
                        , ImportChild NameSpaceValue NameShow
                            { name = "+++"
                            , node = "operator@(128 - 131)"
                            }
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
