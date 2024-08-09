module StaticLS.Hir.Types where

import AST (DynNode)
import AST qualified
import AST.Haskell qualified as H
import AST.Haskell qualified as Haskell
import AST.Sum (Nil, (:+))
import Data.Function (on)
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)

data NameSpace
  = NameSpaceValue
  | NameSpaceType
  | NameSpacePattern
  deriving (Show, Eq, Generic)

instance Hashable NameSpace

type ThSplice = Haskell.TopSplice :+ Haskell.Splice

data Name = Name
  { node :: !DynNode
  , isOperator :: !Bool
  , isConstructor :: !Bool
  }

data NameShow = NameShow {name :: Text, node :: DynNode}
  deriving (Show)

instance Show Name where
  show Name {node} = show NameShow {name = AST.nodeToText node, node}

instance Eq Name where
  (==) = (==) `on` (.node.nodeText)

instance Hashable Name where
  hashWithSalt salt name = hashWithSalt salt name.node.nodeText

data Qualified = Qualified
  { mod :: Maybe ModuleName
  , name :: Name
  }
  deriving (Show, Eq, Generic)

instance Hashable Qualified

data ModuleText = ModuleText
  { parts :: NonEmpty Text
  , text :: Text
  }
  deriving (Show)

instance Eq ModuleText where
  (==) = (==) `on` (.text)

instance Hashable ModuleText where
  hashWithSalt salt ModuleText {text} = hashWithSalt salt text

data ModuleName = ModuleName
  { mod :: ModuleText
  , node :: H.Module
  }
  deriving (Show)

instance Eq ModuleName where
  (==) = (==) `on` (.mod)

instance Hashable ModuleName where
  hashWithSalt salt ModuleName {mod} = hashWithSalt salt mod

data ImportChildren
  = ImportAllChildren
  | ImportChild NameSpace Name
  deriving (Show)

data ImportItem = ImportItem
  { namespace :: NameSpace
  , name :: Name
  , children :: [ImportChildren]
  }
  deriving (Show)

data ExportChildren
  = ExportAllChildren
  | ExportChild NameSpace Qualified
  deriving (Show, Eq, Generic)

instance Hashable ExportChildren

data ExportItem
  = ExportItem
      { namespace :: NameSpace
      , name :: Qualified
      , children :: [ExportChildren]
      }
  | ExportModuleItem ModuleName
  deriving (Show, Eq, Generic)

instance Hashable ExportItem

data ImportName = ImportName
  { name :: Text
  }
  deriving (Show, Eq)

data Import = Import
  { mod :: ModuleText
  , alias :: Maybe ModuleText
  , qualified :: !Bool
  , hiding :: !Bool
  , importList :: [ImportItem]
  }
  deriving (Show)

pattern OpenImport :: ModuleText -> Import
pattern OpenImport mod = Import {mod, alias = Nothing, qualified = False, hiding = False, importList = []}

type ParseNameTypes =
  Haskell.Name
    :+ Haskell.Constructor
    :+ Haskell.Variable
    :+ Haskell.Operator
    :+ Haskell.FieldName
    :+ Haskell.ConstructorOperator
    :+ Nil

type ParseQualifiedTypes = H.Qualified :+ ParseNameTypes

data DataDecl = DataDecl
  { name :: Name
  , node :: H.DataType
  }
  deriving (Show)

data ClassDecl = ClassDecl
  { name :: Name
  , node :: H.Class
  }
  deriving (Show)

data BindDecl = BindDecl
  { name :: Name
  , node :: H.Bind :+ H.Function :+ AST.Nil
  }
  deriving (Show)

data SigDecl = SigDecl
  { name :: Name
  , node :: H.Signature
  }
  deriving (Show)

data DataFamilyDecl = DataFamilyDecl
  {name :: Name, node :: H.DataFamily}
  deriving (Show)

data NewtypeDecl = NewtypeDecl
  { name :: Name
  , node :: H.Newtype
  }
  deriving (Show)

data PatternSigDecl = PatternSigDecl
  { name :: Name,
    node :: H.Signature
  }
  deriving (Show)

data PatternDecl = PatternDecl
  { name :: Name
  , node :: H.Equation
  }
  deriving (Show)

data TypeFamilyDecl = TypeFamilyDecl
  { name :: Name
  , node :: H.TypeFamily
  }
  deriving (Show)

data TypeSynonymDecl = TypeSynonymDecl
  { name :: Name
  , node :: H.TypeSynomym
  }
  deriving (Show)

data Decl
  = DeclData DataDecl
  | DeclNewtype NewtypeDecl
  | DeclClass ClassDecl
  | DeclSig SigDecl
  | DeclBind BindDecl
  | DeclDataFamily DataFamilyDecl
  | DeclPatternSig PatternSigDecl
  | DeclPattern PatternDecl
  | DeclTypeFamily TypeFamilyDecl
  | DeclTypeSynonym TypeSynonymDecl
  deriving (Show)

data Program = Program
  { imports :: [Import]
  , exports :: [ExportItem]
  , decls :: [Decl]
  }
  deriving (Show)

type GetNameTypes =
  Haskell.Name
    :+ Haskell.Constructor
    :+ Haskell.Variable
    :+ Haskell.Operator
    :+ Haskell.FieldName
    :+ Haskell.ConstructorOperator
    :+ Nil

data ThQuotedName = ThQuotedName
  { isTy :: Bool
  , node :: AST.DynNode
  }
