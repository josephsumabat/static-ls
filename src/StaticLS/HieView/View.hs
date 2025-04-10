{-# LANGUAGE TemplateHaskell #-}

-- We use lens here because traversals
module StaticLS.HieView.View (
  File (..),
  viewHieFile,
  NodeOrigin (..),
  Ast (..),
  SourcedNodeInfo,
  NodeAnnotation (..),
  NodeInfo (..),
  Identifier (..),
  IdentifierDetails (..),
  ContextInfo (..),
  BindType (..),
  Scope (..),
  identifierModule,
  identiferName,
  Name,
  ModuleName,
  InternStr,
  EvVarSource (..),
  EvBindDeps (..),
  -- optics
  _IdentName,
  _IdentModule,
  _EvidenceVarBind,
  -- printing
  pPrint,
  pPrintColor,
  readFile,
)
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT, exceptToMaybeT)
import Data.Bifunctor (bimap)
import Data.Foldable (fold)
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.LineColRange (LineColRange)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as T.Encoding
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic)
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins qualified as GHC (LexicalFastString (..))
import Optics
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.HieView.InternStr (InternStr)
import StaticLS.HieView.InternStr qualified as InternStr
import StaticLS.HieView.Name (ModuleName, Name)
import StaticLS.HieView.Name qualified as Name
import StaticLS.HieView.Type (TypeIndex)
import StaticLS.HieView.Type qualified as Type
import StaticLS.HieView.Utils qualified as Utils
import Text.Pretty.Simple qualified as PS
import Prelude hiding (readFile)

data File = File
  { asts :: (Map FilePath (Ast TypeIndex))
  , typeArray :: Type.TypeArray
  , source :: Text
  }
  deriving (Show)

data Ast a = Ast
  { range :: LineColRange
  , sourcedNodeInfo :: SourcedNodeInfo a
  , children :: [Ast a]
  }
  deriving (Show, Eq)

data NodeAnnotation = NodeAnnotation
  { constr :: InternStr
  , ty :: InternStr
  }
  deriving (Show, Eq, Generic)

instance Hashable NodeAnnotation

data NodeInfo a = NodeInfo
  { annotations :: HashSet NodeAnnotation
  , tys :: [a]
  , identifiers :: HashMap Identifier (IdentifierDetails a)
  }
  deriving (Show, Eq)

data Identifier
  = IdentModule !ModuleName
  | IdentName !Name
  deriving (Show, Eq, Generic)

instance Hashable Identifier

data IdentifierDetails a = IdentifierDetails
  { info :: HashSet ContextInfo
  , ty :: Maybe a
  }
  deriving (Show, Eq)

data Scope
  = NoScope
  | LocalScope LineColRange
  | ModuleScope
  deriving (Show, Eq, Generic)

instance Hashable Scope

data BindType = RegularBind | InstanceBind
  deriving (Show, Eq, Generic)

instance Hashable BindType

data DeclType = DeclOther
  deriving (Show, Eq, Generic)

instance Hashable DeclType

data EvBindDeps = EvBindDeps {deps :: [Name]}
  deriving (Show, Eq, Generic)

instance Hashable EvBindDeps

data EvVarSource
  = EvOther
  | EvInstBind {isSuperInst :: Bool, cls :: Name}
  | EvLetBind EvBindDeps
  deriving (Show, Eq, Generic)

instance Hashable EvVarSource

data ContextInfo
  = ContextOther
  | ValBind BindType Scope (Maybe LineColRange)
  | PatternBind Scope Scope (Maybe LineColRange)
  | ClassTyDecl (Maybe LineColRange)
  | TyDecl
  | Decl DeclType (Maybe LineColRange)
  | EvidenceVarBind EvVarSource Scope (Maybe LineColRange)
  | EvidenceVarUse
  deriving (Show, Eq, Generic)

instance Hashable ContextInfo

data NodeOrigin
  = SourceInfo
  | GeneratedInfo
  deriving (Show, Eq, Ord)

type SourcedNodeInfo a = Map NodeOrigin (NodeInfo a)

$( fold
     [ makePrisms ''ContextInfo
     , makePrisms ''Identifier
     ]
 )

readFile :: (MonadIO m) => FilePath -> MaybeT m File
readFile = exceptToMaybeT . fmap viewHieFile . HIE.File.readHieFile

pPrint :: File -> Text
pPrint file = TL.toStrict $ PS.pShowOpt (PS.defaultOutputOptionsNoColor {PS.outputOptionsIndentAmount = 2}) file.asts

pPrintColor :: File -> Text
pPrintColor file = TL.toStrict $ PS.pShowOpt (PS.defaultOutputOptionsDarkBg {PS.outputOptionsIndentAmount = 2}) file.asts

viewHieFile :: GHC.HieFile -> File
viewHieFile hieFile =
  File
    { asts = viewHieAsts $ GHC.hie_asts hieFile
    , typeArray = Type.fromGHCHieTypes $ GHC.hie_types hieFile
    , source = T.Encoding.decodeUtf8 $ GHC.hie_hs_src hieFile
    }

viewHieAsts :: GHC.HieASTs GHC.TypeIndex -> Map FilePath (Ast TypeIndex)
viewHieAsts hieAsts =
  Map.fromList
    ( ( \((GHC.LexicalFastString k), v) ->
          (InternStr.toString $ InternStr.fromGHCFastString k, viewAst v)
      )
        <$> (Map.toList (GHC.getAsts hieAsts))
    )

viewNodeOrigin :: GHC.NodeOrigin -> NodeOrigin
viewNodeOrigin = \case
  GHC.SourceInfo -> SourceInfo
  GHC.GeneratedInfo -> GeneratedInfo

viewAst :: GHC.HieAST GHC.TypeIndex -> Ast TypeIndex
viewAst hieAst =
  Ast
    { sourcedNodeInfo = viewSourcedNodeInfo (GHC.sourcedNodeInfo hieAst)
    , range = Utils.realSrcSpanToLcRange $ GHC.nodeSpan hieAst
    , children = viewAst <$> GHC.nodeChildren hieAst
    }

viewSourcedNodeInfo :: GHC.SourcedNodeInfo GHC.TypeIndex -> SourcedNodeInfo TypeIndex
viewSourcedNodeInfo (GHC.SourcedNodeInfo sourcedNodeInfo) =
  Map.fromList
    ( (bimap viewNodeOrigin viewNodeInfo)
        <$> (Map.toList sourcedNodeInfo)
    )

viewNodeAnnotation :: GHC.NodeAnnotation -> NodeAnnotation
viewNodeAnnotation GHC.NodeAnnotation {nodeAnnotConstr, nodeAnnotType} =
  NodeAnnotation
    { constr = InternStr.fromGHCFastString nodeAnnotConstr
    , ty = InternStr.fromGHCFastString nodeAnnotType
    }

viewNodeInfo :: GHC.NodeInfo GHC.TypeIndex -> NodeInfo TypeIndex
viewNodeInfo GHC.NodeInfo {nodeAnnotations, nodeType, nodeIdentifiers} =
  NodeInfo
    { annotations = HashSet.fromList (viewNodeAnnotation <$> (Set.toList nodeAnnotations))
    , tys = Type.fromGHCTypeIndex <$> nodeType
    , identifiers =
        HashMap.fromList
          ( map
              (bimap viewIdentifier viewIdentifierDetails)
              (Map.toList nodeIdentifiers)
          )
    }

identiferName :: Identifier -> Maybe Name
identiferName = \case
  IdentModule _ -> Nothing
  IdentName name -> Just name

identifierModule :: Identifier -> Maybe ModuleName
identifierModule = \case
  IdentModule modName -> Just modName
  IdentName _ -> Nothing

viewIdentifier :: GHC.Identifier -> Identifier
viewIdentifier identifier = case identifier of
  Left modName -> IdentModule $ Name.fromGHCModuleName modName
  Right name -> IdentName (Name.fromGHCName name)

viewIdentifierDetails :: GHC.IdentifierDetails GHC.TypeIndex -> IdentifierDetails TypeIndex
viewIdentifierDetails GHC.IdentifierDetails {identInfo, identType} =
  IdentifierDetails
    { info = HashSet.fromList (viewContextInfo <$> (Set.toList identInfo))
    , ty = Type.fromGHCTypeIndex <$> identType
    }

viewBindType :: GHC.BindType -> BindType
viewBindType = \case
  GHC.RegularBind -> RegularBind
  GHC.InstanceBind -> InstanceBind

viewScope :: GHC.Scope -> Scope
viewScope = \case
  GHC.NoScope -> NoScope
  GHC.LocalScope loc -> LocalScope (Utils.realSrcSpanToLcRange loc)
  GHC.ModuleScope -> ModuleScope

viewContextInfo :: GHC.ContextInfo -> ContextInfo
viewContextInfo = \case
  GHC.ValBind bindType scope range ->
    ValBind
      (viewBindType bindType)
      (viewScope scope)
      (Utils.realSrcSpanToLcRange <$> range)
  GHC.PatternBind scope1 scope2 range ->
    PatternBind
      (viewScope scope1)
      (viewScope scope2)
      (Utils.realSrcSpanToLcRange <$> range)
  GHC.ClassTyDecl range ->
    ClassTyDecl (Utils.realSrcSpanToLcRange <$> range)
  GHC.TyDecl -> TyDecl
  GHC.Decl _ty range -> Decl DeclOther (fmap Utils.realSrcSpanToLcRange range)
  GHC.EvidenceVarBind evSource scope range ->
    EvidenceVarBind (viewEvVarSource evSource) (viewScope scope) (Utils.realSrcSpanToLcRange <$> range)
  GHC.EvidenceVarUse -> EvidenceVarUse
  _ -> ContextOther

viewEvBindDeps :: GHC.EvBindDeps -> EvBindDeps
viewEvBindDeps deps = EvBindDeps (Name.fromGHCName <$> (GHC.getEvBindDeps deps))

viewEvVarSource :: GHC.EvVarSource -> EvVarSource
viewEvVarSource = \case
  GHC.EvInstBind isSuperInst cls -> EvInstBind isSuperInst (Name.fromGHCName cls)
  GHC.EvLetBind deps -> EvLetBind (viewEvBindDeps deps)
  _ -> EvOther
