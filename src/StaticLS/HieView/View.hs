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
)
where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.LineColRange (LineColRange)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as T.Encoding
import GHC.Generics (Generic)
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins qualified as GHC (LexicalFastString (..), moduleNameFS)
import StaticLS.HieView.InternStr (InternStr)
import StaticLS.HieView.InternStr qualified as InternStr
import StaticLS.HieView.Name (Name)
import StaticLS.HieView.Name qualified as Name
import StaticLS.HieView.Type (TypeIndex)
import StaticLS.HieView.Type qualified as Type
import StaticLS.HieView.Utils qualified as Utils

data File = File
  { asts :: (Map FilePath (Ast TypeIndex))
  , typeArray :: Type.TypeArray
  , source :: Text
  }

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
data NodeOrigin
  = SourceInfo
  | GeneratedInfo
  deriving (Show, Eq, Ord)

viewNodeOrigin :: GHC.NodeOrigin -> NodeOrigin
viewNodeOrigin = \case
  GHC.SourceInfo -> SourceInfo
  GHC.GeneratedInfo -> GeneratedInfo

data Ast a = Ast
  { sourcedNodeInfo :: !(Map NodeOrigin (NodeInfo a))
  , range :: LineColRange
  , children :: [Ast a]
  }

viewAst :: GHC.HieAST GHC.TypeIndex -> Ast TypeIndex
viewAst hieAst =
  Ast
    { sourcedNodeInfo = viewSourcedNodeInfo (GHC.sourcedNodeInfo hieAst)
    , range = Utils.realSrcSpanToLcRange $ GHC.nodeSpan hieAst
    , children = viewAst <$> GHC.nodeChildren hieAst
    }

type SourcedNodeInfo = Map NodeOrigin (NodeInfo TypeIndex)

viewSourcedNodeInfo :: GHC.SourcedNodeInfo GHC.TypeIndex -> SourcedNodeInfo
viewSourcedNodeInfo (GHC.SourcedNodeInfo sourcedNodeInfo) =
  Map.fromList
    ( ( \(k, v) ->
          (viewNodeOrigin k, viewNodeInfo v)
      )
        <$> (Map.toList sourcedNodeInfo)
    )

data NodeAnnotation = NodeAnnotation
  { constr :: InternStr
  , ty :: InternStr
  }
  deriving (Show, Eq, Generic)

instance Hashable NodeAnnotation

viewNodeAnnotation :: GHC.NodeAnnotation -> NodeAnnotation
viewNodeAnnotation GHC.NodeAnnotation {nodeAnnotConstr, nodeAnnotType} =
  NodeAnnotation
    { constr = InternStr.fromGHCFastString nodeAnnotConstr
    , ty = InternStr.fromGHCFastString nodeAnnotType
    }

data NodeInfo a = NodeInfo
  { annotations :: HashSet NodeAnnotation
  , tys :: [a]
  , identifiers :: HashMap Identifier (IdentifierDetails a)
  }

viewNodeInfo :: GHC.NodeInfo GHC.TypeIndex -> NodeInfo TypeIndex
viewNodeInfo GHC.NodeInfo {nodeAnnotations, nodeType, nodeIdentifiers} =
  NodeInfo
    { annotations = HashSet.fromList (viewNodeAnnotation <$> (Set.toList nodeAnnotations))
    , tys = Type.fromGHCTypeIndex <$> nodeType
    , identifiers =
        HashMap.fromList
          ( map
              (\(k, v) -> (viewIdentifier k, viewIdentifierDetails v))
              (Map.toList nodeIdentifiers)
          )
    }

data Identifier
  = IdentModule !InternStr
  | IdentName !Name
  deriving (Show, Eq, Generic)

instance Hashable Identifier

viewIdentifier :: GHC.Identifier -> Identifier
viewIdentifier identifier = case identifier of
  Left modName -> IdentModule $ InternStr.fromGHCFastString $ GHC.moduleNameFS modName
  Right name -> IdentName (Name.fromGHCName name)

data IdentifierDetails a = IdentifierDetails
  { info :: HashSet ContextInfo
  , ty :: Maybe a
  }

viewIdentifierDetails :: GHC.IdentifierDetails GHC.TypeIndex -> IdentifierDetails TypeIndex
viewIdentifierDetails GHC.IdentifierDetails {identInfo, identType} =
  IdentifierDetails
    { info = HashSet.fromList (viewContextInfo <$> (Set.toList identInfo))
    , ty = Type.fromGHCTypeIndex <$> identType
    }

data ContextInfo = ContextOther
  deriving (Show, Eq, Generic)

instance Hashable ContextInfo

viewContextInfo :: GHC.ContextInfo -> ContextInfo
viewContextInfo = \case
  _ -> ContextOther
