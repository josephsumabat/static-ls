module StaticLS.HieView where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Types.Avail qualified as GHC
import HieDb.Compat qualified
import StaticLS.HieView.InternStr (InternStr)
import StaticLS.HieView.InternStr qualified as InternStr
import StaticLS.HieView.Name (Name)
import StaticLS.HieView.Name qualified as Name
import StaticLS.HieView.Type (FlatType, Type)

data File a = File
  { path :: FilePath
  , asts :: (Map FilePath (Ast a))
  }

data NodeOrigin
  = SourceInfo
  | GeneratedInfo

data Ast a = Ast
  { sourcedNodeInfo :: !(Map NodeOrigin (NodeInfo a))
  }

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

data TypeInfo = TypeInfo

data NodeInfo a = NodeInfo
  { annotations :: HashSet NodeAnnotation
  , tys :: [a]
  , identifiers :: HashMap Identifier (IdentifierDetails a)
  }

viewAst :: GHC.HieAST GHC.HieTypeFlat -> Ast FlatType
viewAst hieAst = undefined

data Identifier
  = IdentModule !InternStr
  | IdentName !Name
  deriving (Show, Eq, Generic)

instance Hashable Identifier

viewIdentifier :: GHC.Identifier -> Identifier
viewIdentifier identifier = case identifier of
  Left (GHC.ModuleName modName) -> IdentModule $ InternStr.fromGHCFastString modName
  Right name -> IdentName (Name.fromGHCName name)
  
data IdentifierDetails a = IdentifierDetails
  { info :: HashSet ContextInfo
  , ty :: Maybe a
  }

viewIdentifierDetails :: GHC.IdentifierDetails a -> IdentifierDetails a
viewIdentifierDetails GHC.IdentifierDetails {identInfo, identType} =
  IdentifierDetails
    { info = HashSet.fromList (viewContextInfo <$> (Set.toList identInfo))
    , ty = identType
    }

data ContextInfo = ContextOther
  deriving (Show, Eq, Generic)

instance Hashable ContextInfo

viewContextInfo :: GHC.ContextInfo -> ContextInfo
viewContextInfo = \case
  _ -> ContextOther

-- data Name = Name
--   { occName :: OccName
--   }

-- viewName :: GHC.Name -> Name
-- viewName name =
--   Name
--     { occName = viewOccName (GHC.occName name)
--     }

-- data AvailInfo
--   = Avail Name
--   | AvailTC Name [Name]
--   | AvailOther

-- viewAvailInfo :: GHC.AvailInfo -> AvailInfo
-- viewAvailInfo availInfo = case availInfo of
--   HieDb.Compat.AvailName name -> Avail (viewName name)
--   HieDb.Compat.AvailTC name names _ -> AvailTC (viewName name) (map viewName names)
--   HieDb.Compat.AvailFL _ -> AvailOther

-- data OccName = OccName
--   { text :: Text
--   }

-- viewOccName :: GHC.OccName -> OccName
-- viewOccName = OccName . T.pack . GHC.occNameString
