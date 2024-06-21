module StaticLS.HieView where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Types.Avail qualified as GHC
import HieDb.Compat qualified

data File = File
  { path :: FilePath
  , asts :: (Map FilePath Ast)
  }

data NodeOrigin
  = SourceInfo
  | GeneratedInfo

data Ast = Ast
  { sourcedNodeInfo :: !(Map NodeOrigin NodeInfo)
  }

data NodeAnnotation = NodeAnnotation
  { constr :: Text
  , ty :: Text
  }
  deriving (Show, Eq, Ord)

data TypeInfo = TypeInfo

data NodeInfo = NodeInfo
  { annotations :: Set NodeAnnotation
  }

data Name = Name
  { occName :: OccName
  }

viewName :: GHC.Name -> Name
viewName name =
  Name
    { occName = viewOccName (GHC.occName name)
    }

data Identifier
  = IdentModule !Text
  | IdentName !Name

data AvailInfo
  = Avail Name
  | AvailTC Name [Name]
  | AvailOther

viewAvailInfo :: GHC.AvailInfo -> AvailInfo
viewAvailInfo availInfo = case availInfo of
  HieDb.Compat.AvailName name -> Avail (viewName name)
  HieDb.Compat.AvailTC name names _ -> AvailTC (viewName name) (map viewName names)
  HieDb.Compat.AvailFL _ -> AvailOther

data OccName = OccName
  { text :: Text
  }

viewOccName :: GHC.OccName -> OccName
viewOccName = OccName . T.pack . GHC.occNameString
