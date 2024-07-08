module StaticLS.HieView.Type (Type, TypeArray, FlatType) where

import Data.Array (Array)
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC

newtype TypeArray = TypeArray {tys :: Array GHC.TypeIndex GHC.HieTypeFlat}

newtype FlatType = FlatType {ty :: GHC.HieTypeFlat}

newtype Type = Type {ty :: GHC.HieTypeFix}

fromGHCHieType :: GHC.HieTypeFix -> Type
fromGHCHieType = Type
