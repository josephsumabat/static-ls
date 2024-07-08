module StaticLS.HieView.Type (
  Type,
  TypeArray,
  FlatType,
  TypeIndex (..),
  fromGHCHieType,
  fromGHCHieTypeFlat,
  fromGHCHieTypes,
  fromGHCTypeIndex,
) where

import Data.Array (Array)
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC

newtype TypeArray = TypeArray {tys :: Array GHC.TypeIndex GHC.HieTypeFlat}

fromGHCHieTypes :: Array GHC.TypeIndex GHC.HieTypeFlat -> TypeArray
fromGHCHieTypes = TypeArray

newtype TypeIndex = TypeIndex {idx :: GHC.TypeIndex}

fromGHCTypeIndex :: GHC.TypeIndex -> TypeIndex
fromGHCTypeIndex = TypeIndex

newtype FlatType = FlatType {ty :: GHC.HieTypeFlat}

fromGHCHieTypeFlat :: GHC.HieTypeFlat -> FlatType
fromGHCHieTypeFlat = FlatType

newtype Type = Type {ty :: GHC.HieTypeFix}

fromGHCHieType :: GHC.HieTypeFix -> Type
fromGHCHieType = Type
