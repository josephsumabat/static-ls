module StaticLS.HieView.Type (
  Type,
  TypeArray,
  FlatType,
  TypeIndex (..),
  fromGHCHieType,
  fromGHCHieTypeFlat,
  fromGHCHieTypes,
  fromGHCTypeIndex,
  recoverFullType,
  getTypeNames,
  printType,
) where

import Data.Array (Array)
import Data.Foldable qualified as Foldable
import Data.Text (Text)
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC
import GHC.Iface.Type qualified
import StaticLS.HieView.Name (Name)
import StaticLS.HieView.Name qualified as Name
import StaticLS.SDoc qualified as SDoc

newtype TypeArray = TypeArray {tys :: Array GHC.TypeIndex GHC.HieTypeFlat}

instance Show TypeArray where
  show _ = "TypeArray"

fromGHCHieTypes :: Array GHC.TypeIndex GHC.HieTypeFlat -> TypeArray
fromGHCHieTypes = TypeArray

newtype TypeIndex = TypeIndex {idx :: GHC.TypeIndex}
  deriving (Show, Eq, Ord)

fromGHCTypeIndex :: GHC.TypeIndex -> TypeIndex
fromGHCTypeIndex = TypeIndex

newtype FlatType = FlatType {ty :: GHC.HieTypeFlat}

fromGHCHieTypeFlat :: GHC.HieTypeFlat -> FlatType
fromGHCHieTypeFlat = FlatType

newtype Type = Type {ty :: GHC.HieTypeFix}

printType :: Type -> Text
printType = SDoc.showGhc . GHC.hieTypeToIface . (.ty)

recoverFullType :: TypeArray -> TypeIndex -> Type
recoverFullType (TypeArray tys) (TypeIndex ty) = Type (GHC.recoverFullType ty tys)

fromGHCHieType :: GHC.HieTypeFix -> Type
fromGHCHieType = Type

getTypeNames :: Type -> [Name]
getTypeNames (Type ty) =
  goTypeToName [] ty
 where
  goTypeToName :: [Name] -> GHC.HieTypeFix -> [Name]
  goTypeToName acc (GHC.Roll tyFix) =
    Foldable.foldl' goTypeToName (fmap Name.fromGHCName name ++ acc) tyFix
   where
    name = case tyFix of
      (GHC.HTyConApp (GHC.Iface.Type.IfaceTyCon name _info) _args) -> [name]
      _ -> []
