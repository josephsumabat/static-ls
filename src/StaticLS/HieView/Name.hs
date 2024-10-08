module StaticLS.HieView.Name (
  Name,
  ModuleName,
  fromGHCName,
  toInternStr,
  toText,
  toString,
  getUnit,
  getRange,
  getFileRange,
  toGHCOccName,
  getModule,
  toGHCModuleName,
  fromGHCModuleName,
  moduleNameToInternStr,
  getModuleName,
  toGHCName,
)
where

import Data.Hashable (Hashable (..))
import Data.LineColRange (LineColRange)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Plugins qualified as GHC
import GHC.Types.Unique qualified
import StaticLS.HieView.InternStr (InternStr)
import StaticLS.HieView.InternStr qualified as InternStr
import StaticLS.HieView.Utils

newtype ModuleName = ModuleName {name :: GHC.ModuleName}
  deriving (Show, Eq)

instance Hashable ModuleName where
  hashWithSalt salt = hashWithSalt @Word64 salt . fromIntegral . GHC.Types.Unique.getKey . GHC.getUnique . (.name)

moduleNameToInternStr :: ModuleName -> InternStr
moduleNameToInternStr = InternStr.fromGHCFastString . GHC.moduleNameFS . (.name)

fromGHCModuleName :: GHC.ModuleName -> ModuleName
fromGHCModuleName = ModuleName

toGHCModuleName :: ModuleName -> GHC.ModuleName
toGHCModuleName = (.name)

newtype Name = Name {name :: GHC.Name}
  deriving (Eq)

instance Hashable Name where
  hashWithSalt salt = hashWithSalt @Word64 salt . fromIntegral . GHC.Types.Unique.getKey . GHC.nameUnique . (.name)

fromGHCName :: GHC.Name -> Name
fromGHCName = Name

toInternStr :: Name -> InternStr
toInternStr = InternStr.fromGHCFastString . GHC.occNameFS . GHC.nameOccName . (.name)

toText :: Name -> Text
toText = InternStr.toText . toInternStr

toString :: Name -> String
toString = InternStr.toString . toInternStr

data NameShow = NameShow
  { name :: String
  , range :: Maybe LineColRange
  , moduleName :: Maybe ModuleName
  }
  deriving (Show)

instance Show Name where
  show name =
    show
      NameShow
        { name = toString name
        , range = getRange name
        , moduleName = getModuleName name
        }

getUnit :: Name -> Maybe Text
getUnit = fmap (T.pack . GHC.unitString . GHC.moduleUnit) . GHC.nameModule_maybe . (.name)

getRange :: Name -> Maybe LineColRange
getRange = srcSpanToLcRange . GHC.nameSrcSpan . (.name)

getFileRange :: Name -> Maybe FileRange
getFileRange = srcSpanToFileLcRange . GHC.nameSrcSpan . (.name)

-- the only reason we have this is because hiedb defines an orphan instance on OccName
-- this is temporary
toGHCOccName :: Name -> GHC.OccName
toGHCOccName = GHC.nameOccName . (.name)

-- this is temporary
toGHCName :: Name -> GHC.Name
toGHCName = (.name)

getModule :: Name -> Maybe Text
getModule = fmap (T.pack . GHC.moduleNameString . GHC.moduleName) . GHC.nameModule_maybe . (.name)

getModuleName :: Name -> Maybe ModuleName
getModuleName = fmap (fromGHCModuleName . GHC.moduleName) . GHC.nameModule_maybe . (.name)
