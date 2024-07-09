module StaticLS.HieView.InternStr (
  InternStr,
  fromString,
  toString,
  fromText,
  toText,
  fromGHCFastString,
) where

import Data.Hashable (Hashable (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (FastString)
import GHC.Data.FastString qualified as FS

newtype InternStr = InternStr {str :: FastString}
  deriving (Show, Eq)

instance Hashable InternStr where
  hashWithSalt salt (InternStr fs) = hashWithSalt @Int salt (FS.uniqueOfFS fs)

fromGHCFastString :: FastString -> InternStr
fromGHCFastString = InternStr
{-# INLINE fromGHCFastString #-}

fromString :: String -> InternStr
fromString x = InternStr (FS.fsLit x)
{-# INLINE fromString #-}

toString :: InternStr -> String
toString = FS.unpackFS . (.str)
{-# INLINE toString #-}

fromText :: Text -> InternStr
fromText = fromString . T.unpack

toText :: InternStr -> Text
toText = T.pack . toString
{-# INLINE toText #-}
