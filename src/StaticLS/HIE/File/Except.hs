{-# LANGUAGE DerivingStrategies #-}

module StaticLS.HIE.File.Except where

import Control.Exception
import qualified GHC.Iface.Ext.Binary as GHC

data HieFileReadException
    = HieFileReadException
    | HieFileVersionException GHC.HieHeader
    deriving stock (Show)

instance Exception HieFileReadException
