{-# LANGUAGE DerivingStrategies #-}

module StaticLS.HIE.File.Except where

import Control.Exception
import qualified GHC.Iface.Ext.Binary as GHC

data HieFileReadException
    = HieFileReadException
    | HieFileVersionException GHC.HieHeader
    deriving stock (Show, Eq)

instance Exception HieFileReadException
