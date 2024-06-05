{-# LANGUAGE DerivingStrategies #-}

module StaticLS.HIE.File.Except where

import Control.Exception
import GHC.Iface.Ext.Binary qualified as GHC

data HieFileReadException
  = HieFileReadException
  | HieFileVersionException GHC.HieHeader
  deriving stock (Show)

instance Exception HieFileReadException
