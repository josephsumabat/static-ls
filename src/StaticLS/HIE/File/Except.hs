{-# LANGUAGE DerivingStrategies #-}

module StaticLS.HIE.File.Except where

import Control.Exception

data HieFileReadException
    = HieFileReadException
    deriving stock (Show)

instance Exception HieFileReadException
