{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module StaticLS.HIE.File.Except where

import Control.Exception

newtype HieFileReadException
    = HieFileReadException IOException
    deriving stock (Show)
    deriving newtype Exception
