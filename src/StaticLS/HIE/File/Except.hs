{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module StaticLS.HIE.File.Except where

import Control.Exception

newtype HieFileReadException
    = HieFileReadException IOException
    deriving stock (Show)
    deriving newtype Exception

data HieFileTdiException
    = HieTdiReadException HieFileReadException
    | HieTdiSrcNotFoundException
    | HieTdiHieNotFoundException
    deriving stock (Show)

instance Exception HieFileTdiException
