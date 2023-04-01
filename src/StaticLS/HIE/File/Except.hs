module StaticLS.HIE.File.Except where

import Control.Exception

newtype HieFileReadException
    = HieFileReadException IOException
    deriving (Show)

instance Exception HieFileReadException

data HieFileTdiException
    = HieTdiReadException HieFileReadException
    | HieTdiSrcNotFoundException
    | HieTdiHieNotFoundException
    deriving (Show)

instance Exception HieFileTdiException
