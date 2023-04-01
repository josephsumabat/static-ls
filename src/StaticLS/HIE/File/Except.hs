module StaticLS.HIE.File.Except where

import Control.Exception
import Control.Monad.Trans.Except

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
