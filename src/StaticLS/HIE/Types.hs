module StaticLS.HIE.Types (
  HieFile,
  MonadHieFile (..),
) where

import Data.Path (AbsPath)

import Control.Monad.Trans.Maybe (MaybeT)
import GHC.Iface.Ext.Types qualified as GHC

type HieFile = GHC.HieFile

class MonadHieFile m where
  getHieFile :: AbsPath -> MaybeT m HieFile
