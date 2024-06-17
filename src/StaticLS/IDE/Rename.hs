module StaticLS.IDE.Rename where

import Control.Monad (join)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.HashMap.Strict qualified as HashMap
import Data.LineColRange (LineColRange (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (LineCol (..))
import Data.Rope qualified as Rope
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import Data.Traversable (for)
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins qualified as GHC
import HieDb qualified
import StaticLS.FileEnv
import StaticLS.HIE
import StaticLS.HIE.File hiding (getHieSource)
import StaticLS.IDE.FileWith (FileLcRange, FileWith (..))
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.StaticEnv
import StaticLS.StaticLsEnv
import StaticLS.IDE.SourceEdit (SourceEdit)

rename :: AbsPath -> LineCol -> StaticLsM SourceEdit
rename path lineCol = do
  pure undefined
