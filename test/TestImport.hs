module TestImport where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import qualified GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Paths as GHC
import qualified GHC.Types.Name.Cache as GHC
import GHC.Unit.Types
import HieDb (
    initConn,
    withHieDb,
 )
import Language.LSP.Server (
    Handlers,
    LanguageContextEnv,
    LspT,
    ServerDefinition (..),
 )
import qualified Language.LSP.Server as LSP
import Language.LSP.Types
import StaticLS.HIE
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.Monad
import System.FilePath ((</>))
import System.Directory

initStaticEnv :: IO StaticEnv
initStaticEnv = do
  wsRoot <- makeAbsolute "."
  let dbPath = wsRoot </> ".hiedb"
  hscEnv <- liftIO $ GHC.runGhc (Just GHC.libdir) GHC.getSession
  nameCache <- liftIO $ GHC.initNameCache 'a' []
  pure $ StaticEnv
                    { hieDbPath = dbPath
                    , hscEnv = hscEnv
                    , nameCache = nameCache
                    , wsRoot = wsRoot
                    }
