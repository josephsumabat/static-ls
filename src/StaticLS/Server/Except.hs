module StaticLS.Server.Except where

import Control.Exception (Exception)
import Data.Text (pack)
import qualified Language.LSP.Types as LSP

class Exception e => RenderableLspException e where
    renderLspException :: e -> LSP.ResponseError
