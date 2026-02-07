module StaticLS.IDE.InlayHints.Imports where

import Data.Path (AbsPath)
import Data.Text qualified as T
import StaticLS.Logger (logError)
import StaticLS.IDE.InlayHints.Common (defaultInlayHint)
import StaticLS.IDE.InlayHints.Types
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import StaticLS.Monad (StaticLsM)

getInlayHints :: AbsPath -> StaticEnvOptions -> StaticLsM [InlayHint]
getInlayHints _path _options = do
  case defaultInlayHint.label of
    Left l -> logError $ "defaultInlayHint " <> (T.pack $ show l)
    Right (r0 : _rr) -> logError $ "defaultInlayHint " <> r0.value
  pure [defaultInlayHint]
