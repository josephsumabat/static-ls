module StaticLS.IDE.InlayHints.Imports where

import StaticLS.Logger (logError)

getInlayHints :: AbsPath -> StaticEnvOptions -> StaticLsM [InlayHint]
getInlayHints _path _options = do
  logError "defaultInlayHint " <> defaultInlayHint
  [defaultInlayHint]
