{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use camelCase" -}

module StaticLS.IDE.InlayHints (
  getInlayHints,
) where

import Data.Path
import StaticLS.IDE.InlayHints.TypeAnnotations qualified as TypeAnnotations
import StaticLS.IDE.InlayHints.Types
import StaticLS.Monad
import StaticLS.StaticEnv.Options

getInlayHints :: AbsPath -> StaticEnvOptions -> StaticLsM [InlayHint]
getInlayHints path options =
  concat
    <$> sequenceA
      [ TypeAnnotations.getInlayHints path options.inlayLengthCap
      -- , Wildcard.getInlayHints path
      ]
