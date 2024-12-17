{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use camelCase" -}

module StaticLS.IDE.InlayHints (
  getInlayHints,
) where

import Data.Path
import StaticLS.IDE.InlayHints.TypeAnnotations qualified as TypeAnnotations
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad
import StaticLS.Monad
import StaticLS.StaticEnv.Options

type InlayHintOptions = ()

getInlayHints :: AbsPath -> StaticEnvOptions -> StaticLsM [InlayHint]
getInlayHints path options = do
  typeAnnotations <- TypeAnnotations.getInlayHints path options.inlayLengthCap
  pure typeAnnotations
