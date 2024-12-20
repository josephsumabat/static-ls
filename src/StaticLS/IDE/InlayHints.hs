{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use :" -}

module StaticLS.IDE.InlayHints (
  getInlayHints,
) where

import Data.Path
import StaticLS.IDE.InlayHints.TypeAnnotations qualified as TypeAnnotations
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.InlayHints.Wildcard qualified as Wildcard
import StaticLS.Monad
import StaticLS.StaticEnv.Options

getInlayHints :: AbsPath -> StaticEnvOptions -> StaticLsM [InlayHint]
getInlayHints path options = concat <$> sequenceA hints
 where
  hints =
    ( [TypeAnnotations.getInlayHints options path]
        ++ [Wildcard.getInlayHints path | options.experimentalFeatures]
    )
