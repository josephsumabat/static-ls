{-# LANGUAGE RecordWildCards #-}

module StaticLS.GhcidSession where

import Data.Path
import Text.Parsec
import Text.Parsec.Text (Parser)
import Control.Exception (displayException)

data GhcidSession = GhcidSession
  { workingDirectory :: AbsPath
  }

parseGhcidSession :: Parser GhcidSession
parseGhcidSession = do
  workingDirectoryFP <- manyTill anyChar newline
  workingDirectory <- case filePathToAbsThrow workingDirectoryFP of
    Left e -> fail $ displayException e
    Right x -> pure x

  eof *> pure GhcidSession{..}
