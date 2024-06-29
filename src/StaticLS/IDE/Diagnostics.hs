module StaticLS.IDE.Diagnostics where

import Data.Text (Text)
import StaticLS.IDE.FileWith (FileLcRange)

data Severity = Error | Warning
  deriving (Show, Eq)

data Diagnostic = Diagnostic
  { range :: !FileLcRange
  , severity :: !Severity
  , message :: !Text
  }
  deriving (Show, Eq)
