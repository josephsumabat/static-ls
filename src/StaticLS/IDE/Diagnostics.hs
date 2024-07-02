module StaticLS.IDE.Diagnostics where

import Data.Text (Text)
import StaticLS.IDE.FileWith (FileLcRange)

data Severity = Error | Warning
  deriving (Show, Eq)

data Diagnostic = Diagnostic
  { range :: !FileLcRange
  , severity :: !Severity
  , message :: !Text
  , code :: Maybe Text
  , codeUri :: Maybe Text
  }
  deriving (Show, Eq)

mkDiagnostic :: FileLcRange -> Severity -> Text -> Diagnostic
mkDiagnostic range severity message =
  Diagnostic
    { range
    , severity
    , message
    , code = Nothing
    , codeUri = Nothing
    }
