{-# LANGUAGE BlockArguments #-}

module StaticLS.IDE.Completion where

import qualified Language.LSP.Protocol.Types as LSP
import StaticLS.StaticEnv

getCompletion :: LSP.Uri -> StaticLs ()
getCompletion uri = do
    fileState <- getFileState uri
    case fileState of
        Nothing -> pure ()
        Just _fs ->
            pure ()
