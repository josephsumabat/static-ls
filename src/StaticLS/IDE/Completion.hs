{-# LANGUAGE BlockArguments #-}

module StaticLS.IDE.Completion where

import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.StaticLsEnv

getCompletion :: (HasStaticLsEnv m) => LSP.Uri -> m ()
getCompletion uri = do
    fileState <- getFileState uri
    case fileState of
        Nothing -> pure ()
        Just _fs ->
            pure ()
