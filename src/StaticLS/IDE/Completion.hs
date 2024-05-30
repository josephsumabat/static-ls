{-# LANGUAGE BlockArguments #-}

module StaticLS.IDE.Completion where

import qualified Language.LSP.Protocol.Types as LSP
import StaticLS.StaticEnv
import qualified TreeSitter.Api as TS
import TreeSitter.Query

getCompletion :: LSP.Uri -> StaticLs ()
getCompletion uri = do
    fileState <- getFileState uri
    case fileState of
        Nothing -> pure ()
        Just fs ->
            pure ()

headerQuery :: NodeQuery TS.Node
headerQuery = do
    (moduleNode, ()) <-
        nodeName_ "haskell" do
            child $ nodeName_ "header" do
                "module" #= nodeName "module" (pure ())
    pure moduleNode
