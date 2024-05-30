{-# LANGUAGE BlockArguments #-}

module StaticLS.IDE.Completion where

import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.StaticEnv
import TreeSitter.Query
import TreeSitter.Api qualified as TS

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
