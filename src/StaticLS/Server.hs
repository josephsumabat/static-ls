{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.Server (
  runServer,
)
where

import Colog.Core qualified as Colog
import Control.Monad qualified as Monad
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Foldable qualified as Foldable
import Data.Path qualified as Path
import Data.Text qualified as T
import Language.LSP.Logging qualified as LSP.Logging
import Language.LSP.Protocol.Message (
  Method (..),
  TMessage,
 )
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (
  LanguageContextEnv,
  ServerDefinition (..),
  mapHandlers,
  type (<~>) (Iso),
 )
import Language.LSP.Server qualified as LSP
import StaticLS.Handlers
import StaticLS.Handlers qualified as Handlers
import StaticLS.Logger
import StaticLS.Monad
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import System.Directory qualified as Dir
import System.FSNotify qualified as FSNotify
import System.FilePath qualified as FilePath
import UnliftIO.Concurrent qualified as Conc
import UnliftIO.Exception qualified as Exception

type LspConfig = ()

data ReactorMsg where
  ReactorMsgAct :: StaticLsM () -> ReactorMsg
  ReactorMsgRequest :: StaticLsM a -> Conc.MVar a -> ReactorMsg
  ReactorMsgLspAct :: LSP.LspT LspConfig StaticLsM () -> ReactorMsg

reactor :: Conc.Chan ReactorMsg -> LSP.LanguageContextEnv LspConfig -> LoggerM IO -> StaticLsM ()
reactor chan lspEnv _logger = do
  Monad.forever do
    msg <- liftIO $ Conc.readChan chan
    Exception.catchAny
      ( case msg of
          ReactorMsgAct act -> act
          ReactorMsgRequest act resp -> do
            a <- act
            Conc.putMVar resp a
          ReactorMsgLspAct act -> do
            LSP.runLspT lspEnv act
      )
      ( \e -> do
          logError $ "Error in reactor: " <> T.pack (show e)
      )

fileWatcher :: Conc.Chan ReactorMsg -> StaticEnv -> LoggerM IO -> IO ()
fileWatcher chan staticEnv _logger = do
  mgr <- FSNotify.startManager
  _stop <-
    FSNotify.watchTree
      mgr
      (Path.toFilePath staticEnv.hieFilesPath)
      (\e -> FilePath.takeExtension e.eventPath == ".hie")
      ( \e -> Conc.writeChan chan $ ReactorMsgAct $ do
          logInfo $ "File changed: " <> T.pack (show e)
          Handlers.handleHieFileChangeEvent e
      )

  Foldable.for_ staticEnv.srcDirs \srcDir -> do
    srcDir <- pure $ Path.toFilePath srcDir
    exists <- Dir.doesDirectoryExist srcDir
    Monad.when exists do
      _stop <-
        FSNotify.watchTree
          mgr
          srcDir
          (\e -> FilePath.takeExtension e.eventPath == ".hs")
          ( \e -> Conc.writeChan chan $ ReactorMsgAct $ do
              logInfo $ "File changed: " <> T.pack (show e)
              Handlers.handleFileChangeEvent e
          )
      pure ()

  _stop <-
    FSNotify.watchDir
      mgr
      (Path.toFilePath staticEnv.wsRoot)
      (\e -> FilePath.takeFileName e.eventPath == "ghcid.txt")
      ( \e -> Conc.writeChan chan $ ReactorMsgLspAct $ do
          lift $ logInfo $ "ghcid file changed: " <> T.pack (show e)
          Handlers.handleGhcidFileChange
          pure ()
      )

  pure ()

#if MIN_VERSION_lsp(2,7,0)
initServer ::
  Conc.Chan ReactorMsg ->
  StaticEnvOptions ->
  LoggerM IO ->
  LanguageContextEnv LspConfig ->
  TMessage 'Method_Initialize ->
  IO (Either (LSP.TResponseError Method_Initialize) (LanguageContextEnv LspConfig))
initServer reactorChan staticEnvOptions logger serverConfig _ = do
  runExceptT $ do
    wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
    wsRoot <- Path.filePathToAbs wsRoot
    env <- ExceptT $ Right <$> initEnv wsRoot staticEnvOptions logger
    _ <- liftIO $ Conc.forkIO $ runStaticLsM env $ reactor reactorChan serverConfig logger
    _ <- liftIO $ Conc.forkIO $ fileWatcher reactorChan env.staticEnv logger
    -- pretend that the ghcid file changed so we can get diagnostics immediately when the editor opens
    -- the ghcid file may still have diagnostics from the last usage, so show them here
    Conc.writeChan reactorChan $ ReactorMsgLspAct Handlers.handleGhcidFileChange
    pure serverConfig
 where
  getWsRoot :: LSP.LspM config (Either (LSP.TResponseError Method_Initialize) FilePath)
  getWsRoot = do
    mRootPath <- LSP.getRootPath
    pure $ case mRootPath of
      Nothing -> Left $ LSP.TResponseError (InR ErrorCodes_InvalidRequest) "No root workspace was found" Nothing
      Just p -> Right p
#else
initServer ::
  Conc.Chan ReactorMsg ->
  StaticEnvOptions ->
  LoggerM IO ->
  LanguageContextEnv LspConfig ->
  TMessage 'Method_Initialize ->
  IO (Either LSP.ResponseError (LanguageContextEnv LspConfig))
initServer reactorChan staticEnvOptions logger serverConfig _ = do
  runExceptT $ do
    wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
    wsRoot <- Path.filePathToAbs wsRoot
    env <- ExceptT $ Right <$> initEnv wsRoot staticEnvOptions logger
    _ <- liftIO $ Conc.forkIO $ runStaticLsM env $ reactor reactorChan serverConfig logger
    _ <- liftIO $ Conc.forkIO $ fileWatcher reactorChan env.staticEnv logger
    -- pretend that the ghcid file changed so we can get diagnostics immediately when the editor opens
    -- the ghcid file may still have diagnostics from the last usage, so show them here
    Conc.writeChan reactorChan $ ReactorMsgLspAct Handlers.handleGhcidFileChange
    pure serverConfig
 where
  getWsRoot :: LSP.LspM config (Either LSP.ResponseError FilePath)
  getWsRoot = do
    mRootPath <- LSP.getRootPath
    pure $ case mRootPath of
      Nothing -> Left $ LSP.ResponseError (InR ErrorCodes_InvalidRequest) "No root workspace was found" Nothing
      Just p -> Right p
#endif

serverDef :: StaticEnvOptions -> LoggerM IO -> IO (ServerDefinition ())
serverDef argOptions logger = do
  reactorChan <- liftIO Conc.newChan
  let
    -- TODO: actually respond to the client with an error
    goReq ::
      forall (a :: LSP.Method LSP.ClientToServer LSP.Request) c.
      LSP.Handler (LSP.LspT c StaticLsM) a ->
      LSP.Handler (LSP.LspM c) a
    goReq f = \msg responseCont -> do
      env <- LSP.getLspEnv
      let responseCont' resp = do
            liftIO $ LSP.runLspT env (responseCont resp)
      Conc.writeChan reactorChan $ ReactorMsgAct $ LSP.runLspT env do
        Exception.catchAny (f msg responseCont') $ \e -> do
          logException e
          _ <- respondWithError responseCont' e
          pure ()

    goNot ::
      forall (a :: LSP.Method LSP.ClientToServer LSP.Notification) c.
      LSP.Handler (LSP.LspT c StaticLsM) a ->
      LSP.Handler (LSP.LspM c) a
    goNot f = \msg -> do
      env <- LSP.getLspEnv
      Conc.writeChan reactorChan $ ReactorMsgAct $ LSP.runLspT env do
        catchAndLog $ f msg
  pure
    ServerDefinition
      { onConfigChange = \_conf -> pure ()
      , configSection = ""
      , parseConfig = \_conf _value -> Right ()
      , doInitialize = do
          initServer reactorChan argOptions logger
      , -- TODO: Do handlers need to inspect clientCapabilities?
        staticHandlers = \_clientCapabilities ->
          mapHandlers goReq goNot $
            mconcat
              ( [ handleInitialized
                , handleChangeConfiguration
                , handleTextDocumentHoverRequest
                , handleDefinitionRequest
                , handleTypeDefinitionRequest
                , handleImplementationRequest
                , handleReferencesRequest
                , handleRenameRequest
                , handlePrepareRenameRequest
                , handleCancelNotification
                , handleDidOpen
                , handleDidChange
                , handleDidSave
                , handleDidClose
                , handleWorkspaceSymbol
                , handleSetTrace
                , handleCodeAction
                , handleResolveCodeAction
                , handleDocumentSymbols
                , handleCompletion
                , -- Currently disabled until we support configuration for the formatter
                  -- , handleFormat
                  handleCompletionItemResolve
                ]
                  <> (if argOptions.provideInlays then [handleInlayHintRequest argOptions, handleResolveInlayHint] else [])
              )
      , interpretHandler = \env -> Iso (LSP.runLspT env) liftIO
      , options = lspOptions
      , defaultConfig = ()
      }
 where
  catchAndLog m = do
    Exception.catchAny m $ \e ->
      logException e
  logException e = do
    LSP.Logging.logToLogMessage Colog.<& Colog.WithSeverity (T.pack (show e)) Colog.Error

#if MIN_VERSION_lsp(2,7,0)
respondWithError :: forall {f :: LSP.MessageDirection} {a}
                                 {m :: Method f LSP.Request} {b1} {b2}.
                          Show a =>
                          (Either (LSP.TResponseError m) b1 -> b2) -> a -> b2
respondWithError res e = res $ Left $ LSP.TResponseError (InR ErrorCodes_InvalidRequest) ("An error was caught: " <> T.pack (show e)) Nothing
#else
respondWithError :: Show a => (Either LSP.ResponseError b1 -> b2) -> a -> b2
respondWithError res e = res $ Left $ LSP.ResponseError (InR ErrorCodes_InvalidRequest) ("An error was caught: " <> T.pack (show e)) Nothing
#endif

lspOptions :: LSP.Options
lspOptions =
  LSP.defaultOptions
    { LSP.optTextDocumentSync =
        Just
          LSP.TextDocumentSyncOptions
            { LSP._openClose = Just True
            , LSP._change = Just LSP.TextDocumentSyncKind_Incremental
            , LSP._willSave = Just False
            , LSP._willSaveWaitUntil = Just False
            , LSP._save =
                Just $
                  InR $
                    LSP.SaveOptions
                      { LSP._includeText = Just False
                      }
            }
    , LSP.optCompletionTriggerCharacters = Just ['.']
    }

runServer :: StaticEnvOptions -> LoggerM IO -> IO Int
runServer argOptions logger = do
  server <- serverDef argOptions logger
  LSP.runServer server
