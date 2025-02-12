{-# LANGUAGE TemplateHaskell #-}

module App.Configuration where

import Colog.Core qualified as Colog
import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Text qualified as T
import StaticLS.Logger
import StaticLS.StaticEnv.Options
import System.Directory
import System.Directory.Internal.Prelude

data StaticEnvJson = StaticEnvJson
  { hiedbPath :: Maybe FilePath
  , hieFilePath :: Maybe [FilePath]
  , hiFilePath :: Maybe FilePath
  , srcFilePath :: Maybe [FilePath]
  }

$(deriveJSON defaultOptions ''StaticEnvJson)

data ConfigResult = ConfigResult
  { fileName :: String
  , result :: Either String StaticEnvOptions
  }

getFileConfig :: (HasCallStack) => Colog.LogAction IO Msg -> IO (Maybe StaticEnvOptions)
getFileConfig logger = do
  localConfig <- readConfig "static-ls.local.json"
  globalConfig <- readConfig "static-ls.json"
  let eFileConfig = localConfig <|> globalConfig
  case eFileConfig of
    (Just (Left e)) -> do
      logWithLogger logger Colog.Error (T.pack e) callStack
      exitFailure
    (Just (Right f)) -> pure (Just f)
    Nothing -> pure Nothing

readConfig :: FilePath -> IO (Maybe (Either String StaticEnvOptions))
readConfig fileName = do
  configExists <- doesFileExist fileName
  if configExists
    then do
      res <- eitherDecodeFileStrict fileName
      pure $
        Just $
          either
            (\e -> Left $ toHumanError e fileName)
            (Right . toOptions)
            res
    else pure Nothing
 where
  toHumanError :: String -> String -> String
  toHumanError jsonParseError fileName =
    "Error parsing configuration file"
      <> "\n  File: "
      <> fileName
      <> "\n  Error: "
      <> jsonParseError

toOptions :: StaticEnvJson -> StaticEnvOptions
toOptions jsonOptions =
  defaultStaticEnvOptions
    { optionHieDbPath = fromMaybe defaultStaticEnvOptions.optionHieDbPath jsonOptions.hiedbPath
    , optionHieDirs = fromMaybe defaultStaticEnvOptions.optionHieDirs jsonOptions.hieFilePath
    , optionHiFilesPath = fromMaybe defaultStaticEnvOptions.optionHiFilesPath jsonOptions.hiFilePath
    , optionSrcDirs = fromMaybe defaultStaticEnvOptions.optionSrcDirs jsonOptions.srcFilePath
    }
