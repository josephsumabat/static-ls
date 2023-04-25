{-# LANGUAGE DerivingStrategies #-}

module StaticLS.HIE.File.Except where

import Control.Exception
import Data.Aeson
import qualified Data.Aeson.KeyMap as Aeson
import Data.Text (pack)
import qualified GHC.Iface.Ext.Binary as GHC
import qualified Language.LSP.Types as LSP
import StaticLS.Server.Except

data HieFileReadException
    = HieFileReadException FilePath
    | HieFileVersionException FilePath Integer GHC.HieHeader
    deriving stock (Show, Eq)

instance Exception HieFileReadException

instance RenderableLspException HieFileReadException where
    renderLspException (HieFileReadException fp) =
        let msg = "Could not parse hie file"
            xdata =
                toJSON $
                    Aeson.fromList
                        [ ("hie_filename", fp)
                        ]
         in LSP.ResponseError
                { LSP._code = LSP.RequestFailed
                , LSP._message = pack msg
                , LSP._xdata = Just xdata
                }
    renderLspException (HieFileVersionException fp expected (got, ghcVer)) =
        let msg =
                "hie version mismatch on read"
            xdata =
                toJSON $
                    Aeson.fromList
                        [ ("expected", String . pack . show $ expected)
                        , ("got", String . pack . show $ got)
                        , ("compiled_on_ghc_ver", String . pack . show $ ghcVer)
                        , ("hie_filename", String . pack $ fp)
                        ]
         in LSP.ResponseError
                { LSP._code = LSP.RequestFailed
                , LSP._message = pack msg
                , LSP._xdata = Nothing
                }
