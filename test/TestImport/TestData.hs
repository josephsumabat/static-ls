{-# LANGUAGE DuplicateRecordFields #-}

module TestImport.TestData where

import qualified Language.LSP.Protocol.Types as LSP
import qualified System.Directory as Dir

myFunDefTdiAndPosition :: (LSP.TextDocumentIdentifier, LSP.Position)
myFunDefTdiAndPosition =
    let pos =
            LSP.Position
                { LSP._line = unsafeIntToUInt 4
                , LSP._character = unsafeIntToUInt 0
                }
        tdi =
            LSP.TextDocumentIdentifier $ LSP.filePathToUri "test/TestData/Mod2.hs"
     in (tdi, pos)

myFunDefDefinitionLink :: IO LSP.DefinitionLink
myFunDefDefinitionLink = do
    LSP.Location{..} <- myFunDefLocation
    pure . LSP.DefinitionLink $
        LSP.LocationLink
            { _originSelectionRange = Nothing
            , _targetUri = _uri
            , _targetRange = _range
            , _targetSelectionRange = _range
            }

myFunDefLocation :: IO LSP.Location
myFunDefLocation = do
    absDir <- Dir.makeAbsolute "test/TestData/Mod2.hs"
    pure $
        LSP.Location
            { LSP._uri = LSP.filePathToUri absDir
            , LSP._range =
                LSP.Range
                    { _start =
                        LSP.Position
                            { LSP._line = 4
                            , LSP._character = 0
                            }
                    , LSP._end =
                        LSP.Position
                            { LSP._line = 4
                            , LSP._character = 5
                            }
                    }
            }

myFunRef1TdiAndPosition :: (LSP.TextDocumentIdentifier, LSP.Position)
myFunRef1TdiAndPosition =
    let pos =
            LSP.Position
                { LSP._line = unsafeIntToUInt 10
                , LSP._character = unsafeIntToUInt 18
                }
        tdi =
            LSP.TextDocumentIdentifier $ LSP.filePathToUri "test/TestData/Mod1.hs"
     in (tdi, pos)

unsafeIntToUInt :: Int -> LSP.UInt
unsafeIntToUInt = fromIntegral
