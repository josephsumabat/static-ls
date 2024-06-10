{-# LANGUAGE DuplicateRecordFields #-}

module TestImport.TestData where

import Data.LineColRange (LineColRange (..))
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (LineCol (..))
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.FileWith (FileLcRange, FileWith (..))
import System.Directory qualified as Dir

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

-- myFunDefDefinitionLink :: IO LSP.DefinitionLink
-- myFunDefDefinitionLink = do
--   LSP.Location {..} <- myFunDefLocation
--   pure . LSP.DefinitionLink $
--     LSP.LocationLink
--       { _originSelectionRange = Nothing
--       , _targetUri = _uri
--       , _targetRange = _range
--       , _targetSelectionRange = _range
--       }

myFunDefLocation :: IO FileLcRange
myFunDefLocation = do
  absPath <- Path.filePathToAbs "test/TestData/Mod2.hs"
  pure $
    FileWith
      absPath
      LineColRange
        { start = LineCol 10 0
        , end = LineCol 10 5
        }

-- pure $
--   LSP.Location
--     { LSP._uri = LSP.filePathToUri absDir,
--       LSP._range =
--         LSP.Range
--           { _start =
--               LSP.Position
--                 { LSP._line = 10,
--                   LSP._character = 0
--                 },
--             LSP._end =
--               LSP.Position
--                 { LSP._line = 10,
--                   LSP._character = 5
--                 }
--           }
--     }

myFunRef1TdiAndPosition :: IO (AbsPath, LineCol)
myFunRef1TdiAndPosition = do
  path <- Path.filePathToAbs "test/TestData/Mod1.hs"
  let pos = LineCol 10 18
  pure (path, pos)

unsafeIntToUInt :: Int -> LSP.UInt
unsafeIntToUInt = fromIntegral
