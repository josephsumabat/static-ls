module StaticLS.IDE.Rename (
  rename,
)
where

import Data.Edit qualified as Edit
import Data.Path (AbsPath)
import Data.Pos (LineCol (..))
import Data.Text (Text)
import Data.Traversable (for)
import StaticLS.IDE.FileWith (FileWith (..))
import StaticLS.IDE.References qualified as References
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.Monad

rename :: AbsPath -> LineCol -> Text -> StaticLsM SourceEdit
rename path lineCol newName = do
  refs <- References.findRefsPos path lineCol
  sourceEdits <- for refs \ref -> do
    let edit = Edit.replace ref.loc newName
    pure $ SourceEdit.single ref.path edit
  sourceEdit <- pure $ mconcat sourceEdits
  pure sourceEdit
