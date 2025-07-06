module StaticLS.IDE.CodeActions.UseValidHoleFit where

import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.CodeActions.Utils (quickFix)

codeAction :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> T.Text -> LSP.CodeAction
codeAction = useValidHoleFit

useValidHoleFit :: LSP.TextDocumentIdentifier -> LSP.Diagnostic -> T.Text -> LSP.CodeAction
useValidHoleFit tdi diag sym =
  quickFix tdi diag ("Valid hole fit: " <> sym) (diag._range) sym
