module StaticLS.Hir.Make where

import AST qualified
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import StaticLS.Hir.Types

mkName :: Text -> Bool -> Bool -> Name
mkName name isOperator isConstructor =
  Name
    { node = AST.defaultNode {AST.nodeText = name}
    , isOperator
    , isConstructor
    }

mkModuleText :: NonEmpty Text -> ModuleText
mkModuleText parts =
  ModuleText
    { parts
    , text = T.intercalate "." (NE.toList parts)
    }

-- mkModuleName :: NonEmpty Text -> ModuleName
-- mkModuleName parts =
--  ModuleName
--    { mod = mkModuleText parts
--    , node = AST.defaultNode
--    }
