module StaticLS.IDE.CodeActions.Parse (
  VariableNotInScopeFixes (..),
  ActionableIssue (..),
  Ignored (..),
  KnownExtension (..),
  actionableIssue,
  missingFields,
  normalize,
  nonExhaustivePatterns,
  between,
  ident,
  getNormalText,
  validHoleFits,
  missingMethods,
  missingAssociatedType,
  variableNotInScope,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard, (<=<))
import Data.Foldable (asum, toList)
import Data.Functor (($>), (<&>))
import Data.Text qualified as T
import Debug.Trace
import Language.Haskell.TH.LanguageExtensions (Extension)
import Language.LSP.Protocol.Types qualified as LSP
import Text.Regex.TDFA (getAllTextMatches, (=~), (=~~))

-- This allows us to ignore diagnostics when comparing `ActionableIssue`s.
-- Otherwise, we end up with duplicates with slightly different wording.
newtype Ignored a = Ignored a deriving (Show)
instance Eq (Ignored a) where (==) _ _ = True
instance Ord (Ignored a) where compare _ _ = EQ

data KnownExtension = KnownExtension {-# UNPACK #-} !Extension {-# UNPACK #-} !T.Text
  deriving (Eq, Ord, Show)

{-# NOINLINE knownExtensions #-}
knownExtensions :: [KnownExtension]
knownExtensions = (\ext -> KnownExtension ext $ T.pack $ show ext) <$> [minBound .. maxBound]

data VariableNotInScopeFixes
  = MissSpelled T.Text T.Text
  | AddToImportList T.Text T.Text
  deriving
    ( Eq
    , Ord
    , Show
    )

data ActionableIssue
  = RequiredExtension (Ignored LSP.Diagnostic) KnownExtension
  | TypedHoleFits (Ignored LSP.Diagnostic) [T.Text]
  | MissingFields (Ignored LSP.Diagnostic) T.Text (Maybe T.Text) [T.Text]
  | MissingMethods (Ignored LSP.Diagnostic) [T.Text]
  | MissingAssociatedType (Ignored LSP.Diagnostic) T.Text
  | MissingCases (Ignored LSP.Diagnostic) [T.Text]
  | VariableNotInScope (Ignored LSP.Diagnostic) [VariableNotInScopeFixes]
  deriving
    ( Eq
    , Ord
    , Show
    )

actionableIssue :: LSP.Diagnostic -> Maybe ActionableIssue
actionableIssue diag =
  asum
    [ checkRequiredExtensions
    , checkValidHoleFits
    , checkMissingFields
    , checkMissingCases
    , checkMissingMethods
    , checkMissingAssociatedType
    , checkVariableNotInScope
    ]
 where
  message :: NormalText
  message = normalize diag._message

  checkRequiredExtensions :: Maybe ActionableIssue
  checkRequiredExtensions =
    let checkExt ext@(KnownExtension _ text) =
          guard (T.isInfixOf text $ getNormalText message) $> RequiredExtension (Ignored diag) ext
     in asum $ map checkExt knownExtensions

  checkMissingFields :: Maybe ActionableIssue
  checkMissingFields = do
    (ctor, ext, flds) <- missingFields message
    Just $ MissingFields (Ignored diag) (getNormalText ctor) (fmap getNormalText ext) (map getNormalText flds)

  checkMissingCases :: Maybe ActionableIssue
  checkMissingCases = MissingCases (Ignored diag) . map getNormalText <$> nonExhaustivePatterns message

  checkValidHoleFits :: Maybe ActionableIssue
  checkValidHoleFits = TypedHoleFits (Ignored diag) . map getNormalText <$> validHoleFits message

  checkMissingMethods :: Maybe ActionableIssue
  checkMissingMethods = MissingMethods (Ignored diag) . map getNormalText <$> missingMethods message

  checkMissingAssociatedType :: Maybe ActionableIssue
  checkMissingAssociatedType = MissingAssociatedType (Ignored diag) . getNormalText <$> missingAssociatedType message

  checkVariableNotInScope :: Maybe ActionableIssue
  checkVariableNotInScope = VariableNotInScope (Ignored diag) <$> variableNotInScope message

newtype NormalText = Normal T.Text
  deriving (Eq, Ord, Show)

getNormalText :: NormalText -> T.Text
getNormalText (Normal txt) = txt

normalize :: T.Text -> NormalText
normalize = Normal . T.unwords . T.words

capture :: T.Text -> T.Text -> T.Text -> NormalText -> Maybe NormalText
capture pfx pat sfx = fmap Normal . (=~~ pat) . getNormalText <=< between pfx sfx

captures :: T.Text -> T.Text -> T.Text -> NormalText -> [NormalText]
captures pfx pat sfx (Normal text) = do
  pfxCapSfx <- getAllTextMatches (text =~ T.concat [pfx, pat, sfx])
  cap <- toList (T.stripPrefix pfx =<< T.stripSuffix sfx pfxCapSfx)
  [Normal cap]

between :: T.Text -> T.Text -> NormalText -> Maybe NormalText
between pfx sfx text = do
  (_, _, afterPfx) <- cut pfx text
  (betweenPfxSfx, _, _) <- cut sfx afterPfx
  pure betweenPfxSfx

cut :: T.Text -> NormalText -> Maybe (NormalText, NormalText, NormalText)
cut pat (Normal txt) = (\(x, y, z) -> (Normal x, Normal y, Normal z)) <$> txt =~~ pat

ident :: T.Text
ident = "[0-9A-Z'\\._a-z]+"

validHoleFits :: NormalText -> Maybe [NormalText]
validHoleFits t1 = do
  (_, _, t2) <- cut "Valid hole fits include" t1
  Just $ captures " " ident " ::" t2

missingMethods :: NormalText -> Maybe [NormalText]
missingMethods t1 = do
  t2 <- between "No explicit implementation for" "In the instance declaration" t1
  let matches = captures "‘" "[^’]+" "’" t2
  Just $ fmap (addOperatorParentheses . removeModulePrefix) matches

removeModulePrefix :: NormalText -> NormalText
removeModulePrefix text =
  case cut "^.*\\." text of
    Just (_, _, suffix) -> suffix
    Nothing -> text

addOperatorParentheses :: NormalText -> NormalText
addOperatorParentheses (Normal text) =
  normalize $
    if text =~ ident
      then
        text
      else "(" <> text <> ")"

missingAssociatedType :: NormalText -> Maybe NormalText
missingAssociatedType = capture "No explicit associated type or default declaration for ‘" ident "’"

nonExhaustivePatterns :: NormalText -> Maybe [NormalText]
nonExhaustivePatterns t1 = do
  (_, _, (Normal t2)) <- cut "non-exhaustive.*not matched:" t1
  pure . fmap normalize . filter (/= "...") . getAllTextMatches $ t2 =~ (ident <> "( _)*")

missingFields :: NormalText -> Maybe (NormalText, Maybe NormalText, [NormalText])
missingFields t1 = do
  (_, _, t2) <- cut "(Fields of|Constructor) ‘" t1
  (constructor, _, t3) <- cut "’ (not initialised:|does not have[^:]*:)" t2
  (fieldsSection, _, t4) <- cut "•" t3
  let missingFields = captures " " ident " :: " fieldsSection
  braces <- between (getNormalText constructor <> " \\{") "\\}" t4
  let existingFields = if T.null (getNormalText braces) then Nothing else Just braces
  pure (constructor, existingFields, missingFields)

variableNotInScope :: NormalText -> Maybe [VariableNotInScopeFixes]
variableNotInScope diagTxt = do
  (_, _, t1) <- cut "Variable not in scope:" diagTxt

  notInScope <- capture " " ident " :: " t1
  -- TODO: properly get case of only one fix (Suggested fix vs Suggested fixes)
  textFixes <- traceShowId $ oneFix t1 <|> multipleFixes t1
  pure $
    traceShowId $
      concat $
        [ action
        | fix <- textFixes
        , Just action <- getFixesDecs notInScope <&> ($ fix)
        ]
 where
  oneFix txt = do
    (_, _, t1) <- cut "Suggested fix:" txt
    pure $ captures "Perhaps" "[^•]*" "" t1
  multipleFixes txt = do
    (_, _, t1) <- cut "Suggested fixes:" txt
    pure $ captures "• Perhaps" "[^•]*" "" t1

  getFixesDecs :: NormalText -> [NormalText -> Maybe [VariableNotInScopeFixes]]
  getFixesDecs notInScope =
    [ -- For matching: • Perhaps use ‘sum’ (imported from Prelude)
      \t -> do
        (_, _, t1) <- cut "use ‘" t
        (name, _, _) <- cut "’ " t1
        pure [MissSpelled (getNormalText notInScope) (getNormalText name)]
    , -- For matching:
      --  • Perhaps you want to add ‘asum’ to the import list
      --  in the import of ‘Data.Foldable’
      \t -> do
        (_, _, t1) <- cut "you want to add ‘" t
        (name, _, t2) <- cut "’ to the import list" t1
        (_, _, t3) <- cut "in the import of ‘" t2
        (mdl, _, _) <- cut "’" t3
        pure [AddToImportList (getNormalText name) (getNormalText mdl)]
    , -- For matching:
      --  • Perhaps you want to add ‘asum’ to one of these import lists:
      --      ‘Control.Applicative’ (…)
      --      ‘Data.Foldable’ (…)
      \t -> do
        (_, _, t1) <- cut "you want to add ‘" t
        (name, _, t2) <- cut "’ to one of these import lists:" t1
        let mdls = traceShowId $ captures "‘" ident "’" t2
        pure [AddToImportList (getNormalText name) (getNormalText mdl) | mdl <- mdls]
    ]
