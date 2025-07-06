module StaticLS.IDE.CodeActions.Parse (
  ActionableIssue (..),
  Ignored (..),
  KnownExtension (..),
  actionableIssue,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard, (<=<))
import Data.Foldable (asum, toList)
import Data.Functor (($>))
import Data.Text qualified as T
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

data ActionableIssue
    = RequiredExtension (Ignored LSP.Diagnostic) KnownExtension
    | TypedHoleFits (Ignored LSP.Diagnostic) [T.Text]
    | MissingFields (Ignored LSP.Diagnostic) T.Text (Maybe T.Text) [T.Text]
    | MissingMethods (Ignored LSP.Diagnostic) [T.Text]
    | MissingAssociatedType (Ignored LSP.Diagnostic) T.Text
    | MissingCasses (Ignored LSP.Diagnostic) [T.Text]
    deriving (Eq, Ord, Show)

actionableIssue :: LSP.Diagnostic -> Maybe ActionableIssue
actionableIssue diag =
    asum
        [ checkRequiredExtensions
        , checkValidHoleFits
        , checkMissingFields
        , checkMissingCases
        , checkMissingMethods
        , checkMissingAssociatedType
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
        (ctor, ext, flds) <- requiredStrictFields message <|> fieldsNotInitialized message
        Just $ MissingFields (Ignored diag) (getNormalText ctor) (fmap getNormalText ext) (map getNormalText flds)

    checkMissingCases :: Maybe ActionableIssue
    checkMissingCases = MissingCasses (Ignored diag) . map getNormalText <$> nonExhaustivePatterns message

    checkValidHoleFits :: Maybe ActionableIssue
    checkValidHoleFits = TypedHoleFits (Ignored diag) . map getNormalText <$> validHoleFits message

    checkMissingMethods :: Maybe ActionableIssue
    checkMissingMethods = MissingMethods (Ignored diag) . map getNormalText <$> missingMethods message

    checkMissingAssociatedType :: Maybe ActionableIssue
    checkMissingAssociatedType = MissingAssociatedType (Ignored diag) . getNormalText <$> missingAssociatedType message

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
validHoleFits = fmap (captures " " ident " ::\\>") . between "Valid hole fits include" "\\| "

missingMethods :: NormalText -> Maybe [NormalText]
missingMethods = fmap (captures "‘" ident "’") . between "No explicit implementation for " " • In the instance declaration"

missingAssociatedType :: NormalText -> Maybe NormalText
missingAssociatedType = capture "No explicit associated type or default declaration for ‘" ident "’"

nonExhaustivePatterns :: NormalText -> Maybe [NormalText]
nonExhaustivePatterns = fmap (captures " " (ident <> "( _)*") " ") . between "not matched:" " \\| "

fieldsNotInitialized :: NormalText -> Maybe (NormalText, Maybe NormalText, [NormalText])
fieldsNotInitialized t1 = do
    (_, _, t2) <- cut "Fields of ‘" t1
    (constructor, _, t3) <- cut "’ not initialised: " t2
    (fieldsSection, _, t4) <- cut " • In the expression: " t3
    let missingFields = captures " " ident " :: " fieldsSection
    braces <- between "\\{" "\\}" t4
    let existingFields = if T.null (getNormalText braces) then Nothing else Just braces
    pure ( constructor, existingFields, missingFields)

requiredStrictFields :: NormalText -> Maybe (NormalText, Maybe NormalText, [NormalText])
requiredStrictFields t1 = do
    (_, _, t2) <- cut "Constructor ‘" t1
    (constructor, _, t3) <- cut "’ does not have the required strict field\\(s\\):" t2
    (fieldsSection, _, t4) <- cut "• In the expression:" t3
    let missingFields = captures " " ident " ::" fieldsSection
    braces <- between "\\{" "\\}" t4
    let existingFields = if T.null (getNormalText braces) then Nothing else Just braces
    pure ( constructor, existingFields, missingFields)
