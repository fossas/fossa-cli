{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Fossa.API.Types
  ( ApiKey (..),
    ApiOpts (..),
    useApiOpts,
    Issues (..),
    IssueRule (..),
    IssueType (..),
    Issue (..),
  )
where

import Control.Effect.Diagnostics hiding (fromMaybe)
import Data.Aeson
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req
import Text.URI (URI, render)
import qualified Unsafe.Coerce as Unsafe
import Text.URI.QQ (uri)

newtype ApiKey = ApiKey {unApiKey :: Text}
  deriving (Eq, Ord, Show)

data ApiOpts = ApiOpts
  { apiOptsUri :: Maybe URI,
    apiOptsApiKey :: ApiKey
  }
  deriving (Eq, Ord, Show)

data Issues = Issues
  { issuesCount :: Int
  , issuesIssues :: [Issue]
  , issuesStatus :: Text
  } deriving (Eq, Ord, Show)

data IssueType
  = IssuePolicyConflict
  | IssuePolicyFlag
  | IssueVulnerability
  | IssueUnlicensedDependency
  | IssueOutdatedDependency
  | IssueOther Text
  deriving (Eq, Ord, Show)

renderIssueType :: IssueType -> Text
renderIssueType = \case
  IssuePolicyConflict -> "Denied by Policy"
  IssuePolicyFlag -> "Flagged by Policy"
  IssueVulnerability -> "Vulnerability"
  IssueUnlicensedDependency -> "Unlicensed Dependency"
  IssueOutdatedDependency -> "Outdated Dependency"
  IssueOther other -> other

data Issue = Issue
  { issueId :: Int
  , issuePriorityString :: Maybe Text -- we only use this field for `fossa test --json`
  , issueResolved :: Bool
  , issueRevisionId :: Text
  , issueType :: IssueType
  , issueRule :: Maybe IssueRule
  } deriving (Eq, Ord, Show)

newtype IssueRule = IssueRule
  { ruleLicenseId :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromJSON Issues where
  parseJSON = withObject "Issues" $ \obj ->
    Issues <$> obj .: "count"
           <*> obj .:? "issues" .!= []
           <*> obj .: "status"

instance ToJSON Issues where
  toJSON Issues{..} = object
    [ "count" .= issuesCount
    , "issues" .= issuesIssues
    , "status" .= issuesStatus
    ]

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \obj ->
    Issue <$> obj .: "id"
           <*> obj .:? "priorityString"
           <*> obj .: "resolved"
           -- VPS issues don't have a revisionId
           <*> obj .:? "revisionId" .!= "unknown project"
           <*> obj .: "type"
           <*> obj .:? "rule"

instance ToJSON Issue where
  toJSON Issue{..} = object
    [ "id" .= issueId
    , "priorityString" .= issuePriorityString
    , "resolved" .= issueResolved
    , "revisionId" .= issueRevisionId
    , "type" .= issueType
    , "rule" .= issueRule
    ]

instance FromJSON IssueType where
  parseJSON = withText "IssueType" $ \case
    "policy_conflict" -> pure IssuePolicyConflict
    "policy_flag" -> pure IssuePolicyFlag
    "vulnerability" -> pure IssueVulnerability
    "unlicensed_dependency" -> pure IssueUnlicensedDependency
    "outdated_dependency" -> pure IssueOutdatedDependency
    other -> pure (IssueOther other)

instance ToJSON IssueType where
  toJSON = String . \case
    IssuePolicyConflict -> "policy_conflict"
    IssuePolicyFlag -> "policy_flag"
    IssueVulnerability -> "vulnerability"
    IssueUnlicensedDependency -> "unlicensed_dependency"
    IssueOutdatedDependency -> "outdated_dependency"
    IssueOther text -> text

instance FromJSON IssueRule where
  parseJSON = withObject "IssueRule" $ \obj ->
    IssueRule <$> obj .:? "licenseId"

instance ToJSON IssueRule where
  toJSON IssueRule{..} = object ["licenseId" .= ruleLicenseId]

instance Pretty Issues where
  pretty = renderedIssues

---------------

renderedIssues :: Issues -> Doc ann
renderedIssues issues = rendered
  where
    padding :: Int
    padding = 20

    issuesList :: [Issue]
    issuesList = issuesIssues issues

    categorize :: Ord k => (v -> k) -> [v] -> Map k [v]
    categorize f = M.fromListWith (++) . map (\v -> (f v, [v]))

    issuesByType :: Map IssueType [Issue]
    issuesByType = categorize issueType issuesList

    renderSection :: IssueType -> [Issue] -> Doc ann
    renderSection issueType rawIssues =
      renderHeader issueType <> line <> vsep (map renderIssue rawIssues) <> line

    rendered :: Doc ann
    rendered = vsep
      [renderSection issueType rawIssues | (issueType,rawIssues) <- M.toList issuesByType]

    renderHeader :: IssueType -> Doc ann
    renderHeader ty = vsep
      [ "========================================================================"
      , pretty $ renderIssueType ty
      , "========================================================================"
      , hsep $ map (fill padding) $ case ty of
          IssuePolicyConflict -> ["Dependency", "Revision", "License"]
          IssuePolicyFlag -> ["Dependency", "Revision", "License"]
          _ -> ["Dependency", "Revision"]
      , ""
      ]

    renderIssue :: Issue -> Doc ann
    renderIssue issue = hsep (map format [name, revision, license])
      where
        format :: Text -> Doc ann
        format = fill padding . pretty

        locatorSplit = T.split (\c -> c == '$' || c == '+') (issueRevisionId issue)

        name = fromMaybe (issueRevisionId issue) (locatorSplit !? 1)
        revision = fromMaybe "" (locatorSplit !? 2)
        license = fromMaybe "" (ruleLicenseId =<< issueRule issue)

        (!?) :: [a] -> Int -> Maybe a
        xs !? ix
          | length xs <= ix = Nothing
          | otherwise = Just (xs !! ix)


-- | parse a URI for use as a base Url, along with some default options (auth, port, ...)
useApiOpts :: Has Diagnostics sig m => ApiOpts -> m (Url 'Https, Option 'Https)
useApiOpts opts = case useURI serverURI of
  Nothing -> fatalText ("Invalid URL: " <> render serverURI)
  -- Url is "type role Url nominal" in the scheme (Http/Https), so we have to
  -- unsafeCoerce @Url 'Http@ into @Url 'Https@. Options isn't nominal in the
  -- scheme, so we can coerce as usual.
  Just (Left (url, options)) -> pure (Unsafe.unsafeCoerce url, coerce options <> authHeader (apiOptsApiKey opts))
  Just (Right (url, options)) -> pure (url, options <> authHeader (apiOptsApiKey opts))
  where 
    serverURI = fromMaybe [uri|https://app.fossa.com|] (apiOptsUri opts)

authHeader :: ApiKey -> Option 'Https
authHeader key = header "Authorization" (encodeUtf8 ("Bearer " <> unApiKey key))
