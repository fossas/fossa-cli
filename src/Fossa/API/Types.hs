{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Fossa.API.Types (
  ApiKey (..),
  ApiOpts (..),
  Archive (..),
  ArchiveComponents (..),
  Build (..),
  BuildStatus (..),
  BuildTask (..),
  Contributors (..),
  Issue (..),
  IssueRule (..),
  IssueType (..),
  Issues (..),
  OrgId (..),
  Organization (..),
  Project (..),
  SignedURL (..),
  UploadResponse (..),
  ScanId (..),
  ScanResponse (..),
  useApiOpts,
  defaultApiPollDelay,
) where

import Control.Effect.Diagnostics (Diagnostics, Has, fatalText)
import Control.Timeout (Duration (Seconds))
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  Value (String),
  object,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Coerce (coerce)
import Data.List.Extra ((!?))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String.Conversion (encodeUtf8)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Req (
  Option,
  Scheme (Https),
  Url,
  header,
  useURI,
 )
import Prettyprinter (
  Doc,
  Pretty (pretty),
  fill,
  hsep,
  line,
  vsep,
 )
import Srclib.Types (Locator, parseLocator)
import Text.URI (URI, render)
import Text.URI.QQ (uri)
import Types (ArchiveUploadType (..))
import Unsafe.Coerce qualified as Unsafe

newtype ApiKey = ApiKey {unApiKey :: Text}
  deriving (Eq, Ord)

-- We manually define these instances so that we never leak keys in debug/telemetry.
instance Show ApiKey where
  show _ = "<REDACTED>"

instance ToJSON ApiKey where
  toJSON = toJSON . show

data ApiOpts = ApiOpts
  { apiOptsUri :: Maybe URI
  , apiOptsApiKey :: ApiKey
  , apiOptsPollDelay :: Duration
  }
  deriving (Eq, Ord, Show)

instance ToJSON ApiOpts where
  toJSON ApiOpts{..} =
    object
      [ "uri" .= show apiOptsUri
      , "apiKey" .= apiOptsApiKey
      ]

defaultApiPollDelay :: Duration
defaultApiPollDelay = Seconds 8

newtype SignedURL = SignedURL
  { signedURL :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON SignedURL where
  parseJSON = withObject "SignedUrl" $ \obj ->
    SignedURL <$> obj .: "signedUrl"

data ArchiveComponents = ArchiveComponents
  { archives :: [Archive]
  , forceRebuild :: Bool
  }
  deriving (Eq, Ord, Show)

instance ToJSON ArchiveComponents where
  toJSON ArchiveComponents{..} =
    object
      [ "archives" .= archives
      , "forceRebuild" .= forceRebuild
      ]

data Archive = Archive
  { archiveName :: Text
  , archiveVersion :: Text
  }
  deriving (Eq, Ord, Show)

instance ToJSON Archive where
  toJSON Archive{..} =
    object
      [ "packageSpec" .= archiveName
      , "revision" .= archiveVersion
      ]

data BuildStatus
  = StatusSucceeded
  | StatusFailed
  | StatusCreated
  | StatusAssigned
  | StatusRunning
  | StatusUnknown Text
  deriving (Eq, Ord, Show)

data Build = Build
  { buildId :: Int
  , buildError :: Maybe Text
  , buildTask :: BuildTask
  }
  deriving (Eq, Ord, Show)

newtype BuildTask = BuildTask
  { buildTaskStatus :: BuildStatus
  }
  deriving (Eq, Ord, Show)

instance FromJSON Build where
  parseJSON = withObject "Build" $ \obj ->
    Build
      <$> obj .: "id"
      <*> obj .:? "error"
      <*> obj .: "task"

instance FromJSON BuildTask where
  parseJSON = withObject "BuildTask" $ \obj ->
    BuildTask <$> obj .: "status"

instance FromJSON BuildStatus where
  parseJSON = withText "BuildStatus" $ \case
    "SUCCEEDED" -> pure StatusSucceeded
    "FAILED" -> pure StatusFailed
    "CREATED" -> pure StatusCreated
    "ASSIGNED" -> pure StatusAssigned
    "RUNNING" -> pure StatusRunning
    other -> pure $ StatusUnknown other

newtype Contributors = Contributors
  {unContributors :: Map Text Text}
  deriving (Eq, Ord, Show, ToJSON)

data Issues = Issues
  { issuesCount :: Int
  , issuesIssues :: [Issue]
  , issuesStatus :: Text
  }
  deriving (Eq, Ord, Show)

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
  }
  deriving (Eq, Ord, Show)

newtype IssueRule = IssueRule
  { ruleLicenseId :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON Issues where
  parseJSON = withObject "Issues" $ \obj ->
    Issues
      <$> obj .: "count"
      <*> obj .:? "issues" .!= []
      <*> obj .: "status"

instance ToJSON Issues where
  toJSON Issues{..} =
    object
      [ "count" .= issuesCount
      , "issues" .= issuesIssues
      , "status" .= issuesStatus
      ]

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \obj ->
    Issue
      <$> obj .: "id"
      <*> obj .:? "priorityString"
      <*> obj .: "resolved"
      -- VPS issues don't have a revisionId
      <*> obj .:? "revisionId" .!= "unknown project"
      <*> obj .: "type"
      <*> obj .:? "rule"

instance ToJSON Issue where
  toJSON Issue{..} =
    object
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
  toJSON =
    String . \case
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

newtype OrgId = OrgId Int
  deriving (Eq, Ord, FromJSON, ToJSON)

instance Show OrgId where
  show (OrgId orgId) = show orgId

data Organization = Organization
  { organizationId :: OrgId
  , orgUsesSAML :: Bool
  , orgCoreSupportsLocalLicenseScan :: Bool
  , orgSupportsAnalyzedRevisionsQuery :: Bool
  , orgDefaultVendoredDependencyScanType :: ArchiveUploadType
  , orgSupportsIssueDiffs :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
    Organization
      <$> obj .: "organizationId"
      <*> obj .:? "usesSAML" .!= False
      <*> obj .:? "supportsCliLicenseScanning" .!= False
      <*> obj .:? "supportsAnalyzedRevisionsQuery" .!= False
      <*> obj .:? "defaultVendoredDependencyScanType" .!= CLILicenseScan
      <*> obj .:? "supportsIssueDiffs" .!= False

data Project = Project
  { projectId :: Text
  , projectTitle :: Text
  , projectIsMonorepo :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \obj ->
    Project
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "isMonorepo"

data UploadResponse = UploadResponse
  { uploadLocator :: Locator
  , uploadError :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON UploadResponse where
  parseJSON = withObject "UploadResponse" $ \obj ->
    UploadResponse
      <$> (parseLocator <$> obj .: "locator")
      <*> obj .:? "error"

newtype ScanId = ScanId Text deriving (Eq, Ord, FromJSON, ToJSON)

instance Show ScanId where
  show (ScanId scanId) = show scanId

data ScanResponse = ScanResponse
  { responseScanId :: ScanId
  , responseScanStatus :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse
      <$> obj .: "id"
      <*> obj .:? "status"

---------------

renderedIssues :: Issues -> Doc ann
renderedIssues issues = rendered
  where
    padding :: Int
    padding = 20

    issuesList :: [Issue]
    issuesList = issuesIssues issues

    categorize :: Ord k => (v -> k) -> [v] -> Map k [v]
    categorize f = Map.fromListWith (++) . map (\v -> (f v, [v]))

    issuesByType :: Map IssueType [Issue]
    issuesByType = categorize issueType issuesList

    renderSection :: IssueType -> [Issue] -> Doc ann
    renderSection issueType rawIssues =
      renderHeader issueType <> line <> vsep (map renderIssue rawIssues) <> line

    rendered :: Doc ann
    rendered =
      vsep
        [renderSection issueType rawIssues | (issueType, rawIssues) <- Map.toList issuesByType]

    renderHeader :: IssueType -> Doc ann
    renderHeader ty =
      vsep
        [ "========================================================================"
        , pretty $ renderIssueType ty
        , "========================================================================"
        , hsep $
            map (fill padding) $ case ty of
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

        locatorSplit = Text.split (\c -> c == '$' || c == '+') (issueRevisionId issue)

        name = fromMaybe (issueRevisionId issue) (locatorSplit !? 1)
        revision = fromMaybe "" (locatorSplit !? 2)
        license = fromMaybe "" (ruleLicenseId =<< issueRule issue)

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
