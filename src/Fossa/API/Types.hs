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
  IssueSummaryRevision (..),
  IssuesSummary (..),
  IssueSummaryTarget (..),
  OrgId (..),
  Organization (..),
  Project (..),
  RevisionDependencyCache (..),
  RevisionDependencyCacheStatus (..),
  PathDependencyUpload (..),
  UploadedPathDependencyLocator (..),
  SignedURL (..),
  SignedURLWithKey (..),
  UploadResponse (..),
  PathDependencyUploadReq (..),
  PathDependencyFinalizeReq (..),
  AnalyzedPathDependenciesResp (..),
  AnalyzedPathDependency (..),
  TokenType (..),
  TokenTypeResponse (..),
  Subscription (..),
  CustomBuildUploadPermissions (..),
  ProjectPermissionStatus (..),
  ReleaseGroupPermissionStatus (..),
  useApiOpts,
  defaultApiPollDelay,
  blankOrganization,
) where

import App.Fossa.Lernie.Types (GrepEntry)
import App.Types (FullFileUploads (..), fullFileUploadsToCliLicenseScanType)
import Control.Applicative ((<|>))
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
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.List.Extra ((!?))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.String.Conversion (ToText, encodeUtf8, toText)
import Data.Text (Text, toLower, toUpper)
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
  line,
  vsep,
 )
import Srclib.Types (Locator, parseLocator)
import Text.URI (URI, render)
import Text.URI.QQ (uri)
import Types (ArchiveUploadType (..))
import Unsafe.Coerce qualified as Unsafe

--- | Data types of CLI owned endpoints in Core

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

data SignedURLWithKey = SignedURLWithKey
  { surlwkSignedURL :: Text
  , surlwkKey :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON SignedURL where
  parseJSON = withObject "SignedUrl" $ \obj ->
    SignedURL <$> obj .: "signedUrl"

instance FromJSON SignedURLWithKey where
  parseJSON = withObject "SignedUrl" $ \obj ->
    SignedURLWithKey <$> obj .: "signedUrl" <*> obj .: "key"

data ArchiveComponents = ArchiveComponents
  { archives :: [Archive]
  , forceRebuild :: Bool
  , fullFiles :: FullFileUploads
  }
  deriving (Eq, Ord, Show)

instance ToJSON ArchiveComponents where
  toJSON ArchiveComponents{..} =
    object
      [ "archives" .= archives
      , "forceRebuild" .= forceRebuild
      , "fullFiles" .= unFullFileUploads fullFiles
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
  , issuesSummary :: Maybe IssuesSummary
  }
  deriving (Eq, Ord, Show)

data IssuesSummary = IssuesSummary
  { revision :: IssueSummaryRevision
  , targets :: [IssueSummaryTarget]
  }
  deriving (Eq, Ord, Show)

data IssueSummaryRevision = IssueSummaryRevision
  { isrProjectTitle :: Text
  , isrProjectRevision :: Text
  , isrIsPublic :: Maybe Bool
  }
  deriving (Eq, Ord, Show)

data IssueSummaryTarget = IssueSummaryTarget
  { istTargetType :: Text
  , istTargetPaths :: [Text]
  }
  deriving (Eq, Show)

instance Ord IssueSummaryTarget where
  compare lhs rhs =
    if (istTargetType lhs == istTargetType rhs)
      then comparing istTargetPaths lhs rhs
      else comparing istTargetType lhs rhs

data IssueCategory
  = Security
  | Compliance
  | Other Text
  deriving (Eq, Ord, Show)

instance ToText IssueCategory where
  toText :: IssueCategory -> Text
  toText i =
    case i of
      Security -> "Security"
      Compliance -> "Compliance"
      Other t -> t

instance Pretty IssueCategory where
  pretty = pretty . toUpper . toText

issueTypeToCategory :: IssueType -> IssueCategory
issueTypeToCategory =
  \case
    IssueAbandonware -> Security
    IssueDenyListedDep -> Security
    IssueEmptyPackage -> Security
    IssueNativeCode -> Security
    IssueOutdatedDependency -> Security
    IssueVulnerability -> Security
    IssuePolicyConflict -> Compliance
    IssuePolicyFlag -> Compliance
    IssueUnlicensedAndPublicDep -> Compliance
    IssueUnlicensedDependency -> Compliance
    IssueOther t -> Other t

-- These constructors are ordered alphabetically.
-- This is needed for our `fossa test` output.
data IssueType
  = IssueAbandonware
  | IssueDenyListedDep
  | IssueEmptyPackage
  | IssueNativeCode
  | IssueOther Text
  | IssueOutdatedDependency
  | IssuePolicyConflict
  | IssuePolicyFlag
  | IssueUnlicensedAndPublicDep
  | IssueUnlicensedDependency
  | IssueVulnerability
  deriving (Eq, Ord, Show)

instance Pretty IssueType where
  pretty = \case
    IssueAbandonware -> "Abandoned Dependencies"
    IssueDenyListedDep -> "Denylisted Dependency"
    IssueNativeCode -> "Native Code Dependency"
    IssuePolicyConflict -> "Denied by Policy"
    IssuePolicyFlag -> "Flagged by Policy"
    IssueVulnerability -> "Vulnerability"
    IssueUnlicensedDependency -> "Unlicensed Dependency"
    IssueUnlicensedAndPublicDep -> "Unlicensed and Public Dependency"
    IssueOutdatedDependency -> "Outdated Dependency"
    IssueEmptyPackage -> "Empty Package"
    IssueOther other -> pretty other

data Issue = Issue
  { issueId :: Int
  , issuePriorityString :: Maybe Text -- we only use this field for `fossa test --json`
  , issueResolved :: Bool
  , issueRevisionId :: Text
  , issueType :: IssueType
  , issueRule :: Maybe IssueRule
  , issueLicense :: Maybe Text
  , issueDashURL :: Maybe Text
  , issueCVE :: Maybe Text
  , issueFixedIn :: Maybe Text
  }
  deriving (Eq, Ord, Show)

newtype IssueRule = IssueRule
  {ruleId :: Maybe Int}
  deriving (Eq, Ord, Show)

instance FromJSON Issues where
  parseJSON = withObject "Issues" $ \obj ->
    Issues
      <$> obj .: "count"
      <*> obj .:? "issues" .!= []
      <*> obj .: "status"
      <*> obj .:? "summary"

instance ToJSON Issues where
  toJSON Issues{..} =
    object
      [ "count" .= issuesCount
      , "issues" .= issuesIssues
      , "status" .= issuesStatus
      , "summary" .= issuesSummary
      ]

instance FromJSON IssuesSummary where
  parseJSON = withObject "IssuesSummary" $ \obj ->
    IssuesSummary
      <$> obj .: "revision"
      <*> obj .:? "targets" .!= []

instance ToJSON IssuesSummary where
  toJSON IssuesSummary{..} =
    object
      [ "revision" .= revision
      , "targets" .= targets
      ]

instance FromJSON IssueSummaryRevision where
  parseJSON = withObject "IssueSummaryRevision" $ \obj ->
    IssueSummaryRevision
      <$> obj .: "projectTitle"
      <*> obj .: "projectRevision"
      <*> obj .:? "isPublic"

instance ToJSON IssueSummaryRevision where
  toJSON IssueSummaryRevision{..} =
    object
      [ "projectTitle" .= isrProjectTitle
      , "projectRevision" .= isrProjectRevision
      , "isPublic" .= isrIsPublic
      ]

instance FromJSON IssueSummaryTarget where
  parseJSON = withObject "IssueSummaryTarget" $ \obj ->
    IssueSummaryTarget
      <$> obj .: "type"
      <*> obj .: "originPaths"

instance ToJSON IssueSummaryTarget where
  toJSON IssueSummaryTarget{..} =
    object
      [ "type" .= istTargetType
      , "originPaths" .= istTargetPaths
      ]

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \obj ->
    Issue
      <$> obj .: "id"
      <*> obj .:? "priorityString"
      <*> obj .: "resolved"
      <*> obj .:? "revisionId" .!= "unknown project"
      <*> obj .: "type"
      <*> obj .:? "rule"
      <*> obj .:? "license"
      <*> obj .:? "issueDashURL"
      <*> obj .:? "cve"
      <*> obj .:? "fixedIn"

instance ToJSON Issue where
  toJSON Issue{..} =
    object
      [ "id" .= issueId
      , "priorityString" .= issuePriorityString
      , "resolved" .= issueResolved
      , "revisionId" .= issueRevisionId
      , "type" .= issueType
      , "rule" .= issueRule
      , "license" .= issueLicense
      , "issueDashURL" .= issueDashURL
      , "cve" .= issueCVE
      , "fixedIn" .= issueFixedIn
      ]

instance FromJSON IssueType where
  parseJSON = withText "IssueType" $ \case
    "policy_conflict" -> pure IssuePolicyConflict
    "policy_flag" -> pure IssuePolicyFlag
    "vulnerability" -> pure IssueVulnerability
    "unlicensed_dependency" -> pure IssueUnlicensedDependency
    "outdated_dependency" -> pure IssueOutdatedDependency
    "risk_empty_package" -> pure IssueEmptyPackage
    "risk_native_code" -> pure IssueNativeCode
    "blacklisted_dependency" -> pure IssueDenyListedDep
    "unlicensed_and_public" -> pure IssueUnlicensedAndPublicDep
    other -> pure (IssueOther other)

instance ToJSON IssueType where
  toJSON =
    String . \case
      IssueAbandonware -> "risk_abandonware"
      IssueEmptyPackage -> "risk_empty_package"
      IssueDenyListedDep -> "blacklisted_dependency"
      IssueNativeCode -> "risk_native_code"
      IssueUnlicensedAndPublicDep -> "unlicensed_and_public"
      IssuePolicyConflict -> "policy_conflict"
      IssuePolicyFlag -> "policy_flag"
      IssueVulnerability -> "vulnerability"
      IssueUnlicensedDependency -> "unlicensed_dependency"
      IssueOutdatedDependency -> "outdated_dependency"
      IssueOther text -> text

instance FromJSON IssueRule where
  parseJSON = withObject "IssueRule" $ \obj ->
    IssueRule <$> obj .:? "ruleId"

instance ToJSON IssueRule where
  toJSON IssueRule{..} =
    object
      [ "ruleId" .= ruleId
      ]

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
  , orgSupportsNativeContainerScan :: Bool
  , orgSupportsDependenciesCachePolling :: Bool
  , orgRequiresFullFileUploads :: Bool
  , orgDefaultsToFirstPartyScans :: Bool
  , orgSupportsPathDependencyScans :: Bool
  , orgSupportsFirstPartyScans :: Bool
  , orgCustomLicenseScanConfigs :: [GrepEntry]
  , orgSupportsReachability :: Bool
  , orgSupportsPreflightChecks :: Bool
  , orgSubscription :: Subscription
  }
  deriving (Eq, Ord, Show)

blankOrganization :: Organization
blankOrganization =
  Organization
    { organizationId = OrgId 0
    , orgUsesSAML = False
    , orgCoreSupportsLocalLicenseScan = True
    , orgSupportsAnalyzedRevisionsQuery = True
    , orgDefaultVendoredDependencyScanType = CLILicenseScan
    , orgSupportsIssueDiffs = True
    , orgSupportsNativeContainerScan = True
    , orgSupportsDependenciesCachePolling = True
    , orgRequiresFullFileUploads = False
    , orgDefaultsToFirstPartyScans = False
    , orgSupportsPathDependencyScans = False
    , orgSupportsFirstPartyScans = True
    , orgCustomLicenseScanConfigs = []
    , orgSupportsReachability = False
    , orgSupportsPreflightChecks = False
    , orgSubscription = Free
    }

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
    Organization
      <$> obj .: "organizationId"
      <*> obj .:? "usesSAML" .!= False
      <*> obj .:? "supportsCliLicenseScanning" .!= False
      <*> obj .:? "supportsAnalyzedRevisionsQuery" .!= False
      <*> obj .:? "defaultVendoredDependencyScanType" .!= CLILicenseScan
      <*> obj .:? "supportsIssueDiffs" .!= False
      <*> obj .:? "supportsNativeContainerScans" .!= False
      <*> obj .:? "supportsDependenciesCachePolling" .!= False
      <*> obj .:? "requireFullFileUploads" .!= False
      <*> obj .:? "defaultToFirstPartyScans" .!= False
      <*> obj .:? "supportsPathDependency" .!= False
      <*> obj .:? "supportsFirstPartyScans" .!= False
      <*> obj .:? "customLicenseScanConfigs" .!= []
      <*> obj .:? "supportsReachability" .!= False
      <*> obj .:? "supportsPreflightChecks" .!= False
      <*> obj .:? "subscription" .!= Free

data TokenType
  = Push
  | FullAccess
  deriving (Eq, Ord, Show)

newtype TokenTypeResponse = TokenTypeResponse {tokenType :: TokenType}
  deriving (Eq, Ord, Show)

instance FromJSON TokenTypeResponse where
  parseJSON = withObject "TokenTypeResponse" $ \obj ->
    TokenTypeResponse
      <$> obj .: "tokenType"

instance FromJSON TokenType where
  parseJSON = withText "TokenType" $ \case
    "Push" -> pure Push
    _ -> pure FullAccess

data Subscription
  = Free
  | Premium
  deriving (Eq, Ord, Show)

instance FromJSON Subscription where
  parseJSON = withText "Subscription" $ \case
    "Premium" -> pure Premium
    _ -> pure Free

data CustomBuildUploadPermissions = CustomBuildUploadPermissions
  { projectPermissionStatus :: ProjectPermissionStatus
  , maybeReleaseGroupPermissionStatus :: Maybe ReleaseGroupPermissionStatus
  }
  deriving (Eq, Ord, Show)

data ProjectPermissionStatus = ValidProjectPermission | InvalidEditProjectPermission | InvalidCreateProjectPermission | InvalidCreateTeamProjectPermission | InvalidCreateProjectOnlyToTeamPermission
  deriving (Eq, Ord, Show)

data ReleaseGroupPermissionStatus = ValidReleaseGroupPermission | InvalidEditReleaseGroupPermission | InvalidCreateTeamProjectsForReleaseGroupPermission
  deriving (Eq, Ord, Show)

instance FromJSON CustomBuildUploadPermissions where
  parseJSON = withObject "CustomBuildUploadPermissions" $ \obj ->
    CustomBuildUploadPermissions
      <$> obj .: "projectPermissionStatus"
      <*> obj .:? "releaseGroupPermissionStatus"

instance FromJSON ProjectPermissionStatus where
  parseJSON = withText "ProjectPermissionStatus" $ \case
    "ValidProjectPermission" -> pure ValidProjectPermission
    "InvalidEditProjectPermission" -> pure InvalidEditProjectPermission
    "InvalidCreateProjectPermission" -> pure InvalidCreateProjectPermission
    "InvalidCreateTeamProjectPermission" -> pure InvalidCreateTeamProjectPermission
    "InvalidCreateProjectOnlyToTeamPermission" -> pure InvalidCreateProjectOnlyToTeamPermission
    _ -> fail "Invalid ProjectPermissionStatus"

instance FromJSON ReleaseGroupPermissionStatus where
  parseJSON = withText "FailedPermissionType" $ \case
    "ValidReleaseGroupPermission" -> pure ValidReleaseGroupPermission
    "InvalidEditReleaseGroupPermission " -> pure InvalidEditReleaseGroupPermission
    "InvalidCreateTeamProjectsForReleaseGroupPermission" -> pure InvalidCreateTeamProjectsForReleaseGroupPermission
    _ -> fail "Invalid ReleaseGroupPermissionStatus"

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
  , uploadWarnings :: Maybe [Text]
  }
  deriving (Eq, Ord, Show)

instance FromJSON UploadResponse where
  parseJSON = withObject "UploadResponse" $ \obj ->
    UploadResponse . parseLocator
      <$> (obj .: "locator")
      <*> obj .:? "error"
      <*> obj .:? "warnings"

data RevisionDependencyCacheStatus
  = Ready
  | Waiting
  | UnknownDependencyCacheStatus Text
  deriving (Eq, Ord, Show)

newtype RevisionDependencyCache = RevisionDependencyCache {status :: RevisionDependencyCacheStatus}
  deriving (Eq, Ord, Show)

instance FromJSON RevisionDependencyCache where
  parseJSON = withObject "RevisionDependencyCache" $ \obj ->
    RevisionDependencyCache
      <$> obj .: "status"

instance FromJSON RevisionDependencyCacheStatus where
  parseJSON = withText "RevisionDependencyCacheStatus" $ \txt -> case Text.toUpper txt of
    "WAITING" -> pure Waiting
    "READY" -> pure Ready
    other -> pure $ UnknownDependencyCacheStatus other

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

    issuesSortedByName :: Map IssueType [Issue]
    issuesSortedByName = sortBy (compare `on` issueName) <$> categorize issueType issuesList

    issuesSortedByCategory :: Map IssueCategory [(IssueType, [Issue])]
    issuesSortedByCategory =
      categorize (issueTypeToCategory . fst)
        . Map.toList
        $ issuesSortedByName

    renderSection :: IssueType -> [Issue] -> Doc ann
    renderSection issueType rawIssues =
      renderHeader issueType (length rawIssues)
        <> line
        <> vsep (map (\i -> renderIssue i <> line) rawIssues)
        <> line

    renderedRevisionSummary :: Doc ann
    renderedRevisionSummary = case issuesSummary issues of
      Nothing -> mempty
      Just summary ->
        vsep
          [ line
          , headerLine
          , "Tested Following Project:"
          , headerLine
          , line
          , "Project Title: " <> pretty (isrProjectTitle . revision $ summary)
          , "Project Revision: " <> pretty (isrProjectRevision . revision $ summary)
          , "Project Visibility: " <> renderProjectVisibility (isrIsPublic . revision $ summary)
          , renderRevisionTargets (targets summary)
          , line
          ]

    renderRevisionTargets :: [IssueSummaryTarget] -> Doc ann
    renderRevisionTargets [] = mempty
    renderRevisionTargets targets =
      vsep $
        ["Project Targets:"]
          <> map (\target -> "- " <> renderedTarget target) (sort targets)

    renderProjectVisibility :: Maybe Bool -> Doc ann
    renderProjectVisibility Nothing = "unknown"
    renderProjectVisibility (Just True) = "public"
    renderProjectVisibility (Just False) = "private"

    renderedTarget :: IssueSummaryTarget -> Doc ann
    renderedTarget issueRevisionTarget =
      pretty (toLower $ istTargetType issueRevisionTarget)
        <> ": "
        <> pretty (istTargetPaths issueRevisionTarget)

    rendered :: Doc ann
    rendered =
      vsep $
        [renderedRevisionSummary]
          <> do
            (category, categoryIssues) <- Map.toList issuesSortedByCategory
            let totalIssuesCount = length . mconcat . map snd $ categoryIssues
            let issuesRendered = map (uncurry renderSection) categoryIssues
            let categoryHeader = pretty category <> " ISSUES (Total " <> pretty totalIssuesCount <> ")" <> line
            categoryHeader : issuesRendered

    headerLine :: Doc ann
    headerLine = "========================================================================"

    renderHeader :: Pretty a => a -> Int -> Doc ann
    renderHeader headerItem count =
      vsep
        [ headerLine
        , pretty headerItem <> " (Total " <> pretty count <> ")"
        , headerLine
        ]

    issueName :: Issue -> Text
    issueName Issue{issueRevisionId = revisionId} =
      fromMaybe revisionId $ Text.split (\c -> c == '$' || c == '+') revisionId !? 1

    renderIssue :: Issue -> Doc ann
    renderIssue Issue{..} =
      vsep
        ( map format . catMaybes $
            [ Just issueTitle
            , cveMessage
            , fixedIn
            , issueLink
            ]
        )
      where
        format :: Text -> Doc ann
        format = fill padding . pretty

        locatorSplit = Text.split (\c -> c == '$' || c == '+') issueRevisionId

        issueTitle :: Text
        issueTitle =
          "⚑ "
            <> case issueType of
              IssueAbandonware -> "Abandoned dependency detected in " <> nameRevision
              IssueEmptyPackage -> "Empty package detected in " <> nameRevision
              IssueDenyListedDep -> "Denylist dependency detected in " <> nameRevision
              IssueNativeCode -> "Native code dependency detected in " <> nameRevision
              IssueUnlicensedAndPublicDep -> "Unlicensed dependency detected in " <> nameRevision
              IssuePolicyFlag -> issuePolicyFlagMessage
              IssuePolicyConflict -> issuePolicyConflictMessage
              IssueVulnerability -> "Critical vulnerability detected on " <> nameRevision
              IssueUnlicensedDependency -> "Unlicensed dependency detected in " <> nameRevision
              IssueOutdatedDependency -> "Outdated dependency detected in " <> nameRevision
              IssueOther t -> t

        name = fromMaybe issueRevisionId (locatorSplit !? 1)
        revision = fromMaybe "" (locatorSplit !? 2)

        nameRevision :: Text
        nameRevision = name <> "@" <> revision

        intToText :: Int -> Text
        intToText = toText . show

        issuePolicyConflictMessage :: Text
        issuePolicyConflictMessage =
          "Denied by policy "
            <> fromMaybe ("(unknown policy, issueId: " <> intToText issueId <> ")") issueLicense
            <> " on "
            <> nameRevision

        issueLink :: Maybe Text
        issueLink = ("More information: " <>) <$> issueDashURL

        cveMessage :: Maybe Text
        cveMessage = ("CVE ID: " <>) <$> issueCVE

        fixedIn :: Maybe Text
        fixedIn = ("Fixed in: " <>) <$> issueFixedIn

        issuePolicyFlagMessage :: Text
        issuePolicyFlagMessage = fromMaybe missingRuleIdMsg (issuePolicyFlagMsg <|> missingLicenseIdMsg)
          where
            ruleId' :: Maybe Text
            ruleId' = intToText <$> (ruleId =<< issueRule)

            issuePolicyFlagMsg :: Maybe Text
            issuePolicyFlagMsg = (\l -> l <> " license detected in " <> nameRevision) <$> issueLicense

            missingLicenseIdMsg :: Maybe Text
            missingLicenseIdMsg = (\rId -> "Policy flag issue detected (ruleId:  " <> rId <> ") in " <> nameRevision) <$> ruleId'

            missingRuleIdMsg :: Text
            missingRuleIdMsg = "Policy flag issue detected (issueId: " <> intToText issueId <> ") in " <> nameRevision

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

--- Path Dependency

data PathDependencyUploadReq = PathDependencyUploadReq
  { path :: Text
  , version :: Text
  , projectLocator :: Locator
  , scanType :: FullFileUploads
  }

instance ToJSON PathDependencyUploadReq where
  toJSON PathDependencyUploadReq{..} =
    object
      [ "path" .= path
      , "version" .= version
      , "projectLocator" .= projectLocator
      , "scanType" .= fullFileUploadsToCliLicenseScanType scanType
      ]

data PathDependencyFinalizeReq = PathDependencyFinalizeReq
  { locators :: [Locator]
  , pathDepForceRebuild :: Bool
  }

instance ToJSON PathDependencyFinalizeReq where
  toJSON PathDependencyFinalizeReq{..} =
    object
      [ "locators" .= locators
      , "forceRebuild" .= pathDepForceRebuild
      ]

data PathDependencyUpload = PathDependencyUpload
  { pdSignedURL :: SignedURL
  , pdLocator :: UploadedPathDependencyLocator
  }
  deriving (Eq, Ord, Show)

instance FromJSON PathDependencyUpload where
  parseJSON = withObject "PathDependencyUpload" $ \obj -> do
    signedUrl <- obj .: "signedUrl"
    locator <- obj .: "locator"
    pure $ PathDependencyUpload (SignedURL signedUrl) locator

data UploadedPathDependencyLocator = UploadedPathDependencyLocator
  { updlName :: Text
  , updlVersion :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON UploadedPathDependencyLocator where
  parseJSON = withObject "UploadedPathDependencyLocator" $ \obj ->
    UploadedPathDependencyLocator
      <$> obj .: "name"
      <*> obj .: "version"

newtype AnalyzedPathDependenciesResp = AnalyzedPathDependenciesResp {analyzedPathDeps :: [AnalyzedPathDependency]}
  deriving (Eq, Ord, Show)

instance FromJSON AnalyzedPathDependenciesResp where
  parseJSON = withObject "AnalyzedPathDependenciesResp" $ \obj ->
    AnalyzedPathDependenciesResp <$> obj .: "data"

data AnalyzedPathDependency = AnalyzedPathDependency
  { apdPath :: Text
  , adpId :: Text
  , adpVersion :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON AnalyzedPathDependency where
  parseJSON = withObject "AnalyzedPathDependency" $ \obj ->
    AnalyzedPathDependency
      <$> obj .: "path"
      <*> obj .: "id"
      <*> obj .: "version"
