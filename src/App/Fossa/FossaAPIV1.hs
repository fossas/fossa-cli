module App.Fossa.FossaAPIV1
  ( uploadAnalysis
  , UploadResponse(..)
  , ProjectRevision(..)
  , ProjectMetadata(..)
  , FossaError(..)
  , fossaReq

  , getLatestBuild
  , Build(..)
  , BuildTask(..)
  , BuildStatus(..)
  , getIssues
  , Issues(..)
  , Issue(..)
  , IssueType(..)
  , renderIssueType
  , IssueRule(..)

  , getOrganizationId
  ) where

import App.Fossa.Analyze.Project
import Control.Carrier.Error.Either
import Data.List (isInfixOf)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Effect.Logger
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import qualified Network.HTTP.Types as HTTP
import Prologue
import Srclib.Converter (toSourceUnit)
import Srclib.Types
import Data.Maybe (catMaybes)
import OptionExtensions

newtype FossaReq a = FossaReq { unFossaReq :: ErrorC FossaError IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadHttp FossaReq where
  handleHttpException = FossaReq . throwError . mangleError

fossaReq :: MonadIO m => FossaReq a -> m (Either FossaError a)
fossaReq = liftIO . runError @FossaError . unFossaReq

-- TODO: git commit?
cliVersion :: Text
cliVersion = "spectrometer"

uploadUrl :: Url scheme -> Url scheme
uploadUrl baseurl = baseurl /: "api" /: "builds" /: "custom"

-- FIXME: we only want to include organizationId for "archive" and "custom"
-- TODO: need to normalize "git" projects
-- render a locator for use in fossa API urls
renderLocatorUrl :: Int -> Locator -> Text
renderLocatorUrl orgId Locator{..} =
  locatorFetcher <> "+" <> T.pack (show orgId) <> "/" <> locatorProject <> "$" <> fromMaybe "" locatorRevision

data UploadResponse = UploadResponse
  { uploadLocator :: Text
  , uploadError   :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UploadResponse where
  parseJSON = withObject "UploadResponse" $ \obj ->
    UploadResponse <$> obj .: "locator"
                   <*> obj .:? "error"

data FossaError
  = InvalidProjectOrRevision HttpException
  | NoPermission HttpException
  | JsonDeserializeError String
  | OtherError HttpException
  deriving (Show, Generic)

data ProjectRevision = ProjectRevision
  { projectName :: Text
  , projectRevision :: Text
  } deriving (Eq, Ord, Show, Generic)

data ProjectMetadata = ProjectMetadata
  { projectTitle :: Maybe Text
  , projectBranch :: Maybe Text
  , projectUrl :: Maybe Text
  , projectJiraKey :: Maybe Text
  , projectLink :: Maybe Text
  , projectTeam :: Maybe Text
  , projectPolicy :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

uploadAnalysis
  :: UrlOption -- ^ base url
  -> Text -- ^ api key
  -> ProjectRevision
  -> ProjectMetadata
  -> [Project]
  -> FossaReq UploadResponse
uploadAnalysis baseurl key ProjectRevision{..} metadata projects = do
  let filteredProjects = filter (isProductionPath . projectPath) projects
      sourceUnits = fromMaybe [] $ traverse toSourceUnit filteredProjects
      opts = "locator" =: renderLocator (Locator "custom" projectName (Just projectRevision))
          <> "v" =: cliVersion
          <> "managedBuild" =: True
          <> "title" =: fromMaybe projectName (projectTitle metadata)
          <> header "Authorization" ("token " <> encodeUtf8 key)
          <> mkMetadataOpts metadata
  resp <- req POST (uploadUrl $ urlOptionUrl baseurl) (ReqBodyJson sourceUnits) jsonResponse (urlOptionOptions baseurl <> opts)
  pure (responseBody resp)

mkMetadataOpts :: ProjectMetadata -> Option scheme
mkMetadataOpts ProjectMetadata{..} = mconcat $ catMaybes 
  [ ("branch" =:) <$> projectBranch
  , ("projectURL" =:) <$> projectUrl
  , ("jiraProjectKey" =:) <$> projectJiraKey
  , ("link" =:) <$> projectLink
  , ("team" =:) <$> projectTeam
  , ("policy" =:) <$> projectPolicy
  ]

mangleError :: HttpException -> FossaError
mangleError err = case err of
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
    case HTTP.responseStatus resp of
      HTTP.Status 404 _ -> InvalidProjectOrRevision err
      HTTP.Status 403 _ -> NoPermission err
      _                 -> OtherError err
  JsonHttpException msg -> JsonDeserializeError msg
  _ -> OtherError err

-- we specifically want Rel paths here: parent directories shouldn't affect path
-- filtering
isProductionPath :: Path Rel fd -> Bool
isProductionPath path = not $ any (`isInfixOf` toFilePath path)
  [ "doc/"
  , "docs/"
  , "test/"
  , "example/"
  , "examples/"
  , "vendor/"
  , "node_modules/"
  , ".srclib-cache/"
  , "spec/"
  , "Godeps/"
  , ".git/"
  , "bower_components/"
  , "third_party/"
  , "third-party/"
  , "tmp/"
  , "Carthage/"
  , "Checkouts/"
  ]

-----

buildsEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
buildsEndpoint baseurl orgId locator = baseurl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "latest_build"

data BuildStatus
  = StatusSucceeded
  | StatusFailed
  | StatusCreated
  | StatusAssigned
  | StatusRunning
  | StatusUnknown Text
  deriving (Eq, Ord, Show, Generic)

data Build = Build
  { buildId :: Int
  , buildError :: Maybe Text
  , buildTask :: BuildTask
  } deriving (Eq, Ord, Show, Generic)

newtype BuildTask = BuildTask
  { buildTaskStatus :: BuildStatus
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Build where
  parseJSON = withObject "Build" $ \obj ->
    Build <$> obj .: "id"
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

getLatestBuild
  :: UrlOption
  -> Text -- ^ api key
  -> ProjectRevision
  -> FossaReq Build
getLatestBuild baseurl key ProjectRevision{..} = do
  let url = urlOptionUrl baseurl
      opts = urlOptionOptions baseurl <> header "Authorization" ("token " <> encodeUtf8 key)
  Organization orgId <- responseBody <$> req GET (organizationEndpoint url) NoReqBody jsonResponse opts
  response <- req GET (buildsEndpoint url orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

----------

issuesEndpoint :: Int -> Locator -> Url 'Https
issuesEndpoint orgId locator = https "app.fossa.com" /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "issues"

getIssues
  :: UrlOption
  -> Text -- ^ api key
  -> ProjectRevision
  -> FossaReq Issues
getIssues baseurl key ProjectRevision{..} = do
  let opts = urlOptionOptions baseurl <> header "Authorization" ("token " <> encodeUtf8 key)
      url = urlOptionUrl baseurl
  Organization orgId <- responseBody <$> req GET (organizationEndpoint url) NoReqBody jsonResponse opts
  response <- req GET (issuesEndpoint orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

data Issues = Issues
  { issuesCount :: Int
  , issuesIssues :: [Issue]
  , issuesStatus :: Text
  } deriving (Eq, Ord, Show, Generic)

data IssueType
  = IssuePolicyConflict
  | IssuePolicyFlag
  | IssueVulnerability
  | IssueUnlicensedDependency
  | IssueOutdatedDependency
  | IssueOther Text
  deriving (Eq, Ord, Show, Generic)

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
  , issueResolved :: Bool
  , issueRevisionId :: Text
  , issueType :: IssueType
  , issueRule :: Maybe IssueRule
  } deriving (Eq, Ord, Show, Generic)

newtype IssueRule = IssueRule
  { ruleLicenseId :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Issues where
  parseJSON = withObject "Issues" $ \obj ->
    Issues <$> obj .: "count"
           <*> obj .: "issues"
           <*> obj .: "status"

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \obj ->
    Issue <$> obj .: "id"
           <*> obj .: "resolved"
           <*> obj .: "revisionId"
           <*> obj .: "type"
           <*> obj .:? "rule"

instance FromJSON IssueType where
  parseJSON = withText "IssueType" $ \case
    "policy_conflict" -> pure IssuePolicyConflict
    "policy_flag" -> pure IssuePolicyFlag
    "vulnerability" -> pure IssueVulnerability
    "unlicensed_dependency" -> pure IssueUnlicensedDependency
    "outdated_dependency" -> pure IssueOutdatedDependency
    other -> pure (IssueOther other)

instance FromJSON IssueRule where
  parseJSON = withObject "IssueRule" $ \obj ->
    IssueRule <$> obj .:? "licenseId"

----------

newtype Organization = Organization
  { organizationId :: Int
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
    Organization <$> obj .: "organizationId"

organizationEndpoint :: Url scheme -> Url scheme
organizationEndpoint baseurl = baseurl /: "api" /: "cli" /: "organization"

-- TODO: Is this used anywhere?
getOrganizationId
  :: UrlOption
  -> Text -- ^ api key
  -> FossaReq Issues
getOrganizationId baseurl key = do
  let opts = urlOptionOptions baseurl <> header "Authorization" ("token " <> encodeUtf8 key)
      url = urlOptionUrl baseurl
  response <- req GET (organizationEndpoint url) NoReqBody jsonResponse opts
  pure (responseBody response)
