{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.FossaAPIV1
  ( uploadAnalysis
  , uploadContributors
  , UploadResponse(..)
  , ProjectMetadata(..)
  , FossaError(..)
  , FossaReq(..)
  , Contributors(..)
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

  , getAttribution
  ) where

import App.Fossa.Analyze.Project
import qualified App.Fossa.Report.Attribution as Attr
import App.Types
import App.Util (parseUri)
import Control.Effect.Diagnostics
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.List (isInfixOf, stripPrefix)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Effect.Logger
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import qualified Network.HTTP.Types as HTTP
import Path
import Srclib.Converter (toSourceUnit)
import Srclib.Types
import Text.URI (URI)
import qualified Text.URI as URI

newtype FossaReq m a = FossaReq { unFossaReq :: m a }
  deriving (Functor, Applicative, Monad, Algebra sig)

instance Has (Lift IO) sig m => MonadIO (FossaReq m) where
  liftIO = sendIO

instance (Has (Lift IO) sig m, Has Diagnostics sig m) => MonadHttp (FossaReq m) where
  handleHttpException = FossaReq . fatal . mangleError

fossaReq :: FossaReq m a -> m a
fossaReq = unFossaReq

-- TODO: git commit?
cliVersion :: Text
cliVersion = "spectrometer"

uploadUrl :: Url scheme -> Url scheme
uploadUrl baseurl = baseurl /: "api" /: "builds" /: "custom"

-- | This renders an organization + locator into a path piece for the fossa API
renderLocatorUrl :: Int -> Locator -> Text
renderLocatorUrl orgId Locator{..} =
  locatorFetcher <> "+" <> T.pack (show orgId) <> "/" <> normalizeGitProjectName locatorProject <> "$" <> fromMaybe "" locatorRevision

-- | The fossa backend treats http git locators in a specific way for the issues and builds endpoints.
-- This normalizes a project name to conform to what the API expects
normalizeGitProjectName :: Text -> Text
normalizeGitProjectName project
  | "http" `T.isPrefixOf` project = dropPrefix "http://" . dropPrefix "https://" . dropSuffix ".git" $ project
  | otherwise = project
    where
      -- like Text.stripPrefix, but with a non-Maybe result (defaults to the original text)
      dropPrefix :: Text -> Text -> Text
      dropPrefix pre txt = fromMaybe txt (T.stripPrefix pre txt)

      -- like Text.stripSuffix, but with a non-Maybe result (defaults to the original text)
      dropSuffix :: Text -> Text -> Text
      dropSuffix suf txt = fromMaybe txt (T.stripSuffix suf txt)

apiHeader :: ApiKey -> Option scheme
apiHeader key = header "Authorization" ("token " <> encodeUtf8 (unApiKey key))

data UploadResponse = UploadResponse
  { uploadLocator :: Text
  , uploadError   :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromJSON UploadResponse where
  parseJSON = withObject "UploadResponse" $ \obj ->
    UploadResponse <$> obj .: "locator"
                   <*> obj .:? "error"

data FossaError
  = InvalidProjectOrRevision HttpException
  | NoPermission HttpException
  | JsonDeserializeError String
  | OtherError HttpException
  | BadURI URI
  deriving (Show)

instance ToDiagnostic FossaError where
  renderDiagnostic = \case
    InvalidProjectOrRevision _ -> "Response from FOSSA API: invalid project or revision"
    NoPermission _ -> "Response from FOSSA API: no permission"
    JsonDeserializeError err -> "An error occurred when deserializing a response from the FOSSA API: " <> pretty err
    OtherError err -> "An unknown error occurred when accessing the FOSSA API: " <> viaShow err
    BadURI uri -> "Invalid FOSSA URL: " <> pretty (URI.render uri)

data ProjectMetadata = ProjectMetadata
  { projectTitle :: Maybe Text
  , projectUrl :: Maybe Text
  , projectJiraKey :: Maybe Text
  , projectLink :: Maybe Text
  , projectTeam :: Maybe Text
  , projectPolicy :: Maybe Text
  } deriving (Eq, Ord, Show)

uploadAnalysis
  :: (Has (Lift IO) sig m, Has Diagnostics sig m)
  => BaseDir -- ^ root directory for analysis
  -> URI -- ^ base url
  -> ApiKey -- ^ api key
  -> ProjectRevision
  -> ProjectMetadata
  -> [Project]
  -> m UploadResponse
uploadAnalysis rootDir baseUri key ProjectRevision{..} metadata projects = fossaReq $ do
  (baseUrl, baseOptions) <- parseUri baseUri

  -- For each of the projects, we need to strip the root directory path from the prefix of the project path.
  -- We don't want parent directories of the scan root affecting "production path" filtering -- e.g., if we're
  -- running in a directory called "tmp", we still want results.
  let rootPath = fromAbsDir $ unBaseDir rootDir
      dropPrefix :: String -> String -> String
      dropPrefix prefix str = fromMaybe prefix (stripPrefix prefix str)
      filteredProjects = filter (isProductionPath . dropPrefix rootPath . fromAbsDir . projectPath) projects
     
      sourceUnits = fromMaybe [] $ traverse toSourceUnit filteredProjects
      opts = "locator" =: renderLocator (Locator "custom" projectName (Just projectRevision))
          <> "v" =: cliVersion
          <> "managedBuild" =: True
          <> "title" =: fromMaybe projectName (projectTitle metadata)
          <> "branch" =: projectBranch
          <> apiHeader key
          <> mkMetadataOpts metadata
  resp <- req POST (uploadUrl baseUrl) (ReqBodyJson sourceUnits) jsonResponse (baseOptions <> opts)
  pure (responseBody resp)

mkMetadataOpts :: ProjectMetadata -> Option scheme
mkMetadataOpts ProjectMetadata{..} = mconcat $ catMaybes 
  [ ("projectURL" =:) <$> projectUrl
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

isProductionPath :: FilePath -> Bool
isProductionPath path = not $ any (`isInfixOf` path)
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
  deriving (Eq, Ord, Show)

data Build = Build
  { buildId :: Int
  , buildError :: Maybe Text
  , buildTask :: BuildTask
  } deriving (Eq, Ord, Show)

newtype BuildTask = BuildTask
  { buildTaskStatus :: BuildStatus
  } deriving (Eq, Ord, Show)

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
  :: (Has (Lift IO) sig m, Has Diagnostics sig m)
  => URI
  -> ApiKey -- ^ api key
  -> ProjectRevision
  -> m Build
getLatestBuild baseUri key ProjectRevision {..} = fossaReq $ do
  (baseUrl, baseOptions) <- parseUri baseUri

  let opts = baseOptions <> apiHeader key

  Organization orgId <- responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse opts
 
  response <- req GET (buildsEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

----------

issuesEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
issuesEndpoint baseUrl orgId locator = baseUrl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "issues"

getIssues
  :: (Has (Lift IO) sig m, Has Diagnostics sig m)
  => URI
  -> ApiKey -- ^ api key
  -> ProjectRevision
  -> m Issues
getIssues baseUri key ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOptions) <- parseUri baseUri
 
  let opts = baseOptions <> apiHeader key

  Organization orgId <- responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse opts
  response <- req GET (issuesEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

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
           <*> obj .: "issues"
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
           <*> obj .: "revisionId"
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

---------------

attributionEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
attributionEndpoint baseurl orgId locator = baseurl /: "api" /: "revisions" /: renderLocatorUrl orgId locator /: "attribution" /: "json"

getAttribution
  :: (Has (Lift IO) sig m, Has Diagnostics sig m)
  => URI
  -> ApiKey -- ^ api key
  -> ProjectRevision
  -> m Attr.Attribution
getAttribution baseUri key ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOptions) <- parseUri baseUri

  let opts = baseOptions
        <> apiHeader key
        <> "includeDeepDependencies" =: True
        <> "includeHashAndVersionData" =: True
        <> "includeDownloadUrl" =: True
  Organization orgId <- responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse opts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

----------

newtype Organization = Organization
  { organizationId :: Int
  } deriving (Eq, Ord, Show)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
    Organization <$> obj .: "organizationId"

organizationEndpoint :: Url scheme -> Url scheme
organizationEndpoint baseurl = baseurl /: "api" /: "cli" /: "organization"

----------

newtype Contributors = Contributors 
  {unContributors :: Map Text Text}
  deriving (Eq, Ord, Show, ToJSON)

contributorsEndpoint :: Url scheme -> Url scheme
contributorsEndpoint baseurl = baseurl /: "api" /: "organization"

uploadContributors :: (Has (Lift IO) sig m, Has Diagnostics sig m) => URI -> ApiKey -> Text -> Contributors -> m ()
uploadContributors baseUri apiKey locator contributors = fossaReq $ do
  (baseUrl, baseOptions) <- parseUri baseUri

  let opts = baseOptions
        <> apiHeader apiKey
        <> "locator" =: locator

  _ <- req POST (contributorsEndpoint baseUrl) (ReqBodyJson contributors) ignoreResponse opts
  pure ()
