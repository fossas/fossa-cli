{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.FossaAPIV1
  ( uploadAnalysis,
    uploadContributors,
    uploadContainerScan,
    UploadResponse (..),
    mkMetadataOpts,
    FossaError (..),
    FossaReq (..),
    Contributors (..),
    fossaReq,
    getLatestBuild,
    Build (..),
    BuildTask (..),
    BuildStatus (..),
    getIssues,
    Organization (..),
    getOrganization,
    getAttribution,
    getAttributionRaw,
  )
where

import App.Fossa.Analyze.Project
import App.Fossa.Container (ContainerScan (..))
import qualified App.Fossa.Report.Attribution as Attr
import App.Types
import App.Version (versionNumber)
import Control.Effect.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Effect.Logger
import Fossa.API.Types (ApiOpts, Issues, useApiOpts)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import qualified Network.HTTP.Types as HTTP
import Srclib.Converter (toSourceUnit)
import Srclib.Types
import Text.URI (URI)
import qualified Text.URI as URI

newtype FossaReq m a = FossaReq {unFossaReq :: m a}
  deriving (Functor, Applicative, Monad, Algebra sig)

instance Has (Lift IO) sig m => MonadIO (FossaReq m) where
  liftIO = sendIO

instance (Has (Lift IO) sig m, Has Diagnostics sig m) => MonadHttp (FossaReq m) where
  handleHttpException = FossaReq . fatal . mangleError

fossaReq :: FossaReq m a -> m a
fossaReq = unFossaReq

-- Don't send any version if one doesn't exist
cliVersion :: Text
cliVersion = fromMaybe "" versionNumber

uploadUrl :: Url scheme -> Url scheme
uploadUrl baseurl = baseurl /: "api" /: "builds" /: "custom"

-- | This renders an organization + locator into a path piece for the fossa API
renderLocatorUrl :: Int -> Locator -> Text
renderLocatorUrl orgId Locator {..} =
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

data UploadResponse = UploadResponse
  { uploadLocator :: Text,
    uploadError :: Maybe Text
  }
  deriving (Eq, Ord, Show)

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

containerUploadUrl :: Url scheme -> Url scheme
containerUploadUrl baseurl = baseurl /: "api" /: "container" /: "upload"

uploadContainerScan ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  ContainerScan ->
  m UploadResponse
uploadContainerScan apiOpts ProjectRevision {..} metadata scan = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let locator = renderLocator $ Locator "custom" projectName (Just projectRevision)
      opts =
        "locator" =: locator
          <> "cliVersion" =: cliVersion
          <> "managedBuild" =: True
          <> mkMetadataOpts metadata projectName
  resp <- req POST (containerUploadUrl baseUrl) (ReqBodyJson scan) jsonResponse (baseOpts <> opts)
  pure $ responseBody resp

uploadAnalysis ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  NE.NonEmpty ProjectResult ->
  m UploadResponse
uploadAnalysis apiOpts ProjectRevision {..} metadata projects = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let sourceUnits = map toSourceUnit $ NE.toList projects
      opts =
        "locator" =: renderLocator (Locator "custom" projectName (Just projectRevision))
          <> "cliVersion" =: cliVersion
          <> "managedBuild" =: True
          <> mkMetadataOpts metadata projectName
          -- Don't include branch if it doesn't exist, core may not handle empty string properly.
          <> maybe mempty ("branch" =:) projectBranch
  resp <- req POST (uploadUrl baseUrl) (ReqBodyJson sourceUnits) jsonResponse (baseOpts <> opts)
  pure (responseBody resp)

mkMetadataOpts :: ProjectMetadata -> Text -> Option scheme
mkMetadataOpts ProjectMetadata {..} projectName = mconcat $ catMaybes maybes
  where
    title = Just $ fromMaybe projectName projectTitle
    maybes =
      [ ("projectURL" =:) <$> projectUrl,
        ("jiraProjectKey" =:) <$> projectJiraKey,
        ("link" =:) <$> projectLink,
        ("team" =:) <$> projectTeam,
        ("policy" =:) <$> projectPolicy,
        ("title" =:) <$> title
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
  { buildId :: Int,
    buildError :: Maybe Text,
    buildTask :: BuildTask
  }
  deriving (Eq, Ord, Show)

newtype BuildTask = BuildTask
  { buildTaskStatus :: BuildStatus
  }
  deriving (Eq, Ord, Show)

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

getLatestBuild ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Build
getLatestBuild apiOpts ProjectRevision {..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  Organization orgId _ <- getOrganization apiOpts

  response <- req GET (buildsEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

----------

issuesEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
issuesEndpoint baseUrl orgId locator = baseUrl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "issues"

getIssues ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Issues
getIssues apiOpts ProjectRevision {..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  Organization orgId _ <- getOrganization apiOpts
  response <- req GET (issuesEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

---------------

attributionEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
attributionEndpoint baseurl orgId locator = baseurl /: "api" /: "revisions" /: renderLocatorUrl orgId locator /: "attribution" /: "json"

getAttribution ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Attr.Attribution
getAttribution apiOpts ProjectRevision {..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        baseOpts
          <> "includeDeepDependencies" =: True
          <> "includeHashAndVersionData" =: True
          <> "includeDownloadUrl" =: True
  Organization orgId _ <- getOrganization apiOpts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

getAttributionRaw ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Value
getAttributionRaw apiOpts ProjectRevision {..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        baseOpts
          <> "includeDeepDependencies" =: True
          <> "includeHashAndVersionData" =: True
          <> "includeDownloadUrl" =: True
  Organization orgId _ <- getOrganization apiOpts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

----------

data Organization = Organization
  { organizationId :: Int,
    orgUsesSAML :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
    Organization <$> obj .: "organizationId"
      <*> obj .:? "usesSAML" .!= False

organizationEndpoint :: Url scheme -> Url scheme
organizationEndpoint baseurl = baseurl /: "api" /: "cli" /: "organization"

getOrganization :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> m Organization
getOrganization apiOpts = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse baseOpts

----------

newtype Contributors = Contributors
  {unContributors :: Map Text Text}
  deriving (Eq, Ord, Show, ToJSON)

contributorsEndpoint :: Url scheme -> Url scheme
contributorsEndpoint baseurl = baseurl /: "api" /: "contributors"

uploadContributors :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Text -> Contributors -> m ()
uploadContributors apiOpts locator contributors = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = baseOpts <> "locator" =: locator

  _ <- req POST (contributorsEndpoint baseUrl) (ReqBodyJson contributors) ignoreResponse opts
  pure ()
