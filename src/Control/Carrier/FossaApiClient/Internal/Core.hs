{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Carrier.FossaApiClient.Internal.Core (
  getAttribution,
  getIssues,
  getLatestBuild,
  getOrganization,
  getProject,
  uploadAnalysis,
  uploadContainerScan,
  uploadContributors,
) where

import App.Fossa.Config.Report (ReportOutputFormat (ReportJson, ReportMarkdown, ReportSpdx))
import App.Fossa.Container.Scan (ContainerScan)
import App.Fossa.FossaAPIV1 (FossaReq (FossaReq), fossaReq, renderLocatorUrl)
import App.Fossa.FossaAPIV1 qualified as API
import App.Fossa.Report.Attribution (Attribution)
import App.Types (ProjectMetadata, ProjectRevision (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Data.String.Conversion (decodeUtf8)
import Data.Text
import Fossa.API.Types (ApiOpts, Build, Contributors, Issues, Organization (organizationId), Project, UploadResponse, useApiOpts)
import Network.HTTP.Req (GET (GET), NoReqBody (NoReqBody), Scheme (Https), Url, bsResponse, jsonResponse, req, responseBody, (/:), (=:))
import Srclib.Types (Locator (Locator), SourceUnit, createCustomLocator, renderLocator)

-- Fetches an organization from the API
getOrganization ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  m Organization
getOrganization = do
  apiOpts <- ask
  -- TODO: This is a wrapper around FossaAPIV1 for now until more uses are
  -- migrated.
  API.getOrganization apiOpts

getProject ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m Project
getProject revision = do
  apiOpts <- ask
  API.getProject apiOpts revision

uploadAnalysis ::
  ( Has (Lift IO) sig m
  , Has (Reader ApiOpts) sig m
  , Has Diagnostics sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  NE.NonEmpty SourceUnit ->
  m UploadResponse
uploadAnalysis revision metadata units = do
  apiOpts <- ask
  API.uploadAnalysis apiOpts revision metadata units

uploadContainerScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  ContainerScan ->
  m UploadResponse
uploadContainerScan revision metadata scan = do
  apiOpts <- ask
  API.uploadContainerScan apiOpts revision metadata scan

uploadContributors ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Locator ->
  Contributors ->
  m ()
uploadContributors locator contributors = do
  apiOpts <- ask
  API.uploadContributors apiOpts (renderLocator locator) contributors

getLatestBuild ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m Build
getLatestBuild rev = do
  apiOpts <- ask
  API.getLatestBuild apiOpts rev

getIssues ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m Issues
getIssues rev = do
  apiOpts <- ask
  API.getIssues apiOpts rev

----------

attributionEndpoint :: Url 'Https -> Int -> Locator -> ReportOutputFormat -> Url 'Https
attributionEndpoint baseurl orgId locator format = appendSegment format $ baseurl /: "api" /: "revisions" /: renderLocatorUrl orgId locator /: "attribution"
  where
    appendSegment :: ReportOutputFormat -> Url a -> Url a
    appendSegment ReportJson input = input /: "json"
    appendSegment ReportMarkdown input = input /: "full" /: "MD"
    appendSegment ReportSpdx input = input /: "full" /: "spdx"

getAttributionJson ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m Attribution
getAttributionJson revision = fossaReq $ do
  apiOpts <- ask
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        baseOpts
          <> "includeDeepDependencies" =: True
          <> "includeHashAndVersionData" =: True
          <> "includeDownloadUrl" =: True
  orgId <- organizationId <$> getOrganization
  let locator = createCustomLocator revision -- Locator "custom" projectName (Just projectRevision)
  response <- req GET (attributionEndpoint baseUrl orgId locator ReportJson) NoReqBody jsonResponse opts
  pure (responseBody response)

getAttribution ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  ReportOutputFormat ->
  m Text
getAttribution revision ReportJson = do
  jsonValue <- getAttributionJson revision
  pure . decodeUtf8 $ Aeson.encode jsonValue
getAttribution revision format = fossaReq $ do
  apiOpts <- ask
  (baseUrl, opts) <- useApiOpts apiOpts

  orgId <- organizationId <$> getOrganization
  -- let locator = Locator "custom" projectName (Just projectRevision)
  let locator = createCustomLocator revision
  response <- req GET (attributionEndpoint baseUrl orgId locator format) NoReqBody bsResponse opts
  pure (decodeUtf8 $ responseBody response)
