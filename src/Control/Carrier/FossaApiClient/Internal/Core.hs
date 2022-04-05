module Control.Carrier.FossaApiClient.Internal.Core (
  getIssues,
  getLatestBuild,
  getOrganization,
  getProject,
  uploadAnalysis,
  uploadContainerScan,
  uploadContributors,
) where

import App.Fossa.Container.Scan (ContainerScan)
import App.Fossa.FossaAPIV1 qualified as API
import App.Types (ProjectMetadata, ProjectRevision)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Data.List.NonEmpty qualified as NE
import Fossa.API.Types (ApiOpts, Build, Contributors, Issues, Organization, Project, UploadResponse)
import Srclib.Types (Locator, SourceUnit, renderLocator)

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
