module Control.Effect.FossaApiClient (
  FossaApiClientF (..),
  FossaApiClient,
  getApiOpts,
  getProject,
  getOrganization,
  uploadContributors,
  uploadAnalysis,
) where

import App.Types (ProjectMetadata, ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.Simple (Simple, sendSimple)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Fossa.API.Types (ApiOpts, Contributors, Organization, Project, UploadResponse)
import Srclib.Types (SourceUnit)

data FossaApiClientF a where
  GetOrganization :: FossaApiClientF Organization
  GetProject :: ProjectRevision -> FossaApiClientF Project
  GetApiOpts :: FossaApiClientF ApiOpts
  UploadAnalysis ::
    ProjectRevision ->
    ProjectMetadata ->
    NE.NonEmpty SourceUnit ->
    FossaApiClientF UploadResponse
  UploadContributors ::
    -- | Locator
    Text ->
    Contributors ->
    FossaApiClientF ()

type FossaApiClient = Simple FossaApiClientF

-- | Fetches the organization associated with the current API token
getOrganization :: (Has FossaApiClient sig m) => m Organization
getOrganization = sendSimple GetOrganization

-- | Fetches the project associated with a revision
getProject :: (Has FossaApiClient sig m) => ProjectRevision -> m Project
getProject = sendSimple . GetProject

-- | Returns the API options currently in scope.
-- The API options contain a lot of information required to build URLs.
getApiOpts :: (Has FossaApiClient sig m) => m ApiOpts
getApiOpts = sendSimple GetApiOpts

-- | Uploads the results of an analysis and associates it to a project
uploadAnalysis :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> NE.NonEmpty SourceUnit -> m UploadResponse
uploadAnalysis revision metadata units = sendSimple (UploadAnalysis revision metadata units)

-- | Associates contributors to a specific locator
uploadContributors :: (Has FossaApiClient sig m) => Text -> Contributors -> m ()
uploadContributors locator contributors = sendSimple $ UploadContributors locator contributors
