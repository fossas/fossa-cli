{-# LANGUAGE RecordWildCards #-}

module Control.Carrier.FossaApiClient.Internal.Core (
  getAttribution,
  getIssues,
  getLatestBuild,
  getOrganization,
  getProject,
  getAnalyzedRevisions,
  getRevisionDependencyCacheStatus,
  getSignedUploadUrl,
  queueArchiveBuild,
  uploadAnalysis,
  uploadAnalysisWithFirstPartyLicenses,
  uploadArchive,
  uploadNativeContainerScan,
  uploadContributors,
  getEndpointVersion,
) where

import App.Fossa.Config.Report (ReportOutputFormat)
import App.Fossa.Config.Test (DiffRevision)
import App.Fossa.VendoredDependency (VendoredDependency (..))
import App.Types (FullFileUploads, ProjectMetadata, ProjectRevision (..))
import Container.Types qualified as NativeContainer
import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 qualified as API
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (PackageRevision (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Fossa.API.Types (
  ApiOpts,
  Archive,
  Build,
  Contributors,
  Issues,
  Organization,
  Project,
  RevisionDependencyCache,
  SignedURL,
  UploadResponse,
 )
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

getAnalyzedRevisions ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  NE.NonEmpty VendoredDependency ->
  m [Text]
getAnalyzedRevisions vdeps = do
  apiOpts <- ask
  API.getAnalyzedRevisions apiOpts vdeps

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

uploadAnalysisWithFirstPartyLicenses ::
  ( Has (Lift IO) sig m
  , Has (Reader ApiOpts) sig m
  , Has Diagnostics sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  FullFileUploads ->
  m UploadResponse
uploadAnalysisWithFirstPartyLicenses revision metadata fullFileUploads = do
  apiOpts <- ask
  API.uploadAnalysisWithFirstPartyLicenses apiOpts revision metadata fullFileUploads

uploadNativeContainerScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  NativeContainer.ContainerScan ->
  m UploadResponse
uploadNativeContainerScan revision metadata scan = do
  apiOpts <- ask
  API.uploadNativeContainerScan apiOpts revision metadata scan

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
  Maybe DiffRevision ->
  m Issues
getIssues rev diffRevision = do
  apiOpts <- ask
  API.getIssues apiOpts rev diffRevision

getAttribution ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  ReportOutputFormat ->
  m Text
getAttribution revision format = do
  apiOpts <- ask
  API.getAttribution apiOpts revision format

getRevisionDependencyCacheStatus ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m RevisionDependencyCache
getRevisionDependencyCacheStatus rev = do
  apiOpts <- ask
  API.getRevisionDependencyCacheStatus apiOpts rev

getSignedUploadUrl ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  PackageRevision ->
  m SignedURL
getSignedUploadUrl PackageRevision{..} = do
  apiOpts <- ask
  API.getSignedURL apiOpts packageVersion packageName

queueArchiveBuild ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Archive ->
  m (Maybe C8.ByteString)
queueArchiveBuild archive = do
  apiOpts <- ask
  API.archiveBuildUpload apiOpts archive

uploadArchive ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  SignedURL ->
  FilePath ->
  m ByteString
uploadArchive =
  API.archiveUpload

getEndpointVersion ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  m Text
getEndpointVersion = do
  apiOpts <- ask
  API.getEndpointVersion apiOpts
