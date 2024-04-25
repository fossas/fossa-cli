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
  getTokenType,
  uploadReachabilityContent,
  uploadReachabilityBuild,
  getCustomBuildPermissions,
  getPolicies,
  getTeams,
  deleteReleaseGroup,
  deleteReleaseGroupRelease,
  createReleaseGroup,
  createReleaseGroupRelease,
  getReleaseGroups,
  getReleaseGroupReleases,
  updateReleaseGroupRelease,
) where

import App.Fossa.Config.Report (ReportOutputFormat)
import App.Fossa.Config.Test (DiffRevision)
import App.Fossa.VendoredDependency (VendoredDependency (..))
import App.Types (FullFileUploads, ProjectMetadata, ProjectRevision (..), ReleaseGroupReleaseRevision)
import Container.Types qualified as NativeContainer
import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 qualified as API
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (PackageRevision (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Fossa.API.Types (
  ApiOpts,
  Archive,
  Build,
  Contributors,
  CreateReleaseGroupRequest,
  CreateReleaseGroupResponse,
  CustomBuildUploadPermissions,
  Issues,
  Organization,
  Policy,
  Project,
  ReleaseGroup,
  ReleaseGroupRelease,
  RevisionDependencyCache,
  SignedURL,
  Team,
  TokenTypeResponse,
  UpdateReleaseRequest,
  UploadResponse,
 )
import Srclib.Types (Locator, SourceUnit, renderLocator)

-- Fetches an organization from the API
getOrganization ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  m Organization
getOrganization = do
  apiOpts <- ask
  -- TODO: This is a wrapper around FossaAPIV1 for now until more uses are
  -- migrated.
  API.getOrganization apiOpts

getTokenType ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  m TokenTypeResponse
getTokenType = do
  apiOpts <- ask
  API.getTokenType apiOpts

getCustomBuildPermissions ::
  ( Has (Lift IO) sig m
  , Has (Reader ApiOpts) sig m
  , Has Debug sig m
  , Has Diagnostics sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  m CustomBuildUploadPermissions
getCustomBuildPermissions revision metadata = do
  apiOpts <- ask
  API.getCustomBuildUploadPermissions apiOpts revision metadata

getProject ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
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
  , Has Debug sig m
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
  , Has Debug sig m
  , Has Diagnostics sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  [SourceUnit] ->
  m UploadResponse
uploadAnalysis revision metadata units = do
  apiOpts <- ask
  API.uploadAnalysis apiOpts revision metadata units

uploadAnalysisWithFirstPartyLicenses ::
  ( Has (Lift IO) sig m
  , Has (Reader ApiOpts) sig m
  , Has Debug sig m
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
  , Has Debug sig m
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
  , Has Debug sig m
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
  , Has Debug sig m
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
  , Has Debug sig m
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
  , Has Debug sig m
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
  , Has Debug sig m
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
  , Has Debug sig m
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
  , Has Debug sig m
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
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  m Text
getEndpointVersion = do
  apiOpts <- ask
  API.getEndpointVersion apiOpts

uploadReachabilityContent ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ByteString ->
  m Text
uploadReachabilityContent content = do
  apiOpts <- ask
  signedUrl <- API.getReachabilityContentSignedUrl apiOpts mempty
  API.uploadReachabilityContent signedUrl content

uploadReachabilityBuild ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  , ToJSON a
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  a ->
  m ()
uploadReachabilityBuild pr metadata content = do
  apiOpts <- ask
  signedUrl <- API.getReachabilityBuildSignedUrl apiOpts pr metadata
  void $ API.uploadReachabilityBuild signedUrl content

createReleaseGroup ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  CreateReleaseGroupRequest ->
  m CreateReleaseGroupResponse
createReleaseGroup req = do
  apiOpts <- ask
  API.createReleaseGroup apiOpts req

createReleaseGroupRelease ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Int ->
  ReleaseGroupReleaseRevision ->
  m ReleaseGroupRelease
createReleaseGroupRelease releaseGroupId req = do
  apiOpts <- ask
  API.createReleaseGroupRelease apiOpts releaseGroupId req

getPolicies ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  m [Policy]
getPolicies = do
  apiOpts <- ask
  API.getPolicies apiOpts

getTeams ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  m [Team]
getTeams = do
  apiOpts <- ask
  API.getTeams apiOpts

deleteReleaseGroup ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Int ->
  m ()
deleteReleaseGroup releaseGroupId = do
  apiOpts <- ask
  API.deleteReleaseGroup apiOpts releaseGroupId

deleteReleaseGroupRelease ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Int ->
  Int ->
  m ()
deleteReleaseGroupRelease releaseGroupId releaseId = do
  apiOpts <- ask
  API.deleteReleaseGroupRelease apiOpts releaseGroupId releaseId

updateReleaseGroupRelease ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Int ->
  Int ->
  UpdateReleaseRequest ->
  m ReleaseGroupRelease
updateReleaseGroupRelease releaseGroupId releaseId updateReq = do
  apiOpts <- ask
  API.updateReleaseGroupRelease apiOpts releaseGroupId releaseId updateReq

getReleaseGroups ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  m [ReleaseGroup]
getReleaseGroups = do
  apiOpts <- ask
  API.getReleaseGroups apiOpts

getReleaseGroupReleases ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Int ->
  m [ReleaseGroupRelease]
getReleaseGroupReleases releaseGroupId = do
  apiOpts <- ask
  API.getReleaseGroupReleases apiOpts releaseGroupId
