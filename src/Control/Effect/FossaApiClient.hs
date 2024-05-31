{-# LANGUAGE GADTs #-}

module Control.Effect.FossaApiClient (
  PackageRevision (..),
  FossaApiClientF (..),
  FossaApiClient,
  addFilesToVsiScan,
  addTeamProjects,
  assertRevisionBinaries,
  assertUserDefinedBinaries,
  completeVsiScan,
  createVsiScan,
  finalizeLicenseScan,
  finalizeLicenseScanForPathDependency,
  getApiOpts,
  getAttribution,
  getIssues,
  getLatestBuild,
  getEndpointVersion,
  getOrganization,
  getProject,
  getRevisionDependencyCacheStatus,
  getAnalyzedRevisions,
  getSignedFirstPartyScanUrl,
  getSignedLicenseScanUrl,
  uploadPathDependencyScan,
  getSignedUploadUrl,
  getVsiInferences,
  getVsiScanAnalysisStatus,
  queueArchiveBuild,
  queueSBOMBuild,
  resolveProjectDependencies,
  resolveUserDefinedBinary,
  uploadAnalysis,
  uploadAnalysisWithFirstPartyLicenses,
  uploadArchive,
  uploadNativeContainerScan,
  uploadContributors,
  uploadLicenseScanResult,
  uploadFirstPartyScanResult,
  getAnalyzedPathRevisions,
  getTokenType,
  uploadContentForReachability,
  uploadBuildForReachability,
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
  getProjectV2,
  updateProject,
  updateRevision,
  getOrgLabels,
) where

import App.Fossa.Config.Report (ReportOutputFormat)
import App.Fossa.Config.Test (DiffRevision)
import App.Fossa.Reachability.Types (SourceUnitReachability)
import App.Fossa.VSI.Fingerprint (Fingerprint, Raw)
import App.Fossa.VSI.Fingerprint qualified as Fingerprint
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import App.Fossa.VendoredDependency (VendoredDependency)
import App.Types (ComponentUploadFileType, DependencyRebuild, FileUpload, LocatorType, ProjectMetadata, ProjectRevision, ReleaseGroupReleaseRevision)
import Container.Types qualified as NativeContainer
import Control.Algebra (Has)
import Control.Carrier.Simple (Simple, sendSimple)
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Text (Text)
import Fossa.API.Types (
  AnalyzedPathDependency,
  ApiOpts,
  Archive,
  ArchiveComponents,
  Build,
  Contributors,
  CustomBuildUploadPermissions,
  Issues,
  Organization,
  PathDependencyUpload,
  Project,
  RevisionDependencyCache,
  SignedURL,
  TokenTypeResponse,
  UploadResponse,
 )

import Fossa.API.CoreTypes qualified as CoreTypes

import Path (File, Path, Rel)
import Srclib.Types (FullSourceUnit, LicenseSourceUnit, Locator, SourceUnit)

-- | PackageRevisions are like ProjectRevisions, but they never have a branch.
data PackageRevision = PackageRevision
  { packageName :: Text
  , packageVersion :: Text
  }
  deriving (Show, Eq, Ord)

-- | The many operations available in the API effect.
-- Note: If you add an entry here, please add a corresponding entry in @Test.MockApi.matchExpectation@.
data FossaApiClientF a where
  AddFilesToVsiScan :: VSI.ScanID -> Map (Path Rel File) Fingerprint.Combined -> FossaApiClientF ()
  AddTeamProjects :: Int -> CoreTypes.AddTeamProjectsRequest -> FossaApiClientF CoreTypes.AddTeamProjectsResponse
  AssertRevisionBinaries :: Locator -> [Fingerprint Raw] -> FossaApiClientF ()
  AssertUserDefinedBinaries :: IAT.UserDefinedAssertionMeta -> [Fingerprint Raw] -> FossaApiClientF ()
  CompleteVsiScan :: VSI.ScanID -> FossaApiClientF ()
  CreateVsiScan :: ProjectRevision -> FossaApiClientF VSI.ScanID
  FinalizeLicenseScan :: ArchiveComponents -> FossaApiClientF ()
  FinalizeLicenseScanForPathDependency :: [Locator] -> Bool -> FossaApiClientF ()
  GetApiOpts :: FossaApiClientF ApiOpts
  GetAttribution :: ProjectRevision -> ReportOutputFormat -> FossaApiClientF Text
  GetCustomBuildPermissons ::
    ProjectRevision ->
    ProjectMetadata ->
    FossaApiClientF CustomBuildUploadPermissions
  GetIssues :: ProjectRevision -> Maybe DiffRevision -> LocatorType -> FossaApiClientF Issues
  GetEndpointVersion :: FossaApiClientF Text
  GetRevisionDependencyCacheStatus :: ProjectRevision -> FossaApiClientF RevisionDependencyCache
  GetLatestBuild :: ProjectRevision -> LocatorType -> FossaApiClientF Build
  GetOrganization :: FossaApiClientF Organization
  GetPolicies :: FossaApiClientF [CoreTypes.Policy]
  GetProject :: ProjectRevision -> LocatorType -> FossaApiClientF Project
  GetTeams :: FossaApiClientF [CoreTypes.Team]
  GetAnalyzedRevisions :: NonEmpty VendoredDependency -> FossaApiClientF [Text]
  GetAnalyzedPathRevisions :: ProjectRevision -> FossaApiClientF [AnalyzedPathDependency]
  GetSignedFirstPartyScanUrl :: PackageRevision -> FossaApiClientF SignedURL
  GetSignedLicenseScanUrl :: PackageRevision -> FossaApiClientF SignedURL
  GetPathDependencyScanUrl :: PackageRevision -> ProjectRevision -> FileUpload -> FossaApiClientF PathDependencyUpload
  GetSignedUploadUrl :: ComponentUploadFileType -> PackageRevision -> FossaApiClientF SignedURL
  GetTokenType :: FossaApiClientF TokenTypeResponse
  GetVsiInferences :: VSI.ScanID -> FossaApiClientF VSI.VsiExportedInferencesBody
  GetVsiScanAnalysisStatus :: VSI.ScanID -> FossaApiClientF VSI.AnalysisStatus
  QueueArchiveBuild :: [Archive] -> DependencyRebuild -> FossaApiClientF ()
  QueueSBOMBuild :: Archive -> Maybe Text -> DependencyRebuild -> FossaApiClientF ()
  ResolveProjectDependencies :: VSI.Locator -> FossaApiClientF [VSI.Locator]
  ResolveUserDefinedBinary :: IAT.UserDep -> FossaApiClientF IAT.UserDefinedAssertionMeta
  UploadAnalysis ::
    ProjectRevision ->
    ProjectMetadata ->
    [SourceUnit] ->
    FossaApiClientF UploadResponse
  UploadAnalysisWithFirstPartyLicenses ::
    ProjectRevision ->
    ProjectMetadata ->
    FileUpload ->
    FossaApiClientF UploadResponse
  UploadArchive :: SignedURL -> FilePath -> FossaApiClientF ByteString
  UploadNativeContainerScan ::
    ProjectRevision ->
    ProjectMetadata ->
    NativeContainer.ContainerScan ->
    FossaApiClientF UploadResponse
  UploadContributors ::
    Locator ->
    Contributors ->
    FossaApiClientF ()
  UploadLicenseScanResult :: SignedURL -> LicenseSourceUnit -> FossaApiClientF ()
  UploadFirstPartyScanResult :: SignedURL -> NE.NonEmpty FullSourceUnit -> FossaApiClientF ()
  UploadContentForReachability :: ByteString -> FossaApiClientF (Text)
  UploadBuildForReachability :: ProjectRevision -> ProjectMetadata -> [SourceUnitReachability] -> FossaApiClientF ()
  DeleteReleaseGroup :: Int -> FossaApiClientF ()
  DeleteReleaseGroupRelease :: Int -> Int -> FossaApiClientF ()
  CreateReleaseGroup :: CoreTypes.CreateReleaseGroupRequest -> FossaApiClientF CoreTypes.CreateReleaseGroupResponse
  CreateReleaseGroupRelease :: Int -> ReleaseGroupReleaseRevision -> FossaApiClientF CoreTypes.ReleaseGroupRelease
  UpdateReleaseGroupRelease :: Int -> Int -> CoreTypes.UpdateReleaseRequest -> FossaApiClientF CoreTypes.ReleaseGroupRelease
  GetReleaseGroups :: FossaApiClientF [CoreTypes.ReleaseGroup]
  GetReleaseGroupReleases :: Int -> FossaApiClientF [CoreTypes.ReleaseGroupRelease]
  GetProjectV2 :: Text -> FossaApiClientF CoreTypes.Project
  UpdateProject :: Text -> CoreTypes.UpdateProjectRequest -> FossaApiClientF CoreTypes.Project
  UpdateRevision :: Text -> CoreTypes.UpdateRevisionRequest -> FossaApiClientF CoreTypes.Revision
  GetOrgLabels :: FossaApiClientF CoreTypes.Labels

deriving instance Show (FossaApiClientF a)

deriving instance Eq (FossaApiClientF a)

type FossaApiClient = Simple FossaApiClientF

-- | Fetches the organization associated with the current API token
getOrganization :: (Has FossaApiClient sig m) => m Organization
getOrganization = sendSimple GetOrganization

-- | Fetches the project associated with a revision
getProject :: (Has FossaApiClient sig m) => ProjectRevision -> LocatorType -> m Project
getProject revision locatorType = sendSimple $ GetProject revision locatorType

-- | Returns the API options currently in scope.
-- The API options contain a lot of information required to build URLs.
getApiOpts :: (Has FossaApiClient sig m) => m ApiOpts
getApiOpts = sendSimple GetApiOpts

-- | Uploads the results of an analysis and associates it to a project
uploadAnalysis :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> [SourceUnit] -> m UploadResponse
uploadAnalysis revision metadata units = sendSimple (UploadAnalysis revision metadata units)

-- | Uploads the results of a first-party analysis and associates it to a project
uploadAnalysisWithFirstPartyLicenses :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> FileUpload -> m UploadResponse
uploadAnalysisWithFirstPartyLicenses revision metadata uploadKind = sendSimple (UploadAnalysisWithFirstPartyLicenses revision metadata uploadKind)

-- | Uploads results of container analysis performed by native scanner to a project
uploadNativeContainerScan :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> NativeContainer.ContainerScan -> m UploadResponse
uploadNativeContainerScan revision metadata scan = sendSimple (UploadNativeContainerScan revision metadata scan)

-- | Associates contributors to a specific locator
uploadContributors :: (Has FossaApiClient sig m) => Locator -> Contributors -> m ()
uploadContributors locator contributors = sendSimple $ UploadContributors locator contributors

getLatestBuild :: (Has FossaApiClient sig m) => ProjectRevision -> LocatorType -> m Build
getLatestBuild rev locatorType = sendSimple $ GetLatestBuild rev locatorType

getRevisionDependencyCacheStatus :: (Has FossaApiClient sig m) => ProjectRevision -> m RevisionDependencyCache
getRevisionDependencyCacheStatus = sendSimple . GetRevisionDependencyCacheStatus

getIssues :: (Has FossaApiClient sig m) => ProjectRevision -> Maybe DiffRevision -> LocatorType -> m Issues
getIssues projectRevision diffRevision locatorType = sendSimple $ GetIssues projectRevision diffRevision locatorType

getAttribution :: Has FossaApiClient sig m => ProjectRevision -> ReportOutputFormat -> m Text
getAttribution revision format = sendSimple $ GetAttribution revision format

getSignedUploadUrl :: Has FossaApiClient sig m => ComponentUploadFileType -> PackageRevision -> m SignedURL
getSignedUploadUrl fileType packageSpec = sendSimple $ GetSignedUploadUrl fileType packageSpec

uploadArchive :: Has FossaApiClient sig m => SignedURL -> FilePath -> m ByteString
uploadArchive dest path = sendSimple (UploadArchive dest path)

queueArchiveBuild :: Has FossaApiClient sig m => [Archive] -> DependencyRebuild -> m ()
queueArchiveBuild archives rebuild = sendSimple (QueueArchiveBuild archives rebuild)

queueSBOMBuild :: Has FossaApiClient sig m => Archive -> Maybe Text -> DependencyRebuild -> m ()
queueSBOMBuild archive team rebuild = sendSimple (QueueSBOMBuild archive team rebuild)

assertRevisionBinaries :: Has FossaApiClient sig m => Locator -> [Fingerprint Raw] -> m ()
assertRevisionBinaries locator fprints = sendSimple (AssertRevisionBinaries locator fprints)

assertUserDefinedBinaries :: Has FossaApiClient sig m => IAT.UserDefinedAssertionMeta -> [Fingerprint Raw] -> m ()
assertUserDefinedBinaries meta fprints = sendSimple (AssertUserDefinedBinaries meta fprints)

getAnalyzedRevisions :: Has FossaApiClient sig m => NonEmpty VendoredDependency -> m ([Text])
getAnalyzedRevisions = sendSimple . GetAnalyzedRevisions

getAnalyzedPathRevisions :: Has FossaApiClient sig m => ProjectRevision -> m [AnalyzedPathDependency]
getAnalyzedPathRevisions = sendSimple . GetAnalyzedPathRevisions

getSignedFirstPartyScanUrl :: Has FossaApiClient sig m => PackageRevision -> m SignedURL
getSignedFirstPartyScanUrl = sendSimple . GetSignedFirstPartyScanUrl

getSignedLicenseScanUrl :: Has FossaApiClient sig m => PackageRevision -> m SignedURL
getSignedLicenseScanUrl = sendSimple . GetSignedLicenseScanUrl

uploadPathDependencyScan :: Has FossaApiClient sig m => PackageRevision -> ProjectRevision -> FileUpload -> m PathDependencyUpload
uploadPathDependencyScan pkgRev projectRevision uploadKind = sendSimple $ GetPathDependencyScanUrl pkgRev projectRevision uploadKind

finalizeLicenseScan :: Has FossaApiClient sig m => ArchiveComponents -> m ()
finalizeLicenseScan = sendSimple . FinalizeLicenseScan

finalizeLicenseScanForPathDependency :: Has FossaApiClient sig m => [Locator] -> Bool -> m ()
finalizeLicenseScanForPathDependency locators forceRebuild = sendSimple $ FinalizeLicenseScanForPathDependency locators forceRebuild

uploadLicenseScanResult :: Has FossaApiClient sig m => SignedURL -> LicenseSourceUnit -> m ()
uploadLicenseScanResult signedUrl licenseSourceUnit = sendSimple (UploadLicenseScanResult signedUrl licenseSourceUnit)

uploadFirstPartyScanResult :: Has FossaApiClient sig m => SignedURL -> NE.NonEmpty FullSourceUnit -> m ()
uploadFirstPartyScanResult signedUrl fullSourceUnits = sendSimple (UploadFirstPartyScanResult signedUrl fullSourceUnits)

resolveUserDefinedBinary :: Has FossaApiClient sig m => IAT.UserDep -> m IAT.UserDefinedAssertionMeta
resolveUserDefinedBinary = sendSimple . ResolveUserDefinedBinary

resolveProjectDependencies :: Has FossaApiClient sig m => VSI.Locator -> m [VSI.Locator]
resolveProjectDependencies = sendSimple . ResolveProjectDependencies

createVsiScan :: Has FossaApiClient sig m => ProjectRevision -> m VSI.ScanID
createVsiScan = sendSimple . CreateVsiScan

addFilesToVsiScan :: Has FossaApiClient sig m => VSI.ScanID -> Map (Path Rel File) Fingerprint.Combined -> m ()
addFilesToVsiScan scanId files = sendSimple (AddFilesToVsiScan scanId files)

completeVsiScan :: Has FossaApiClient sig m => VSI.ScanID -> m ()
completeVsiScan = sendSimple . CompleteVsiScan

getVsiScanAnalysisStatus :: Has FossaApiClient sig m => VSI.ScanID -> m VSI.AnalysisStatus
getVsiScanAnalysisStatus = sendSimple . GetVsiScanAnalysisStatus

getVsiInferences :: Has FossaApiClient sig m => VSI.ScanID -> m VSI.VsiExportedInferencesBody
getVsiInferences = sendSimple . GetVsiInferences

getEndpointVersion :: Has FossaApiClient sig m => m Text
getEndpointVersion = sendSimple GetEndpointVersion

getTokenType :: Has FossaApiClient sig m => m TokenTypeResponse
getTokenType = sendSimple GetTokenType

getCustomBuildPermissions :: Has FossaApiClient sig m => ProjectRevision -> ProjectMetadata -> m CustomBuildUploadPermissions
getCustomBuildPermissions revision metadata = sendSimple $ GetCustomBuildPermissons revision metadata

uploadContentForReachability :: Has FossaApiClient sig m => ByteString -> m Text
uploadContentForReachability = sendSimple . UploadContentForReachability

uploadBuildForReachability :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> [SourceUnitReachability] -> m ()
uploadBuildForReachability rev revMetadata units = sendSimple $ UploadBuildForReachability rev revMetadata units

getPolicies :: Has FossaApiClient sig m => m [CoreTypes.Policy]
getPolicies = sendSimple GetPolicies

getTeams :: Has FossaApiClient sig m => m [CoreTypes.Team]
getTeams = sendSimple GetTeams

addTeamProjects :: Has FossaApiClient sig m => Int -> CoreTypes.AddTeamProjectsRequest -> m CoreTypes.AddTeamProjectsResponse
addTeamProjects teamId req = sendSimple $ AddTeamProjects teamId req

deleteReleaseGroup :: Has FossaApiClient sig m => Int -> m ()
deleteReleaseGroup releaseGroupId = sendSimple $ DeleteReleaseGroup releaseGroupId

deleteReleaseGroupRelease :: Has FossaApiClient sig m => Int -> Int -> m ()
deleteReleaseGroupRelease releaseGroupId releaseId = sendSimple $ DeleteReleaseGroupRelease releaseGroupId releaseId

updateReleaseGroupRelease :: Has FossaApiClient sig m => Int -> Int -> CoreTypes.UpdateReleaseRequest -> m CoreTypes.ReleaseGroupRelease
updateReleaseGroupRelease releaseGroupId releaseId updateReq = sendSimple $ UpdateReleaseGroupRelease releaseGroupId releaseId updateReq

createReleaseGroup :: Has FossaApiClient sig m => CoreTypes.CreateReleaseGroupRequest -> m CoreTypes.CreateReleaseGroupResponse
createReleaseGroup req = sendSimple $ CreateReleaseGroup req

createReleaseGroupRelease :: Has FossaApiClient sig m => Int -> ReleaseGroupReleaseRevision -> m CoreTypes.ReleaseGroupRelease
createReleaseGroupRelease releaseGroupId req = sendSimple $ CreateReleaseGroupRelease releaseGroupId req

getReleaseGroups :: Has FossaApiClient sig m => m [CoreTypes.ReleaseGroup]
getReleaseGroups = sendSimple GetReleaseGroups

getReleaseGroupReleases :: Has FossaApiClient sig m => Int -> m [CoreTypes.ReleaseGroupRelease]
getReleaseGroupReleases releaseGroupId = sendSimple $ GetReleaseGroupReleases releaseGroupId

getProjectV2 :: Has FossaApiClient sig m => Text -> m CoreTypes.Project
getProjectV2 locator = sendSimple $ GetProjectV2 locator

updateProject :: Has FossaApiClient sig m => Text -> CoreTypes.UpdateProjectRequest -> m CoreTypes.Project
updateProject locator req = sendSimple $ UpdateProject locator req

updateRevision :: Has FossaApiClient sig m => Text -> CoreTypes.UpdateRevisionRequest -> m CoreTypes.Revision
updateRevision revisionLocator req = sendSimple $ UpdateRevision revisionLocator req

getOrgLabels :: Has FossaApiClient sig m => m CoreTypes.Labels
getOrgLabels = sendSimple GetOrgLabels
