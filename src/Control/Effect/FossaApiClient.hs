{-# LANGUAGE GADTs #-}

module Control.Effect.FossaApiClient (
  PackageRevision (..),
  FossaApiClientF (..),
  FossaApiClient,
  addFilesToVsiScan,
  assertRevisionBinaries,
  assertUserDefinedBinaries,
  completeVsiScan,
  createVsiScan,
  finalizeLicenseScan,
  getApiOpts,
  getAttribution,
  getIssues,
  getLatestBuild,
  getEndpointVersion,
  getOrganization,
  getProject,
  getRevisionDependencyCacheStatus,
  getAnalyzedRevisions,
<<<<<<< HEAD
=======
  getScan,
  getSignedFirstPartyScanUrl,
<<<<<<< HEAD
=======
>>>>>>> add getSignedFirstPartyScanUrl and use it to upload first-party results to the right spot
>>>>>>> ane-966-first-party-scans
  getSignedLicenseScanUrl,
  getSignedUploadUrl,
  getVsiInferences,
  getVsiScanAnalysisStatus,
  queueArchiveBuild,
  resolveProjectDependencies,
  resolveUserDefinedBinary,
  uploadAnalysis,
  uploadAnalysisWithFirstPartyLicenses,
  uploadArchive,
  uploadNativeContainerScan,
  uploadContributors,
  uploadLicenseScanResult,
  uploadFirstPartyScanResult,
) where

import App.Fossa.Config.Report (ReportOutputFormat)
import App.Fossa.Config.Test (DiffRevision)
import App.Fossa.VSI.Fingerprint (Fingerprint, Raw)
import App.Fossa.VSI.Fingerprint qualified as Fingerprint
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import App.Fossa.VendoredDependency (VendoredDependency)
import App.Types (FullFileUploads, ProjectMetadata, ProjectRevision)
import Container.Types qualified as NativeContainer
import Control.Algebra (Has)
import Control.Carrier.Simple (Simple, sendSimple)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Text (Text)
import Fossa.API.Types (
  ApiOpts,
  Archive,
  ArchiveComponents,
  Build,
  Contributors,
  Issues,
  Organization,
  Project,
  RevisionDependencyCache,
  SignedURL,
  UploadResponse,
 )
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
  AssertRevisionBinaries :: Locator -> [Fingerprint Raw] -> FossaApiClientF ()
  AssertUserDefinedBinaries :: IAT.UserDefinedAssertionMeta -> [Fingerprint Raw] -> FossaApiClientF ()
  CompleteVsiScan :: VSI.ScanID -> FossaApiClientF ()
  CreateVsiScan :: ProjectRevision -> FossaApiClientF VSI.ScanID
  FinalizeLicenseScan :: ArchiveComponents -> FossaApiClientF ()
  GetApiOpts :: FossaApiClientF ApiOpts
  GetAttribution :: ProjectRevision -> ReportOutputFormat -> FossaApiClientF Text
  GetIssues :: ProjectRevision -> Maybe DiffRevision -> FossaApiClientF Issues
  GetEndpointVersion :: FossaApiClientF Text
  GetRevisionDependencyCacheStatus :: ProjectRevision -> FossaApiClientF RevisionDependencyCache
  GetLatestBuild :: ProjectRevision -> FossaApiClientF Build
  GetOrganization :: FossaApiClientF Organization
  GetProject :: ProjectRevision -> FossaApiClientF Project
  GetAnalyzedRevisions :: NonEmpty VendoredDependency -> FossaApiClientF [Text]
  GetSignedFirstPartyScanUrl :: PackageRevision -> FossaApiClientF SignedURL
  GetSignedLicenseScanUrl :: PackageRevision -> FossaApiClientF SignedURL
  GetSignedUploadUrl :: PackageRevision -> FossaApiClientF SignedURL
  GetVsiInferences :: VSI.ScanID -> FossaApiClientF [Locator]
  GetVsiScanAnalysisStatus :: VSI.ScanID -> FossaApiClientF VSI.AnalysisStatus
  QueueArchiveBuild :: Archive -> FossaApiClientF (Maybe C8.ByteString)
  ResolveProjectDependencies :: VSI.Locator -> FossaApiClientF [VSI.Locator]
  ResolveUserDefinedBinary :: IAT.UserDep -> FossaApiClientF IAT.UserDefinedAssertionMeta
  UploadAnalysis ::
    ProjectRevision ->
    ProjectMetadata ->
    NonEmpty SourceUnit ->
    FossaApiClientF UploadResponse
  UploadAnalysisWithFirstPartyLicenses ::
    ProjectRevision ->
    ProjectMetadata ->
    FullFileUploads ->
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

deriving instance Show (FossaApiClientF a)

deriving instance Eq (FossaApiClientF a)

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
uploadAnalysis :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> NonEmpty SourceUnit -> m UploadResponse
uploadAnalysis revision metadata units = sendSimple (UploadAnalysis revision metadata units)

-- | Uploads the results of a first-party analysis and associates it to a project
uploadAnalysisWithFirstPartyLicenses :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> FullFileUploads -> m UploadResponse
uploadAnalysisWithFirstPartyLicenses revision metadata fullFileUploads = sendSimple (UploadAnalysisWithFirstPartyLicenses revision metadata fullFileUploads)

-- | Uploads results of container analysis performed by native scanner to a project
uploadNativeContainerScan :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> NativeContainer.ContainerScan -> m UploadResponse
uploadNativeContainerScan revision metadata scan = sendSimple (UploadNativeContainerScan revision metadata scan)

-- | Associates contributors to a specific locator
uploadContributors :: (Has FossaApiClient sig m) => Locator -> Contributors -> m ()
uploadContributors locator contributors = sendSimple $ UploadContributors locator contributors

getLatestBuild :: (Has FossaApiClient sig m) => ProjectRevision -> m Build
getLatestBuild = sendSimple . GetLatestBuild

getRevisionDependencyCacheStatus :: (Has FossaApiClient sig m) => ProjectRevision -> m RevisionDependencyCache
getRevisionDependencyCacheStatus = sendSimple . GetRevisionDependencyCacheStatus

getIssues :: (Has FossaApiClient sig m) => ProjectRevision -> Maybe DiffRevision -> m Issues
getIssues projectRevision diffRevision = sendSimple $ GetIssues projectRevision diffRevision

getAttribution :: Has FossaApiClient sig m => ProjectRevision -> ReportOutputFormat -> m Text
getAttribution revision format = sendSimple $ GetAttribution revision format

getSignedUploadUrl :: Has FossaApiClient sig m => PackageRevision -> m SignedURL
getSignedUploadUrl = sendSimple . GetSignedUploadUrl

uploadArchive :: Has FossaApiClient sig m => SignedURL -> FilePath -> m ByteString
uploadArchive dest path = sendSimple (UploadArchive dest path)

queueArchiveBuild :: Has FossaApiClient sig m => Archive -> m (Maybe C8.ByteString)
queueArchiveBuild = sendSimple . QueueArchiveBuild

assertRevisionBinaries :: Has FossaApiClient sig m => Locator -> [Fingerprint Raw] -> m ()
assertRevisionBinaries locator fprints = sendSimple (AssertRevisionBinaries locator fprints)

assertUserDefinedBinaries :: Has FossaApiClient sig m => IAT.UserDefinedAssertionMeta -> [Fingerprint Raw] -> m ()
assertUserDefinedBinaries meta fprints = sendSimple (AssertUserDefinedBinaries meta fprints)

getAnalyzedRevisions :: Has FossaApiClient sig m => NonEmpty VendoredDependency -> m ([Text])
getAnalyzedRevisions = sendSimple . GetAnalyzedRevisions

getSignedFirstPartyScanUrl :: Has FossaApiClient sig m => PackageRevision -> m SignedURL
getSignedFirstPartyScanUrl = sendSimple . GetSignedFirstPartyScanUrl

getSignedLicenseScanUrl :: Has FossaApiClient sig m => PackageRevision -> m SignedURL
getSignedLicenseScanUrl = sendSimple . GetSignedLicenseScanUrl

finalizeLicenseScan :: Has FossaApiClient sig m => ArchiveComponents -> m ()
finalizeLicenseScan = sendSimple . FinalizeLicenseScan

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

getVsiInferences :: Has FossaApiClient sig m => VSI.ScanID -> m [Locator]
getVsiInferences = sendSimple . GetVsiInferences

getEndpointVersion :: Has FossaApiClient sig m => m Text
getEndpointVersion = sendSimple GetEndpointVersion
