{-# LANGUAGE GADTs #-}

module Control.Effect.FossaApiClient (
  PackageRevision (..),
  FossaApiClientF (..),
  FossaApiClient,
  assertRevisionBinaries,
  assertUserDefinedBinaries,
  finalizeLicenseScan,
  getApiOpts,
  getAttribution,
  getIssues,
  getLatestBuild,
  getLatestScan,
  getOrganization,
  getProject,
  getScan,
  getSignedLicenseScanUrl,
  getSignedUploadUrl,
  queueArchiveBuild,
  resolveProjectDependencies,
  resolveUserDefinedBinary,
  uploadAnalysis,
  uploadArchive,
  uploadContainerScan,
  uploadContributors,
  uploadLicenseScanResult,
) where

import App.Fossa.Config.Report (ReportOutputFormat)
import App.Fossa.Container.Scan (ContainerScan (..))
import App.Fossa.VSI.Fingerprint (Fingerprint, Raw)
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import App.Types (ProjectMetadata, ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.Simple (Simple, sendSimple)
import Data.ByteString.Char8 qualified as C8
import Data.List.NonEmpty qualified as NE
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
  ScanId,
  ScanResponse,
  SignedURL,
  UploadResponse,
 )
import Network.HTTP.Req (LbsResponse)
import Srclib.Types (LicenseSourceUnit, Locator, SourceUnit)

-- | PackageRevisions are like ProjectRevisions, but they never have a branch.
data PackageRevision = PackageRevision
  { packageName :: Text
  , packageVersion :: Text
  }
  deriving (Show, Eq, Ord)

-- | The many operations available in the API effect.
-- Note: If you add an entry here, please add a corresponding entry in @Test.MockApi.matchExpectation@.
data FossaApiClientF a where
  AssertRevisionBinaries :: Locator -> [Fingerprint Raw] -> FossaApiClientF ()
  AssertUserDefinedBinaries :: IAT.UserDefinedAssertionMeta -> [Fingerprint Raw] -> FossaApiClientF ()
  FinalizeLicenseScan :: ArchiveComponents -> FossaApiClientF ()
  GetApiOpts :: FossaApiClientF ApiOpts
  GetAttribution :: ProjectRevision -> ReportOutputFormat -> FossaApiClientF Text
  GetIssues :: ProjectRevision -> FossaApiClientF Issues
  GetLatestBuild :: ProjectRevision -> FossaApiClientF Build
  GetLatestScan :: Locator -> ProjectRevision -> FossaApiClientF ScanResponse
  GetOrganization :: FossaApiClientF Organization
  GetProject :: ProjectRevision -> FossaApiClientF Project
  GetScan :: Locator -> ScanId -> FossaApiClientF ScanResponse
  GetSignedLicenseScanUrl :: PackageRevision -> FossaApiClientF SignedURL
  GetSignedUploadUrl :: PackageRevision -> FossaApiClientF SignedURL
  QueueArchiveBuild :: Archive -> FossaApiClientF (Maybe C8.ByteString)
  ResolveProjectDependencies :: VSI.Locator -> FossaApiClientF [VSI.Locator]
  ResolveUserDefinedBinary :: IAT.UserDep -> FossaApiClientF IAT.UserDefinedAssertionMeta
  UploadAnalysis ::
    ProjectRevision ->
    ProjectMetadata ->
    NE.NonEmpty SourceUnit ->
    FossaApiClientF UploadResponse
  UploadArchive :: SignedURL -> FilePath -> FossaApiClientF LbsResponse
  UploadContainerScan ::
    ProjectRevision ->
    ProjectMetadata ->
    ContainerScan ->
    FossaApiClientF UploadResponse
  UploadContributors ::
    Locator ->
    Contributors ->
    FossaApiClientF ()
  UploadLicenseScanResult :: SignedURL -> LicenseSourceUnit -> FossaApiClientF ()

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
uploadAnalysis :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> NE.NonEmpty SourceUnit -> m UploadResponse
uploadAnalysis revision metadata units = sendSimple (UploadAnalysis revision metadata units)

-- | Uploads results of container analysis to a project
uploadContainerScan :: (Has FossaApiClient sig m) => ProjectRevision -> ProjectMetadata -> ContainerScan -> m UploadResponse
uploadContainerScan revision metadata scan = sendSimple (UploadContainerScan revision metadata scan)

-- | Associates contributors to a specific locator
uploadContributors :: (Has FossaApiClient sig m) => Locator -> Contributors -> m ()
uploadContributors locator contributors = sendSimple $ UploadContributors locator contributors

getLatestBuild :: (Has FossaApiClient sig m) => ProjectRevision -> m Build
getLatestBuild = sendSimple . GetLatestBuild

getIssues :: (Has FossaApiClient sig m) => ProjectRevision -> m Issues
getIssues = sendSimple . GetIssues

getScan :: Has FossaApiClient sig m => Locator -> ScanId -> m ScanResponse
getScan locator scanId = sendSimple $ GetScan locator scanId

getLatestScan :: Has FossaApiClient sig m => Locator -> ProjectRevision -> m ScanResponse
getLatestScan locator revision = sendSimple $ GetLatestScan locator revision

getAttribution :: Has FossaApiClient sig m => ProjectRevision -> ReportOutputFormat -> m Text
getAttribution revision format = sendSimple $ GetAttribution revision format

getSignedUploadUrl :: Has FossaApiClient sig m => PackageRevision -> m SignedURL
getSignedUploadUrl = sendSimple . GetSignedUploadUrl

uploadArchive :: Has FossaApiClient sig m => SignedURL -> FilePath -> m LbsResponse
uploadArchive dest path = sendSimple (UploadArchive dest path)

queueArchiveBuild :: Has FossaApiClient sig m => Archive -> m (Maybe C8.ByteString)
queueArchiveBuild = sendSimple . QueueArchiveBuild

assertRevisionBinaries :: Has FossaApiClient sig m => Locator -> [Fingerprint Raw] -> m ()
assertRevisionBinaries locator fprints = sendSimple (AssertRevisionBinaries locator fprints)

assertUserDefinedBinaries :: Has FossaApiClient sig m => IAT.UserDefinedAssertionMeta -> [Fingerprint Raw] -> m ()
assertUserDefinedBinaries meta fprints = sendSimple (AssertUserDefinedBinaries meta fprints)

getSignedLicenseScanUrl :: Has FossaApiClient sig m => PackageRevision -> m SignedURL
getSignedLicenseScanUrl = sendSimple . GetSignedLicenseScanUrl

finalizeLicenseScan :: Has FossaApiClient sig m => ArchiveComponents -> m ()
finalizeLicenseScan = sendSimple . FinalizeLicenseScan

uploadLicenseScanResult :: Has FossaApiClient sig m => SignedURL -> LicenseSourceUnit -> m ()
uploadLicenseScanResult signedUrl licenseSourceUnit = sendSimple (UploadLicenseScanResult signedUrl licenseSourceUnit)

resolveUserDefinedBinary :: Has FossaApiClient sig m => IAT.UserDep -> m IAT.UserDefinedAssertionMeta
resolveUserDefinedBinary = sendSimple . ResolveUserDefinedBinary

resolveProjectDependencies :: Has FossaApiClient sig m => VSI.Locator -> m [VSI.Locator]
resolveProjectDependencies = sendSimple . ResolveProjectDependencies
