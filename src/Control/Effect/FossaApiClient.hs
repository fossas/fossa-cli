{-# LANGUAGE GADTs #-}

module Control.Effect.FossaApiClient (
  ArchiveRevision (..),
  FossaApiClientF (..),
  FossaApiClient,
  getApiOpts,
  getIssues,
  getLatestBuild,
  getLatestScan,
  getOrganization,
  getProject,
  getScan,
  getSignedUploadUrl,
  queueArchiveBuild,
  uploadAnalysis,
  uploadArchive,
  uploadContainerScan,
  uploadContributors,
) where

import App.Fossa.Container.Scan (ContainerScan (..))
import App.Types (ProjectMetadata, ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.Simple (Simple, sendSimple)
import Data.ByteString.Char8 qualified as C8
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
  ScanId,
  ScanResponse,
  SignedURL,
  UploadResponse,
 )
import Network.HTTP.Req (LbsResponse)
import Srclib.Types (Locator, SourceUnit)

data ArchiveRevision = ArchiveRevision
  { archiveName :: Text
  , archiveRevision :: Text
  }
  deriving (Show, Eq, Ord)

data FossaApiClientF a where
  GetApiOpts :: FossaApiClientF ApiOpts
  GetIssues :: ProjectRevision -> FossaApiClientF Issues
  GetLatestBuild :: ProjectRevision -> FossaApiClientF Build
  GetLatestScan :: Locator -> ProjectRevision -> FossaApiClientF ScanResponse
  GetOrganization :: FossaApiClientF Organization
  GetProject :: ProjectRevision -> FossaApiClientF Project
  GetScan :: Locator -> ScanId -> FossaApiClientF ScanResponse
  GetSignedUploadUrl :: ArchiveRevision -> FossaApiClientF SignedURL
  QueueArchiveBuild :: Archive -> FossaApiClientF (Maybe C8.ByteString)
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

getSignedUploadUrl :: Has FossaApiClient sig m => ArchiveRevision -> m SignedURL
getSignedUploadUrl = sendSimple . GetSignedUploadUrl

uploadArchive :: Has FossaApiClient sig m => SignedURL -> FilePath -> m LbsResponse
uploadArchive dest path = sendSimple (UploadArchive dest path)

queueArchiveBuild :: Has FossaApiClient sig m => Archive -> m (Maybe C8.ByteString)
queueArchiveBuild = sendSimple . QueueArchiveBuild
