{-# LANGUAGE GADTs #-}

module Control.Carrier.FossaApiClient (FossaApiClientC, runFossaApiClient) where

import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.Core qualified as Core
import Control.Carrier.FossaApiClient.Internal.LicenseScanning qualified as LicenseScanning
import Control.Carrier.FossaApiClient.Internal.VSI qualified as VSI
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift, sendIO)
import Data.Default.Class (def)
import Fossa.API.Types (ApiOpts)
import Network.Connection (TLSSettings (TLSSettingsSimple, settingClientSupported))
import Network.HTTP.Client (Manager, ManagerSettings, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS (EMSMode (AllowEMS), Supported (supportedExtendedMainSecret))

-- TODO: Remove the ReaderC Manager layer.
-- This was created so that we can use AllowEMS for a few older servers that still require it.
-- As of 2024-10-24 we are free to revert this change, but it must stay until then.
-- After fixing the related errors, you should also be able to remove these deps from spectrometer.cabal:
--  1. crypton-connection
--  2. http-client-tls
--  3. data-default-class

-- | A carrier to run FOSSA API functions in the IO monad
type FossaApiClientC m = SimpleC FossaApiClientF (ReaderC Manager (ReaderC ApiOpts m))

emsTLSSettings :: TLSSettings
emsTLSSettings =
  case def of
    simple@TLSSettingsSimple{} -> simple{settingClientSupported = def{supportedExtendedMainSecret = AllowEMS}}
    otherSettings -> otherSettings

-- | Runs FossaAPI effects as IO operations
runFossaApiClient ::
  ( Has (Lift IO) sig m
  , Has Debug sig m
  , Has Diagnostics sig m
  ) =>
  ApiOpts ->
  FossaApiClientC m a ->
  m a
runFossaApiClient apiOpts action = do
  mgr <- sendIO $ newManager allowEMSManager
  runReader apiOpts
    . runReader mgr
    . interpreter
    $ action
  where
    allowEMSManager :: ManagerSettings
    allowEMSManager = mkManagerSettings emsTLSSettings Nothing

    interpreter =
      interpret
        ( \case
            AddFilesToVsiScan scanId files -> VSI.addFilesToVsiScan scanId files
            AssertRevisionBinaries locator fingerprints -> VSI.assertRevisionBinaries locator fingerprints
            AssertUserDefinedBinaries meta fingerprints -> VSI.assertUserDefinedBinaries meta fingerprints
            CompleteVsiScan scanId -> VSI.completeVsiScan scanId
            CreateVsiScan rev -> VSI.createVsiScan rev
            FinalizeLicenseScan components -> LicenseScanning.finalizeLicenseScan components
            FinalizeLicenseScanForPathDependency locators forceRebuild -> LicenseScanning.finalizePathDependencyScan locators forceRebuild
            GetApiOpts -> pure apiOpts
            GetAttribution rev format locType -> Core.getAttribution rev format locType
            GetIssues rev diffRev locatorType -> Core.getIssues rev diffRev locatorType
            GetEndpointVersion -> Core.getEndpointVersion
            GetLatestBuild rev locatorType -> Core.getLatestBuild rev locatorType
            GetRevisionDependencyCacheStatus rev locatorType -> Core.getRevisionDependencyCacheStatus rev locatorType
            GetOrganization -> Core.getOrganization
            GetPolicies -> Core.getPolicies
            GetProject rev locatorType -> Core.getProject rev locatorType
            GetTeams -> Core.getTeams
            AddTeamProjects teamId req -> Core.addTeamProjects teamId req
            GetAnalyzedRevisions vdeps -> Core.getAnalyzedRevisions vdeps
            GetSignedFirstPartyScanUrl rev -> LicenseScanning.getSignedFirstPartyScanUrl rev
            GetSignedLicenseScanUrl rev -> LicenseScanning.getSignedLicenseScanUrl rev
            GetSignedUploadUrl fileType rev -> Core.getSignedUploadUrl fileType rev
            GetPathDependencyScanUrl rev projectRevision uploadKind -> LicenseScanning.uploadPathDependencyScanResult rev projectRevision uploadKind
            GetVsiInferences scanId -> VSI.getVsiInferences scanId
            GetVsiScanAnalysisStatus scanId -> VSI.getVsiScanAnalysisStatus scanId
            QueueArchiveBuild archives rebuild -> Core.queueArchiveBuild archives rebuild
            QueueSBOMBuild archive team rebuild -> Core.queueSBOMBuild archive team rebuild
            ResolveProjectDependencies locator -> VSI.resolveProjectDependencies locator
            ResolveUserDefinedBinary deps -> VSI.resolveUserDefinedBinary deps
            UploadAnalysis rev metadata units ficusResults -> Core.uploadAnalysis rev metadata units ficusResults
            UploadAnalysisWithFirstPartyLicenses rev metadata uploadKind ficusResults -> Core.uploadAnalysisWithFirstPartyLicenses rev metadata uploadKind ficusResults
            UploadArchive url path -> Core.uploadArchive url path
            UploadNativeContainerScan revision metadata scan -> Core.uploadNativeContainerScan revision metadata scan
            UploadContributors locator contributors -> Core.uploadContributors locator contributors
            UploadLicenseScanResult signedUrl licenseSourceUnit -> LicenseScanning.uploadLicenseScanResult signedUrl licenseSourceUnit
            UploadFirstPartyScanResult signedUrl fullSourceUnits -> LicenseScanning.uploadFirstPartyScanResult signedUrl fullSourceUnits
            GetAnalyzedPathRevisions projectRevision -> LicenseScanning.alreadyAnalyzedPathRevision projectRevision
            -- Reachability
            UploadContentForReachability content -> Core.uploadReachabilityContent content
            UploadBuildForReachability rev metadata content -> Core.uploadReachabilityBuild rev metadata content
            GetTokenType -> Core.getTokenType
            GetCustomBuildPermissons rev metadata -> Core.getCustomBuildPermissions rev metadata
            -- Release Group
            DeleteReleaseGroup releaseGroupId -> Core.deleteReleaseGroup releaseGroupId
            DeleteReleaseGroupRelease releaseGroupId releaseId -> Core.deleteReleaseGroupRelease releaseGroupId releaseId
            UpdateReleaseGroupRelease releaseGroupId releaseId updateReq -> Core.updateReleaseGroupRelease releaseGroupId releaseId updateReq
            GetReleaseGroups -> Core.getReleaseGroups
            GetReleaseGroupReleases releaseGroupId -> Core.getReleaseGroupReleases releaseGroupId
            CreateReleaseGroup req -> Core.createReleaseGroup req
            -- Project
            GetProjectV2 locator -> Core.getProjectV2 locator
            UpdateProject locator req -> Core.updateProject locator req
            -- Revision
            UpdateRevision revisionLocator req -> Core.updateRevision revisionLocator req
            -- Labels
            GetOrgLabels -> Core.getOrgLabels
            CreateReleaseGroupRelease releaseGroupId req -> Core.createReleaseGroupRelease releaseGroupId req
        )
