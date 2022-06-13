{-# LANGUAGE GADTs #-}

module Control.Carrier.FossaApiClient (FossaApiClientC, runFossaApiClient) where

import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.Core qualified as Core
import Control.Carrier.FossaApiClient.Internal.LicenseScanning qualified as LicenseScanning
import Control.Carrier.FossaApiClient.Internal.ScotlandYard qualified as ScotlandYard
import Control.Carrier.FossaApiClient.Internal.VSI qualified as VSI
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Fossa.API.Types (ApiOpts)

-- | A carrier to run FOSSA API functions in the IO monad
type FossaApiClientC m = SimpleC FossaApiClientF (ReaderC ApiOpts m)

-- | Runs FossaAPI effects as IO operations
runFossaApiClient ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  ApiOpts ->
  FossaApiClientC m a ->
  m a
runFossaApiClient apiOpts =
  runReader apiOpts
    . interpret
      ( \case
          AddFilesToVsiScan scanId files -> VSI.addFilesToVsiScan scanId files
          AssertRevisionBinaries locator fingerprints -> VSI.assertRevisionBinaries locator fingerprints
          AssertUserDefinedBinaries meta fingerprints -> VSI.assertUserDefinedBinaries meta fingerprints
          CompleteVsiScan scanId -> VSI.completeVsiScan scanId
          CreateVsiScan rev -> VSI.createVsiScan rev
          FinalizeLicenseScan components -> LicenseScanning.finalizeLicenseScan components
          GetApiOpts -> pure apiOpts
          GetAttribution rev format -> Core.getAttribution rev format
          GetIssues rev -> Core.getIssues rev
          GetLatestBuild rev -> Core.getLatestBuild rev
          GetLatestScan locator rev -> ScotlandYard.getLatestScan locator rev
          GetOrganization -> Core.getOrganization
          GetProject rev -> Core.getProject rev
          GetAnalyzedRevisions vdeps -> Core.getAnalyzedRevisions vdeps
          GetScan locator scanId -> ScotlandYard.getScan locator scanId
          GetSignedLicenseScanUrl rev -> LicenseScanning.getSignedLicenseScanUrl rev
          GetSignedUploadUrl rev -> Core.getSignedUploadUrl rev
          GetVsiInferences scanId -> VSI.getVsiInferences scanId
          GetVsiScanAnalysisStatus scanId -> VSI.getVsiScanAnalysisStatus scanId
          QueueArchiveBuild archive -> Core.queueArchiveBuild archive
          ResolveProjectDependencies locator -> VSI.resolveProjectDependencies locator
          ResolveUserDefinedBinary deps -> VSI.resolveUserDefinedBinary deps
          UploadAnalysis rev metadata units -> Core.uploadAnalysis rev metadata units
          UploadArchive url path -> Core.uploadArchive url path
          UploadContainerScan revision metadata scan -> Core.uploadContainerScan revision metadata scan
          UploadContributors locator contributors -> Core.uploadContributors locator contributors
          UploadLicenseScanResult signedUrl licenseSourceUnit -> LicenseScanning.uploadLicenseScanResult signedUrl licenseSourceUnit
      )
