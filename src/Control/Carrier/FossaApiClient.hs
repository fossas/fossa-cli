{-# LANGUAGE GADTs #-}

module Control.Carrier.FossaApiClient (FossaApiClientC, runFossaApiClient) where

import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.Core qualified as Core
import Control.Carrier.FossaApiClient.Internal.ScotlandYard qualified as ScotlandYard
import Control.Carrier.FossaApiClient.Internal.VSI qualified as VSI
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Fossa.API.Types (ApiOpts)

-- | A carrier to run Fossa API functions in the IO monad
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
          AssertRevisionBinaries locator fingerprints -> VSI.assertRevisionBinaries locator fingerprints
          AssertUserDefinedBinaries meta fingerprints -> VSI.assertUserDefinedBinaries meta fingerprints
          GetApiOpts -> pure apiOpts
          GetAttribution rev format -> Core.getAttribution rev format
          GetIssues rev -> Core.getIssues rev
          GetLatestBuild rev -> Core.getLatestBuild rev
          GetLatestScan locator rev -> ScotlandYard.getLatestScan locator rev
          GetOrganization -> Core.getOrganization
          GetProject rev -> Core.getProject rev
          GetScan locator scanId -> ScotlandYard.getScan locator scanId
          GetSignedUploadUrl rev -> Core.getSignedUploadUrl rev
          ResolveProjectDependencies locator -> VSI.resolveProjectDependencies locator
          ResolveUserDefinedBinary deps -> VSI.resolveUserDefinedBinary deps
          QueueArchiveBuild archive -> Core.queueArchiveBuild archive
          UploadAnalysis rev metadata units -> Core.uploadAnalysis rev metadata units
          UploadContainerScan revision metadata scan -> Core.uploadContainerScan revision metadata scan
          UploadContributors locator contributors -> Core.uploadContributors locator contributors
          UploadArchive url path -> Core.uploadArchive url path
          CreateVsiScan rev -> VSI.createVsiScan rev
          AddFilesToVsiScan scanId files -> VSI.addFilesToVsiScan scanId files
          CompleteVsiScan scanId -> VSI.completeVsiScan scanId
          GetVsiScanAnalysisStatus scanId -> VSI.getVsiScanAnalysisStatus scanId
          GetVsiInferences scanId -> VSI.getVsiInferences scanId
      )
