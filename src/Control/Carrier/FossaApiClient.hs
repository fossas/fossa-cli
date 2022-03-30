{-# LANGUAGE GADTs #-}

module Control.Carrier.FossaApiClient (FossaApiClientC, runFossaApiClient) where

import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.Core qualified as Core
import Control.Carrier.FossaApiClient.Internal.ScotlandYard qualified as ScotlandYard
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
          GetApiOpts -> pure apiOpts
          GetIssues rev -> Core.getIssues rev
          GetLatestBuild rev -> Core.getLatestBuild rev
          GetLatestScan locator rev -> ScotlandYard.getLatestScan locator rev
          GetOrganization -> Core.getOrganization
          GetProject rev -> Core.getProject rev
          GetScan locator scanId -> ScotlandYard.getScan locator scanId
          UploadAnalysis rev metadata units -> Core.uploadAnalysis rev metadata units
          UploadContainerScan revision metadata scan -> Core.uploadContainerScan revision metadata scan
          UploadContributors locator contributors -> Core.uploadContributors locator contributors
      )
