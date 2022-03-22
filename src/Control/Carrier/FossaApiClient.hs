{-# LANGUAGE GADTs #-}

module Control.Carrier.FossaApiClient (FossaApiClientC, runFossaApiClient) where

import App.Fossa.FossaAPIV1 qualified as API
import App.Types (ProjectMetadata, ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Fossa.API.Types (ApiOpts, Contributors, Organization, Project, UploadResponse)
import Srclib.Types (SourceUnit)

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
          GetOrganization -> getOrganization
          GetApiOpts -> pure apiOpts
          GetProject rev -> getProject rev
          UploadAnalysis rev metadata units -> uploadAnalysis rev metadata units
          UploadContributors locator contributors -> uploadContributors locator contributors
      )

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

uploadContributors ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Text ->
  Contributors ->
  m ()
uploadContributors locator contributors = do
  apiOpts <- ask
  API.uploadContributors apiOpts locator contributors
