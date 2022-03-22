{-# LANGUAGE GADTs #-}

module Control.Carrier.FossaApiClientIO (FossaApiClientIOC, runFossaApiClientIO) where

import App.Fossa.FossaAPIV1 qualified as API
import App.Types (ProjectMetadata, ProjectRevision (projectRevision))
import Control.Algebra (Has)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Effect.Logger (Logger, logDebug, pretty)
import Fossa.API.Types (ApiOpts, Contributors, Organization, Project, UploadResponse)
import Srclib.Types (SourceUnit)

-- | A carrier to run Fossa API functions in the IO monad
type FossaApiClientIOC m = SimpleC FossaApiClientF (ReaderC ApiOpts m)

-- | Runs FossaAPI effects as IO operations
--
-- TODO: Flexible exception handling.  Currently exceptions are raised as
-- Diagnostic errors.
runFossaApiClientIO ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  ApiOpts ->
  FossaApiClientIOC m a ->
  m a
runFossaApiClientIO apiOpts =
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
  , Has Logger sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  m Organization
getOrganization = do
  logDebug "Fetching organization"
  apiOpts <- ask
  -- Fall-back to FossaAPIV1 for now until more uses are migrated.
  API.getOrganization apiOpts

getProject ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m Project
getProject revision = do
  logDebug $ "Using revision: `" <> pretty (projectRevision revision) <> "`"
  apiOpts <- ask
  API.getProject apiOpts revision

uploadAnalysis ::
  ( Has (Lift IO) sig m
  , Has (Reader ApiOpts) sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  NE.NonEmpty SourceUnit ->
  m UploadResponse
uploadAnalysis revision metadata units = do
  logDebug $ "Uploading analysis for revision: `" <> pretty (projectRevision revision) <> "`"
  apiOpts <- ask
  API.uploadAnalysis apiOpts revision metadata units

uploadContributors ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  Text ->
  Contributors ->
  m ()
uploadContributors locator contributors = do
  logDebug $ "Uploading contributors for: `" <> pretty locator <> "`"
  apiOpts <- ask
  API.uploadContributors apiOpts locator contributors
