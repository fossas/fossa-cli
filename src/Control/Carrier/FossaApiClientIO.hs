{-# LANGUAGE GADTs #-}

module Control.Carrier.FossaApiClientIO (FossaApiClientIOC, runFossaApiClientIO) where

import App.Fossa.FossaAPIV1 qualified as API
import Control.Algebra (Has)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Effect.Logger (Logger, logDebug)
import Fossa.API.Types (ApiOpts, Organization)

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
  logDebug "Running via FossaApiClientIO"
  apiOpts <- ask
  -- Fall-back to FossaAPIV1 for now until more uses are migrated.
  API.getOrganization apiOpts
