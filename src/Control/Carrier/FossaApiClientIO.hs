{-# LANGUAGE GADTs #-}

module Control.Carrier.FossaApiClientIO (FossaApiClientIOC, runFossaApiClientIO) where

import App.Fossa.FossaAPIV1 qualified as API
import App.Fossa.Report.Attribution (Attribution)
import App.Types (ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Effect.Logger (Logger, logDebug)
import Fossa.API.Types (ApiOpts)

-- | A carrier to run Fossa API functions in the IO monad
type FossaApiClientIOC m = SimpleC FossaApiClientF (ReaderC ApiOpts m)

-- | Runs FossaAPI effects as IO operations
-- TODO: Put ApiOpts in a reader
-- TODO: Exception handling
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
          GetAttribution pid -> getAttribution pid
      )

getAttribution ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m Attribution
getAttribution pid = do
  apiOpts <- ask
  logDebug "Running via FossaApiClientIO"
  API.getAttribution apiOpts pid
