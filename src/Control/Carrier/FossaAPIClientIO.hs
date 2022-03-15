{-# LANGUAGE GADTs #-}
module Control.Carrier.FossaAPIClientIO (FossaAPIClientIOC, runFossaAPIClientIO) where

import Control.Effect.Diagnostics (Diagnostics)
import Control.Carrier.Simple
import Control.Effect.FossaAPIClient
import Control.Effect.Lift (Lift, sendIO)
import Effect.Logger
import App.Fossa.FossaAPIV1 qualified as API

-- | A carrier to run Fossa API functions in the IO monad
type FossaAPIClientIOC = SimpleC FossaAPIClientF

-- | Runs FossaAPI effects as IO operations
-- TODO: Put ApiOpts in a reader
-- TODO: Exception handling
runFossaAPIClientIO :: 
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) => FossaAPIClientIOC m a -> m a
runFossaAPIClientIO = interpret $ \case
  GetProject apiOpts pid -> do
    logDebug "Running via FossaAPIClientIO"
    API.getProject apiOpts pid

