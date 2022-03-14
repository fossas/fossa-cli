{-# LANGUAGE GADTs #-}
module Control.Carrier.FossaAPIIO (runFossaAPIIO) where

import Control.Effect.Diagnostics (Diagnostics)
import Control.Carrier.Simple
import Effect.FossaAPI
import Control.Effect.Lift (Lift)
import App.Fossa.FossaAPIV1 qualified as API

-- | A carrier to run Fossa API functions in the IO monad
type FossaAPIIOC = SimpleC FossaAPIF

-- | Runs FossaAPI effects as IO operations
-- TODO: Put ApiOpts in a reader
-- TODO: Exception handling
runFossaAPIIO :: 
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) => FossaAPIIOC m a -> m a
runFossaAPIIO = interpret $ \case
  GetProject apiOpts pid ->
    API.getProject apiOpts pid

