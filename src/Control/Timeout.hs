-- Provide tools for internal-cancellation functions, built for the funcitons in 'App.Fossa.API.BuildWait'

module Control.Timeout (
  Cancel,
  shouldCancelRightNow,
  checkForCancel,
  timeout',
) where

import Control.Carrier.Threaded (fork, kill)
import Control.Concurrent (
  MVar,
  isEmptyMVar,
  newEmptyMVar,
  putMVar,
  threadDelay,
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic,
  fatal,
 )
import Control.Effect.Exception (finally)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (when)

-- Opaque wrapper around MVar (sort of like an atomic variable)
-- Only created by using `timeout'`
newtype Cancel = Cancel (MVar ()) deriving (Eq)

-- Whether a cancellable should cancel right now, or poll again.
-- Use checkForCancel instead if you want to fail
shouldCancelRightNow :: Cancel -> IO Bool
shouldCancelRightNow (Cancel mvar) = not <$> isEmptyMVar mvar

-- | @checkForCancel err cancel@ will return @fatal err@
-- if @shouldCancelRightnow cancel@ returns true.
checkForCancel ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , ToDiagnostic err
  ) =>
  err ->
  Cancel ->
  m ()
checkForCancel err cancel = do
  should <- sendIO $ shouldCancelRightNow cancel
  when should $ fatal err

-- | @timeout' seconds act@ will allow internal cancellation functions to be
-- used, by passing a cancellation token ('Cancel') to act.
-- The intended usage is:
-- @
-- timeout' 10 $ \cancelToken -> do
--   longRunningComputation cancelFlag
-- @
-- Which will run @longRunningComputation@ until it returns a value or until the 10 seconds have expired.
timeout' :: (Has (Lift IO) sig m) => Int -> (Cancel -> m a) -> m a
timeout' seconds act = do
  mvar <- sendIO newEmptyMVar
  handle <- sendIO $
    fork $ do
      threadDelay $ seconds * 1_000_000
      putMVar mvar ()
  -- We need 'finally' here, because `act` can short-circuit.
  -- If we don't use it, we might join the thread, which
  -- requires the timeout to fully expire.
  act (Cancel mvar) `finally` kill handle
