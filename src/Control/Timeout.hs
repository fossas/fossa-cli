-- Provide tools for internal-cancellation functions, built for the funcitons in 'App.Fossa.API.BuildWait'

module Control.Timeout (
  Cancel,
  shouldCancelRightNow,
  checkForCancel,
  Duration (..),
  durationToMicro,
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

data Duration
  = Seconds Int
  | Minutes Int
  | MicroSeconds Int

-- Whether a cancellable should cancel right now, or poll again.
-- Use checkForCancel instead if you want to fail.
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

-- | @timeout' duration act@ will allow internal cancellation functions to be
-- used, by passing a cancellation token ('Cancel') to act.
-- A negative duration is the same as no time, and will set the cancel flag
-- almost immediately.
--
-- The intended usage is:
-- @
-- timeout' duration $ \cancelToken -> do
--   longRunningComputation cancelFlag
-- @
--
-- Which will run @longRunningComputation@ until it returns, and set the cancel
-- flag after the duration expires.  For a cancellation to occur,
-- @longRunningComputation@ must return a value early by checking the cancel
-- flag with 'checkForCancel'.  If @longRunningComputation@ does not check the
-- cancel flag, or doesn't return early when the flag is set, no timeout will
-- occur.
timeout' :: (Has (Lift IO) sig m) => Duration -> (Cancel -> m a) -> m a
timeout' duration act = do
  -- We start with an empty MVar to signal that we're not ready to cancel.
  mvar <- sendIO newEmptyMVar
  handle <- sendIO . fork $ do
    threadDelay $ durationToMicro duration
    -- We fill the MVar here, which is the cancel signal.
    putMVar mvar ()
  -- We need 'finally' here, because `act` can short-circuit.
  -- If we don't use it, we might join the thread, which
  -- requires the timeout to fully expire.
  act (Cancel mvar) `finally` kill handle

-- threadDelay only accepts microseconds, so we simplfy that with the tiny
-- abstraction of 'Duration'.
durationToMicro :: Duration -> Int
durationToMicro = \case
  Seconds n -> n * 1_000_000
  Minutes n -> n * 60_000_000
  MicroSeconds n -> n
