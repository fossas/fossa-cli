-- Provide tools for internal-cancellation functions, built for the funcitons in 'App.Fossa.API.BuildWait'

module Control.Timeout (
  Cancel,
  shouldCancelRightNow,
  checkForCancel,
  Duration (..),
  durationToMicro,
  timeout',
  timeoutIO,
) where

import Control.Carrier.Threaded (fork, kill)
import Control.Concurrent (
  isEmptyMVar,
  newEmptyMVar,
  putMVar,
  threadDelay,
 )
import Control.Concurrent.Async qualified as Async
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic,
  fatal,
 )
import Control.Effect.Exception (finally)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (when)
import Control.Timeout.Internal (
  Cancel (..),
  Duration (..),
  durationToMicro,
 )
import Data.Functor (($>))

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
  cancel <- sendIO newEmptyMVar
  handle <- sendIO . fork $ do
    threadDelay $ durationToMicro duration
    -- We fill the MVar here, which is the cancel signal.
    -- SAFETY: putMVar can block infinitely if we do it twice, since we never
    --   empty the MVar. Since we never expose the underlying MVar to the
    --   consumer, we only have to validate that this putMVar call is safe,
    --   and that all other usage in this module is non-blocking.
    putMVar cancel ()
  -- We need 'finally' here, because `act` can short-circuit.
  -- If we don't use it, we might join the thread, which
  -- requires the timeout to fully expire.
  act (Cancel cancel) `finally` kill handle

-- Legacy timeout function, using external cancellation, but cannot work with
-- other IO-capable monad stacks or effects.
timeoutIO ::
  -- | number of seconds before timeout
  Int ->
  IO a ->
  IO (Maybe a)
timeoutIO seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1_000_000) $> Nothing)
