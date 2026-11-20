{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- The blocking variants are the point here: 'writeTBMQueue' waiting on a full
-- queue is the back-pressure, and 'readTBMQueue' waiting on an empty one is how
-- a consumer idles until the next listing.
{-# HLINT ignore "Use tryWriteTBMQueue" #-}
{-# HLINT ignore "Use tryReadTBMQueue" #-}

-- | Broadcast fan-out for the shared discovery walk in "Discovery.Walk.Stream":
-- one walker thread drains a listing stream into one bounded queue per enabled
-- consumer, and each consumer thread reads its own queue, so every consumer
-- sees the full listing set while running concurrently.
--
-- A slow consumer only stalls the walker once it has fallen 'listingCapacity'
-- listings behind: below that bound it does not delay anyone, and beyond it the
-- walker waits for it rather than letting it fall arbitrarily behind (which
-- would pin the whole listing set in memory).  Disabled tools are gated
-- per-consumer: a consumer whose strategy tool is excluded by the filters is
-- never started at all (no thread, no queue, no visitor invocation), mirroring
-- the old "Discovery.Filters.withToolFilter" short-circuit where a disabled
-- tool's discovery did no work whatsoever.
--
-- Failure contract, following "Control.Carrier.TaskPool.withTaskPool": the
-- first consumer (or walker) exception aborts the run and every still-running
-- thread is killed; recovered per-directory failures are surfaced per-strategy
-- instead — see 'accumulateConsumer'.
--
-- Safe at '-threaded''s default of one capability: every thread blocks in STM
-- 'retry' (which yields) or between quick, non-retrying STM writes.
module Discovery.Walk.Broadcast (
  -- * One broadcast consumer
  WalkConsumer (..),
  accumulateConsumer,

  -- * Running the broadcast
  broadcastStream,
  withWalkBroadcast,

  -- * Tuning
  listingCapacity,
) where

import Control.Carrier.Threaded (Handle, fork, handleWait, kill)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Control.Effect.Diagnostics (Diagnostics, errorBoundary)
import Control.Effect.Exception (SomeException, mask, onException, throwIO)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Exception.Extra (safeTry)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Functor (void, ($>))
import Data.IORef (IORef, atomicModifyIORef')
import Data.Kind (Type)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Result (Result (Failure, Success), renderFailureWithoutWarnings)
import Discovery.Filters (AllFilters, consumerAllowed)
import Discovery.Walk (WalkStep (WalkContinue))
import Discovery.Walk.Stream (DirListing, walkStream)
import Discovery.Walk.Visitor (Visitor, driveVisitor)
import Effect.Logger (Pretty (pretty), renderIt)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path, toFilePath)
import Streaming (Of (..), Stream)
import Streaming.Prelude qualified as S
import Types (DiscoveredProjectType)

-- | Unread listings each consumer's queue holds before the walker blocks; the
-- capacity 'withWalkBroadcast' passes to 'broadcastStream'.
--
-- This bounds how far the walker may outrun the slowest consumer, and so how
-- much of the listing set stays retained.  A consumer stalled beyond it also
-- stalls its siblings until it catches up, which is what costs wall clock:
-- measured on this repository (22,435 directories, 35 consumers) 2048 ran
-- 25.4s, 16384 21.9s and 65536 22.4s, so the curve flattens here.
--
-- Peak RSS tracks the bound only while the bound binds: 2048 held ~1.74 GB on
-- every run, whereas 16384 ranged 2.2-2.6 GB, set by the tree and by scheduling
-- rather than by the constant.  What is retained is the pinned listings' path
-- lists, not the queue cells, so shortening those paths would buy headroom.
listingCapacity :: Int
listingCapacity = 16384

-- | One broadcast consumer: the strategy tool it serves ('wcProjectType',
-- gated against the filters) and its per-directory 'Visitor'.
data WalkConsumer o m = WalkConsumer
  { wcProjectType :: DiscoveredProjectType
  , wcVisitor :: Visitor o m
  }

-- | Build a broadcast consumer that accumulates the raw projects its visitor
-- collects into @sink@, applying the all-or-nothing failure contract:
--
--   * A failure in any directory records a failure message in the sink and
--     keeps the walk going, so the shared walk is not aborted and sibling
--     strategies (each with its own consumer + sink) are unaffected.
--   * Once the sink holds an error, later successful directories are dropped,
--     so a strategy's whole collection is discarded on any failure.
--
-- The sink is read by the strategy's finalizer (in a separate task): a 'Left'
-- there makes the finalizer throw a Diagnostics error, which
-- "App.Fossa.Analyze" and "Discovery.Projects" render at ERROR severity and
-- discard — restoring the pre-broadcast per-strategy discovery contract.  The
-- finalizer reads from another task after the broadcast wait, so the recorded
-- failure is visible to it; the atomicModifyIORef' keeps the read-modify-write
-- itself from tearing, nothing more.
--
-- The recorded message is @msg@ plus the failing directory plus the underlying
-- diagnostic, because this sink is the /only/ channel back to the finalizer:
-- warnings emitted in a forked consumer thread are discarded along with that
-- thread's result, so anything not carried out in the text is lost.
accumulateConsumer ::
  (Monoid o, Has (Lift IO) sig m, Has Diagnostics sig m) =>
  DiscoveredProjectType ->
  -- | tool-attributed failure-message prefix; the failing directory and its
  -- diagnostic are appended to it
  Text ->
  -- | Accumulates the raw projects via '<>' while healthy; 'Left' once any
  -- directory fails.  Read by the strategy finalizer.
  IORef (Either Text o) ->
  Visitor o m ->
  WalkConsumer () m
accumulateConsumer projectType msg sink rawVisitor =
  WalkConsumer projectType go
  where
    go dir subdirs files = do
      -- 'errorBoundary' rather than 'warnThenRecover': the latter turns the
      -- failure into a warning emitted inside this consumer thread, and a forked
      -- thread's warnings are discarded with its result.  Capturing the 'Result'
      -- lets the recorded text carry the failing directory's own diagnostic out
      -- through the sink, which is the only channel back to the finalizer.
      res <- errorBoundary (rawVisitor dir subdirs files)
      case res of
        -- A failing directory fails the whole strategy: record the error (so the
        -- finalizer discards the collection) and keep walking, leaving the shared
        -- walk and sibling strategies running.
        Failure _ errGroup -> do
          let headerDoc = pretty (msg <> " at " <> toText (toFilePath dir))
              recorded = renderIt (renderFailureWithoutWarnings errGroup headerDoc)
          sendIO (atomicModifyIORef' sink (const (Left recorded, ())))
          pure ((), WalkContinue)
        Success _ (project, step) ->
          -- Append unless this strategy already failed (all-or-nothing), preserving
          -- the recorded failure message so it is surfaced verbatim by the finalizer.
          sendIO
            (atomicModifyIORef' sink (\s -> ((case s of Left e -> Left e; Right acc -> Right (acc <> project)), ())) :: IO ())
            $> ((), step)

-- | Fan out one listing stream to a set of concurrent consumers.
--
-- @capacity@ is how many unread listings each consumer's queue holds before the
-- walker blocks; 'listingCapacity' is what 'withWalkBroadcast' uses.  It is a
-- parameter so tests can pick a small one instead of building a tree as large
-- as the production bound.
broadcastStream ::
  forall sig (m :: Type -> Type) o.
  Has (Lift IO) sig m =>
  Int ->
  AllFilters ->
  Stream (Of DirListing) m () ->
  [WalkConsumer o m] ->
  m ()
broadcastStream capacity filters listings consumers =
  unless (null enabled) startAll
  where
    enabled :: [WalkConsumer o m]
    enabled = filter (consumerAllowed filters . wcProjectType) consumers

    startAll :: m ()
    startAll = do
      -- One bounded queue per enabled consumer, each drained by exactly that
      -- consumer's thread.  'writeTBMQueue' blocks while a queue is full, so the
      -- walker cannot get more than @capacity@ listings ahead of the slowest
      -- consumer; 'closeTBMQueue' ends the stream, since 'readTBMQueue' yields
      -- 'Nothing' once a closed queue has drained.
      queues <- traverse (const $ sendIO (newTBMQueueIO capacity)) enabled
      errVar <- sendIO newEmptyTMVarIO
      hsVar <- sendIO $ newTVarIO ([] :: [Handle])
      runForkWait queues errVar hsVar
    -- Any failure during the forks or the wait must kill every handle
    -- registered so far before propagating.  A 'safeCatch' handler is not
    -- enough: it skips async exceptions, so a 'throwTo' landing between the
    -- walker fork and the consumer forks would orphan the threads started
    -- already.  Instead — mirroring "Control.Carrier.TaskPool.withTaskPool"
    -- — the fork sequence and the wait run masked, with an 'onException'
    -- kill of the registered set on both stages, so ANY exception (sync or
    -- async) is preceded by the kill.
    runForkWait :: [TBMQueue DirListing] -> TMVar SomeException -> TVar [Handle] -> m ()
    runForkWait queues errVar hsVar =
      mask $ \restore -> do
        forkAll queues errVar hsVar `onException` killAll hsVar
        restore (waitAll errVar hsVar) `onException` killAll hsVar

    -- The queues already exist, so a consumer forked after the walker started
    -- still sees every listing written before it began; only the handles need
    -- registering.
    forkAll :: [TBMQueue DirListing] -> TMVar SomeException -> TVar [Handle] -> m ()
    forkAll queues errVar hsVar = do
      _walkerH <- register hsVar =<< fork (walkInto queues errVar listings)
      _consumerHS <- traverse (uncurry registerConsumer) (zip enabled queues)
      pure ()
      where
        registerConsumer c queue = register hsVar =<< fork (consume c errVar queue)

    -- Wait for the first consumer/walker error or every thread's completion;
    -- the error wins whenever it is present (an errored thread always records
    -- before its handle fills, so a fully-complete set means no pending
    -- error).
    waitAll :: TMVar SomeException -> TVar [Handle] -> m ()
    waitAll errs hsVar = do
      hs <- sendIO $ readTVarIO hsVar
      res <- sendIO $ atomically (errOrDone errs hs)
      case res of
        Left e -> throwIO e
        Right () -> pure ()

    errOrDone :: TMVar SomeException -> [Handle] -> STM (Either SomeException ())
    errOrDone errs hs = errEvent `orElse` doneEvent
      where
        errEvent =
          tryReadTMVar errs >>= \case
            Nothing -> retry
            Just e -> pure (Left e)
        doneEvent = do
          rs <- traverse handleWait hs
          pure $ firstErr rs

    firstErr :: [Either SomeException ()] -> Either SomeException ()
    firstErr = foldr go (Right ())
      where
        go (Left e) _ = Left e
        go _ acc = acc

    walkInto :: [TBMQueue DirListing] -> TMVar SomeException -> Stream (Of DirListing) m () -> m ()
    walkInto queues errs ls = do
      res <- safeTry @SomeException (S.mapM_ (broadcastListing queues) ls)
      case res of
        -- On a walker failure the queues are left open on purpose: the recorded
        -- error makes the wait below throw and the cleanup kills the consumers
        -- parked on their queues, rather than letting them end as if the walk
        -- had finished.
        Right () -> sendIO $ atomically $ traverse_ closeTBMQueue queues
        Left e -> void (recordError errs e)

    -- One transaction for all queues keeps them in lockstep, and makes the
    -- walker wait for the fullest one — i.e. for the slowest consumer — instead
    -- of letting any consumer fall arbitrarily far behind.
    broadcastListing :: [TBMQueue DirListing] -> DirListing -> m ()
    broadcastListing queues dl = sendIO $ atomically $ traverse_ (`writeTBMQueue` dl) queues

    consume :: WalkConsumer o m -> TMVar SomeException -> TBMQueue DirListing -> m ()
    consume c errs queue = do
      res <- safeTry @SomeException (driveVisitor (wcVisitor c) next_)
      case res of
        Left e -> void (recordError errs e)
        Right () -> pure ()
      where
        -- Kept as a signatured plain-monad local: splicing 'sendIO' into a
        -- Stream-returning recursive local instead fails GHC's
        -- implicit-parameter resolution (GHC-39999), so 'driveVisitor' does
        -- the per-listing state threading without any stream spine.
        next_ :: m (Maybe DirListing)
        next_ = sendIO $ atomically $ readTBMQueue queue

    register :: TVar [Handle] -> Handle -> m Handle
    register hsVar h = sendIO $ atomically (modifyTVar' hsVar (h :)) $> h

    killAll :: TVar [Handle] -> m ()
    killAll hsVar = do
      hs <- sendIO $ readTVarIO hsVar
      traverse_ kill hs

    recordError :: TMVar SomeException -> SomeException -> m ()
    recordError errs = void . sendIO . atomically . tryPutTMVar errs

-- | Walk @basedir@ once and fan the listings out to a set of concurrent
-- consumers, as in "broadcastStream".
withWalkBroadcast ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  AllFilters ->
  Path Abs Dir ->
  [WalkConsumer o m] ->
  m ()
withWalkBroadcast filters basedir = broadcastStream listingCapacity filters (walkStream filters basedir)
