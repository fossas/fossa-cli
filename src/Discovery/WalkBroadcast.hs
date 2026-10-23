-- | Broadcast fan-out for the shared discovery walk in "Discovery.WalkStream":
-- one walker thread drains a listing stream into a master 'TChan', and each
-- consumer thread reads its own duplicate of that channel, so every consumer
-- sees the full listing set while running concurrently.
--
-- A slow consumer (e.g. an exec-heavy visitor) only stalls its own channel
-- drain — the unbounded 'TChan' buffers the rest — never the walker or any
-- other consumer.  Disabled tools are gated per-consumer: a consumer whose
-- strategy tool is excluded by the filters is never started at all (no thread,
-- no channel, no visitor invocation), mirroring the old
-- "Discovery.Filters.withToolFilter" short-circuit where a disabled tool's
-- discovery did no work whatsoever.
--
-- Failure contract, following "Control.Carrier.TaskPool.withTaskPool": the
-- first consumer (or walker) exception aborts the run — it is re-thrown and
-- every still-running thread is killed; when no error occurs all threads are
-- waited for before returning.  Consumers that recover their own failures
-- internally (the analyzers do this with warnThenRecover, as in
-- 'accumulateConsumer') never reach this boundary: a per-directory failure is
-- recorded in that strategy's sink and surfaced later by its finalizer at ERROR
-- severity, restoring the old per-strategy all-or-nothing discovery contract.
-- This is the accepted delta versus the pre-broadcast per-strategy walks, where
-- each tree was walked independently and one strategy's failure could not reach
-- another: here a failure of the shared WALKER itself still aborts every
-- strategy, because the tree is walked only once.  Pinned by the abort-contract,
-- walker-failure, and all-or-nothing/isolation cases in
-- "Discovery.WalkBroadcastSpec".
--
-- Capability note: unlike VSI's fork+STM pipeline (see the getNumCapabilities
-- clamp in "App.Fossa.VSI.Analyze"), this broadcast needs no 2-capability
-- floor and is safe at -threaded's default of 1.  Every thread here either
-- blocks in STM 'retry' on its channel read (a retrying 'atomically' yields
-- the capability rather than spinning) or does ReadFS IO between quick,
-- non-retrying STM writes; there is no busy-polling loop that could starve
-- the walker on a single capability, and a probe with @-RTS -N1@ completed
-- without deadlock.
module Discovery.WalkBroadcast (
  -- * One broadcast consumer
  WalkConsumer (..),
  accumulateConsumer,

  -- * Running the broadcast
  broadcastStream,
  withWalkBroadcast,
) where

import Control.Carrier.Threaded (Handle, fork, handleWait, kill)
import Control.Concurrent.STM
import Control.Effect.Diagnostics (Diagnostics, warnThenRecover)
import Control.Effect.Exception (SomeException, mask, onException, throwIO)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Exception.Extra (safeTry)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Functor (void, ($>))
import Data.IORef (IORef, atomicModifyIORef')
import Data.Text (Text)
import Discovery.Filters (AllFilters, toolAllowed)
import Discovery.Walk (WalkStep (WalkContinue))
import Discovery.WalkStream (DirListing, walkStream)
import Discovery.WalkVisitor (Visitor, driveVisitor)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path)
import Streaming (Of (..), Stream)
import Streaming.Prelude qualified as S
import Types (DiscoveredProjectType)

-- | One broadcast consumer: the strategy tool it serves ('wcProjectType',
-- gated against the filters) and its per-directory 'Visitor'.
data WalkConsumer o m = WalkConsumer
  { wcProjectType :: DiscoveredProjectType
  , wcVisitor :: Visitor o m
  }

-- | Build a broadcast consumer that accumulates the raw projects its visitor
-- collects into @sink@, applying the T4 all-or-nothing failure contract:
--
--   * A failure in any directory records the tool-attributed @msg@ in the sink
--     and keeps the walk going, so the shared walk is not aborted and sibling
--     strategies (each with its own consumer + sink) are unaffected.
--   * Once the sink holds an error, later successful directories are dropped,
--     so a strategy's whole collection is discarded on any failure.
--
-- The sink is read by the strategy's finalizer (in a separate task): a 'Left'
-- there makes the finalizer throw a Diagnostics error, which
-- "App.Fossa.Analyze" and "Discovery.Projects" render at ERROR severity and
-- discard — restoring the pre-broadcast per-strategy discovery contract.  The
-- single-writer-per-sink invariant (one consumer thread per strategy) means the
-- read-modify-write need not be cross-thread atomic; it is, so the finalizer —
-- reading from another task after the broadcast wait — observes the failure.
accumulateConsumer ::
  (Monoid o, Has (Lift IO) sig m, Has Diagnostics sig m) =>
  DiscoveredProjectType ->
  -- | tool-attributed failure message
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
      found <- warnThenRecover @Text msg (rawVisitor dir subdirs files)
      case found of
        -- A failing directory fails the whole strategy: record the error (so the
        -- finalizer discards the collection) and keep walking, leaving the shared
        -- walk and sibling strategies running.
        Nothing ->
          sendIO (atomicModifyIORef' sink (const (Left msg, ()))) $> ((), WalkContinue)
        Just (project, step) ->
          -- Append unless this strategy already failed (all-or-nothing), preserving
          -- the recorded failure message so it is surfaced verbatim by the finalizer.
          sendIO
            (atomicModifyIORef' sink (\s -> ((case s of Left e -> Left e; Right acc -> Right (acc <> project)), ())) :: IO ())
            $> ((), step)

-- | Fan out one listing stream to a set of concurrent consumers.
broadcastStream ::
  forall sig (m :: * -> *) o.
  Has (Lift IO) sig m =>
  AllFilters -> Stream (Of DirListing) m () -> [WalkConsumer o m] -> m ()
broadcastStream filters listings consumers =
  unless (null enabled) startAll
  where
    enabled :: [WalkConsumer o m]
    enabled = filter (toolAllowed filters . wcProjectType) consumers

    -- The master channel carries @Just listing@ per visit and a terminal
    -- @Nothing@ marker once the walk finishes; this is how consumer channels
    -- end, since stm's TChan has no close.  (On a walker failure no marker is
    -- written: the recorded error makes the wait below throw, and the cleanup
    -- kills the consumers blocked on their channels.)
    startAll :: m ()
    startAll = do
      master <- sendIO newTChanIO
      errVar <- sendIO newEmptyTMVarIO
      hsVar <- sendIO $ newTVarIO ([] :: [Handle])
      runForkWait master errVar hsVar
    -- Any failure during the forks or the wait must kill every handle
    -- registered so far before propagating.  A 'safeCatch' handler is not
    -- enough: it skips async exceptions, so a 'throwTo' landing between the
    -- walker fork and the consumer forks would orphan the threads started
    -- already.  Instead — mirroring "Control.Carrier.TaskPool.withTaskPool"
    -- — the fork sequence and the wait run masked, with an 'onException'
    -- kill of the registered set on both stages, so ANY exception (sync or
    -- async) is preceded by the kill.
    runForkWait :: TChan (Maybe DirListing) -> TMVar SomeException -> TVar [Handle] -> m ()
    runForkWait master errVar hsVar =
      mask $ \restore -> do
        forkAll master errVar hsVar `onException` killAll hsVar
        restore (waitAll errVar hsVar) `onException` killAll hsVar

    -- Register every consumer on its own channel duplicate BEFORE forking the
    -- walker, so no listing can be written before all readers exist (a
    -- channel written to with no duplicate present loses those messages).
    forkAll :: TChan (Maybe DirListing) -> TMVar SomeException -> TVar [Handle] -> m ()
    forkAll master errVar hsVar = do
      chs <- traverse (const $ sendIO (atomically (dupTChan master))) enabled
      _walkerH <- register hsVar =<< fork (walkInto master errVar listings)
      _consumerHS <- traverse (uncurry registerConsumer) (zip enabled chs)
      pure ()
      where
        registerConsumer c ch = register hsVar <$> fork (consume c errVar ch)

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

    walkInto :: TChan (Maybe DirListing) -> TMVar SomeException -> Stream (Of DirListing) m () -> m ()
    walkInto chan errs ls = do
      res <- safeTry @SomeException (S.mapM_ (writeAndDrain chan . Just) ls)
      case res of
        Right () -> writeAndDrain chan Nothing
        Left e -> void (recordError errs e)

    -- Write one message to the master channel and immediately read it back,
    -- discarding it, in the same transaction.  Consumers read dups created
    -- before the walker started, so nobody else ever reads the master — and
    -- stm's TChan retains its node chain from each reader's read pointer: an
    -- undrained master would pin every listing ever broadcast for the whole
    -- run (O(all listings) retention).  Reading one value back per write
    -- advances the master's read pointer atomically with the write, releasing
    -- each node without changing what any dup sees.
    writeAndDrain :: TChan (Maybe DirListing) -> Maybe DirListing -> m ()
    writeAndDrain chan msg = sendIO $ atomically $ do
      writeTChan chan msg
      void (readTChan chan)

    consume :: WalkConsumer o m -> TMVar SomeException -> TChan (Maybe DirListing) -> m ()
    consume c errs chan = do
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
        next_ = sendIO $ atomically $ readTChan chan

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
withWalkBroadcast filters basedir = broadcastStream filters (walkStream filters basedir)
