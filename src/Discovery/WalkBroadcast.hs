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
-- internally (the analyzers do this with warnThenRecover) never reach this
-- boundary, so a single failing strategy keeps walking like it does today.
--
module Discovery.WalkBroadcast (
  -- * One broadcast consumer
  WalkConsumer (..),

  -- * Running the broadcast
  broadcastStream,
  withWalkBroadcast,
) where

import Control.Carrier.Threaded (Handle, fork, handleWait, kill)
import Control.Concurrent.STM
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Exception (SomeException, throwIO)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Exception.Extra (safeCatch, safeTry)
import Data.Foldable (traverse_)
import Data.Functor (($>), void)
import Discovery.Filters (AllFilters, toolAllowed)
import Discovery.WalkStream (DirListing, walkStream)
import Discovery.WalkVisitor (Visitor, driveVisitor)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path)
import Streaming (Of (..), Stream)
import Streaming.Prelude qualified as S
import Types (DiscoveredProjectType)

-- | One broadcast consumer: the strategy tool it serves ('wcProjectType', used
-- to gate against the filters) and its per-directory 'Visitor', whose output
-- @o@ is fed straight into whatever sink the visitor itself writes to.
data WalkConsumer o m = WalkConsumer
  { wcProjectType :: DiscoveredProjectType
  , wcVisitor :: Visitor o m
  }

-- | Fan out one listing stream to a set of concurrent consumers.
--
-- The walk runs in its own thread; each enabled consumer gets its own
-- duplicate of the master channel, so it drains at its own pace.  A consumer
-- whose tool is not allowed by @filters@ is skipped entirely.  With no
-- enabled consumers this does nothing at all (no threads, no walk drain).
broadcastStream :: forall sig (m :: * -> *) o. Has (Lift IO) sig m
                => AllFilters -> Stream (Of DirListing) m () -> [WalkConsumer o m] -> m ()
broadcastStream filters listings consumers =
  if null enabled
    then pure ()
    else startAll
  where
    enabled :: [WalkConsumer o m]
    enabled = filter (\c -> toolAllowed filters (wcProjectType c)) consumers

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
      -- Any failure during the forks or the wait: kill everything we started,
      -- then re-throw (safeCatch skips async exceptions, so being killed
      -- ourselves does not trigger this).
      safeCatch (runForkWait master errVar hsVar) (\e -> killAllAndRethrow hsVar e)

    -- Register every consumer on its own channel duplicate BEFORE forking the
    -- walker, so no listing can be written before all readers exist (a
    -- channel written to with no duplicate present loses those messages).
    runForkWait :: TChan (Maybe DirListing) -> TMVar SomeException -> TVar [Handle] -> m ()
    runForkWait master errVar hsVar = do
      chs <- mapM (const $ sendIO (atomically (dupTChan master))) enabled
      _walkerH <- register hsVar =<< fork (walkInto master errVar listings)
      _consumerHS <- traverse (\(c, ch) -> register hsVar <$> fork (consume c errVar ch)) (zip enabled chs)
      waitAll errVar hsVar

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
        errEvent = tryReadTMVar errs >>= \case
          Nothing -> retry
          Just e -> pure (Left e)
        doneEvent = do
          rs <- mapM handleWait hs
          pure $ firstErr rs

    -- The first error among completed handles, if any.
    firstErr :: [Either SomeException ()] -> Either SomeException ()
    firstErr rs = foldr go (Right ()) rs
      where
        go (Left e) _ = Left e
        go _ acc = acc

    -- 'S.mapM_' runs the stream in this monad and returns its result, so no
    -- 'S.effects' here (unlike 'runVisitor', which yields a stream).
    walkInto :: TChan (Maybe DirListing) -> TMVar SomeException -> Stream (Of DirListing) m () -> m ()
    walkInto chan errs ls = do
      res <- safeTry @SomeException (S.mapM_ (\dl -> sendIO $ atomically $ writeTChan chan (Just dl)) ls)
      case res of
        Right () -> sendIO $ atomically $ writeTChan chan Nothing
        Left e -> void (recordError errs e)

    consume :: WalkConsumer o m -> TMVar SomeException -> TChan (Maybe DirListing) -> m ()
    consume c errs chan = do
      res <- safeTry @SomeException (driveVisitor (wcVisitor c) next_)
      case res of
        Left e -> void (recordError errs e)
        Right () -> pure ()
      where
        -- One channel read per step; the terminal 'Nothing' marker ends the
        -- drain.  Kept as a signatured plain-monad local: splicing 'sendIO'
        -- into a Stream-returning recursive local instead fails GHC's
        -- implicit-parameter resolution (GHC-39999), so 'driveVisitor' does
        -- the per-listing state threading without any stream spine.
        next_ :: m (Maybe DirListing)
        next_ = sendIO $ atomically $ readTChan chan


    register :: TVar [Handle] -> Handle -> m Handle
    register hsVar h = sendIO $ atomically (modifyTVar' hsVar (h :)) $> h

    killAllAndRethrow :: TVar [Handle] -> SomeException -> m ()
    killAllAndRethrow hsVar e = do
      hs <- sendIO $ readTVarIO hsVar
      traverse_ kill hs
      throwIO e

    recordError :: TMVar SomeException -> SomeException -> m ()
    recordError errs e = void (sendIO $ atomically $ tryPutTMVar errs e)

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
