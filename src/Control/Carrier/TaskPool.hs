module Control.Carrier.TaskPool
  ( withTaskPool
  , TaskPoolC

  , Progress(..)

  , module X
  ) where

import Control.Effect.Exception
import Control.Effect.TaskPool as X
import Control.Carrier.Reader
import Control.Carrier.Threaded
import Control.Concurrent.STM
import Prologue

withTaskPool :: (Has (Lift IO) sig m, Has Threaded sig m, MonadIO m)
  => Int -- number of workers
  -> (Progress -> m ()) -- get progress updates
  -> TaskPoolC m a
  -> m ()
withTaskPool numWorkers reportProgress act = do
  state <- liftIO $
    State <$> newTVarIO []
          <*> newTVarIO 0
          <*> newTVarIO 0

  let enqueue action = liftIO $ atomically $ modifyTVar (stQueued state) (action:)
  liftIO $ (enqueue (void (runTaskPool (stQueued state) act)))

  let mkThreads = do
        workerHandles <- replicateM numWorkers (fork (worker state))
        progressHandle <- fork (updateProgress reportProgress state)
        pure (progressHandle:workerHandles)

  let cleanup handles = traverse_ kill handles

  let waitForCompletion _ = liftIO $ atomically $ do
        queued  <- readTVar (stQueued state)
        check (null queued)
        running <- readTVar (stRunning state)
        check (running == 0)

  bracket mkThreads cleanup waitForCompletion

updateProgress :: MonadIO m => (Progress -> m ()) -> State any -> m ()
updateProgress f st@State{..} = do
  loop (Progress 0 0 0)
  where
  loop prev = join $ liftIO $ atomically $ stopWhenDone st $ do
    running <- readTVar stRunning
    queued <- length <$> readTVar stQueued
    completed <- readTVar stCompleted

    let new = Progress running queued completed

    check (prev /= new)

    pure $ f new *> loop new

stopWhenDone :: Applicative m => State any -> STM (m ()) -> STM (m ())
stopWhenDone State{..} act = do
  queued <- readTVar stQueued
  case queued of
    [] -> do
      running <- readTVar stRunning
      if running == 0
        then pure (pure ())
        else act
    _ -> act

worker :: (MonadIO m, Has (Lift IO) sig m) => State m -> m ()
worker st@State{..} = loop
  where

  loop = join $ liftIO $ atomically $ stopWhenDone st $ do
    queued <- readTVar stQueued
    case queued of
      [] -> retry
      (x:xs) -> do
        writeTVar stQueued xs
        addRunning
        -- FIXME: async exceptions should kill execution
        pure $ x `finally` (complete *> loop)

  addRunning :: STM ()
  addRunning = modifyTVar stRunning (+1)

  complete :: MonadIO m => m ()
  complete = liftIO $ atomically $ modifyTVar stRunning (subtract 1) *> modifyTVar stCompleted (+1)

runTaskPool :: TVar [m ()] -> TaskPoolC m a -> m a
runTaskPool var = runReader var . runTaskPoolC

data Progress = Progress
  { pRunning   :: Int
  , pQueued    :: Int
  , pCompleted :: Int
  } deriving (Eq, Ord, Show, Generic)

newtype TaskPoolC m a = TaskPoolC { runTaskPoolC :: ReaderC (TVar [m ()]) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (TaskPool :+: sig) (TaskPoolC m) where
  alg (L (ForkTask m k)) = do
    var <- TaskPoolC ask
    liftIO $ atomically $ modifyTVar' var (runReader var (runTaskPoolC (void m)):)
    k
  alg (R other) = TaskPoolC (alg (R (handleCoercible other)))

data State m = State
  { stQueued    :: TVar [m ()]
  , stRunning   :: TVar Int
  , stCompleted :: TVar Int
  }
