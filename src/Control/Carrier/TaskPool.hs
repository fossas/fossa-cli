{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.TaskPool (
  withTaskPool,
  TaskPoolC,
  Progress (..),
  module X,
) where

import Control.Carrier.Reader
import Control.Carrier.Threaded
import Control.Concurrent.STM
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Effect.TaskPool as X
import Control.Monad (join, replicateM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (traverse_)
import Data.Functor (void)

withTaskPool ::
  Has (Lift IO) sig m =>
  Int -> -- number of workers
  (Progress -> m ()) -> -- get progress updates
  TaskPoolC m a ->
  m ()
withTaskPool numWorkers reportProgress act = do
  state <-
    sendIO $
      State
        <$> newTVarIO []
        <*> newTVarIO 0
        <*> newTVarIO 0
        <*> newEmptyTMVarIO

  let enqueue action = sendIO $ atomically $ modifyTVar (stQueued state) (action :)
  sendIO (enqueue (void (runTaskPool (stQueued state) act)))

  let mkThreads = do
        workerHandles <- replicateM numWorkers (fork (worker state))
        progressHandle <- fork (updateProgress reportProgress state)
        pure (progressHandle : workerHandles)

  let cleanup = traverse_ kill

  let waitForCompletion _ = join $
        sendIO $
          atomically $ do
            maybeErr <- tryReadTMVar $ stError state
            case maybeErr of
              Just err -> pure $ throwIO err
              Nothing -> do
                queued <- readTVar (stQueued state)
                check (null queued)
                running <- readTVar (stRunning state)
                check (running == 0)
                pure $ pure ()

  bracket mkThreads cleanup waitForCompletion

updateProgress :: Has (Lift IO) sig m => (Progress -> m ()) -> State any -> m ()
updateProgress f st@State{..} = do
  loop (Progress 0 0 0)
  where
    loop prev = join $
      sendIO $
        atomically $
          stopWhenDone st $ do
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

worker :: (Has (Lift IO) sig m) => State m -> m ()
worker st@State{..} = loop
  where
    loop = join $
      sendIO $
        atomically $
          stopWhenDone st $ do
            queued <- readTVar stQueued
            case queued of
              [] -> retry
              (x : xs) -> do
                writeTVar stQueued xs
                addRunning
                pure $
                  mask $ \restore -> do
                    res <- try $ restore x
                    case res of
                      Left err -> do
                        _ <- sendIO $ atomically $ tryPutTMVar stError err
                        pure ()
                      Right () -> complete *> loop

    addRunning :: STM ()
    addRunning = modifyTVar stRunning (+ 1)

    complete :: Has (Lift IO) sig m => m ()
    complete = sendIO $ atomically $ modifyTVar stRunning (subtract 1) *> modifyTVar stCompleted (+ 1)

runTaskPool :: TVar [m ()] -> TaskPoolC m a -> m a
runTaskPool var = runReader var . runTaskPoolC

data Progress = Progress
  { pRunning :: Int
  , pQueued :: Int
  , pCompleted :: Int
  }
  deriving (Eq, Ord, Show)

newtype TaskPoolC m a = TaskPoolC {runTaskPoolC :: ReaderC (TVar [m ()]) m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans TaskPoolC where
  lift = TaskPoolC . lift

instance (Has (Lift IO) sig m, Algebra sig m) => Algebra (TaskPool :+: sig) (TaskPoolC m) where
  alg hdl sig ctx = TaskPoolC $ case sig of
    L (ForkTask m) -> do
      var <- ask @(TVar [m ()])
      sendIO $ atomically $ modifyTVar' var (runReader var (runTaskPoolC (void (hdl (m <$ ctx)))) :)
      pure ctx
    R other -> alg (runTaskPoolC . hdl) (R other) ctx

data State m = State
  { stQueued :: TVar [m ()]
  , stRunning :: TVar Int
  , stCompleted :: TVar Int
  , stError :: TMVar SomeException
  }
