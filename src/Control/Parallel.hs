module Control.Parallel
  ( runActions
  , Progress(..)
  ) where

import Prologue

import Control.Concurrent.STM
import Polysemy
import Polysemy.Async
import Polysemy.Resource

data State action = State
  { stQueued    :: TVar [action]
  , stRunning   :: TVar Int
  , stCompleted :: TVar Int
  }

data Progress = Progress
  { pRunning   :: Int
  , pQueued    :: Int
  , pCompleted :: Int
  } deriving (Eq, Ord, Show, Generic)

-- | Run arbitrary actions in parallel, given:
--
-- - @numThreads@ - The number of worker threads to run
-- - @initial@ - The initial list of actions
-- - @runAction@ - A function that runs an action, which can itself enqueue more actions
--   via the @enqueue@ function provided as an argument
-- - A function that can be used to report 'Progress'
--
-- All tasks will complete before this returns.
--
-- __NOTE: Be careful to handle effects that modify control flow -- e.g. 'Error' -- inside of @runAction@__
--
-- Failing to do so can cause worker threads to crash, leading to a runtime exception when all workers crash.
runActions :: forall r action
            . Members '[Embed IO, Async, Resource] r
           => Int -- number of threads
           -> [action] -- initial actions
           -> ((action -> Sem r ()) -> action -> Sem r ()) -- given an action to enqueue more actions, run an action
           -> (Progress -> Sem r ()) -- get progress updates
           -> Sem r ()
runActions numThreads initial runAction reportProgress = do
  state <- embed $
    State <$> newTVarIO initial
          <*> newTVarIO 0
          <*> newTVarIO 0

  _ <- async $ updateProgress reportProgress state

  let enqueue action = embed $ atomically $ modifyTVar (stQueued state) (action:)

  if numThreads > 1
    then replicateM_ numThreads (async (worker (runAction enqueue) state))
    else worker (runAction enqueue) state

  -- wait for queued and running tasks to end

  _ <- embed $ atomically $ do
    queued  <- readTVar (stQueued state)
    check (length queued == 0)
    running <- readTVar (stRunning state)
    check (running == 0)

  pure ()

updateProgress :: Member (Embed IO) r => (Progress -> Sem r ()) -> State any -> Sem r ()
updateProgress f st@State{..} = loop (Progress 0 0 0)
  where
  loop prev = join $ embed $ atomically $ stopWhenDone st $ do
    running <- readTVar stRunning
    queued <- length <$> readTVar stQueued
    completed <- readTVar stCompleted

    let new = Progress running queued completed

    check (prev /= new)

    pure $ f new *> loop new

stopWhenDone :: State any -> STM (Sem r ()) -> STM (Sem r ())
stopWhenDone State{..} act = do
  queued <- readTVar stQueued
  case queued of
    [] -> do
      running <- readTVar stRunning
      if running == 0
        then pure (pure ())
        else act
    _ -> act

worker :: forall r action. (Member (Embed IO) r, Member Resource r) => (action -> Sem r ()) -> State action -> Sem r ()
worker runAction st@State{..} = loop
  where

  loop = join $ embed $ atomically $ stopWhenDone st $ do
    queued <- readTVar stQueued
    case queued of
      [] -> retry
      (x:xs) -> do
        writeTVar stQueued xs
        addRunning
        pure $ do
          (runAction x) `finally` complete
          loop

  addRunning :: STM ()
  addRunning = modifyTVar stRunning (+1)

  complete :: Sem r ()
  complete = embed $ atomically $ modifyTVar stRunning (subtract 1) *> modifyTVar stCompleted (+1)
