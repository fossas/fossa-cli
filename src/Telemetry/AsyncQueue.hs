{-# LANGUAGE RecordWildCards #-}

module Telemetry.AsyncQueue (
  AsyncQueue,
  newAsyncQueue,
  writeAsyncQueue,
  closeAsyncQueue,
) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TBMQueue (
  TBMQueue,
  closeTBMQueue,
  estimateFreeSlotsTBMQueue,
  newTBMQueueIO,
  tryReadTBMQueue,
  tryWriteTBMQueue,
 )
import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (catMaybes, fromMaybe)
import Control.Effect.Exception (SomeException)

-- | Bounded and closable AsyncQueue of fixed @asyncQueueChunkSize size. 
-- When queue is closed, all items are retrieved and processed with @asyncQueueProcessor.
-- When queue is full and write is performed, queue is flushed and processed with @asyncQueueProcessor. 
data AsyncQueue a = AsyncQueue
  { asyncQueue :: TBMQueue a
  , asyncQueueChunkSize :: Int 
  , asyncQueueProcessor :: NonEmpty a ->  IO () -- Todo: capture exceptions
  }

newAsyncQueue :: Int -> (NonEmpty a -> IO ()) -> IO (AsyncQueue a)
newAsyncQueue qSize f = do
  q <- newTBMQueueIO qSize
  pure (AsyncQueue q qSize f)

-- Todo: capture exceptions
writeAsyncQueue :: AsyncQueue a -> a -> IO ()
writeAsyncQueue q entry = do
  res <- atomically $ writeAndFlush q entry
  case res of
    Nothing -> pure ()
    Just flushedItems -> do
      result <- Async.waitCatch =<< Async.async (asyncQueueProcessor q flushedItems)
      case result of
        Left _ -> pure () -- Todo: capture exceptions
        Right _ -> pure ()

-- | Closes the queue, and drains all entries, and process them with
-- @asyncQueueProcessor.
closeAsyncQueue :: AsyncQueue a -> IO (Maybe SomeException)
closeAsyncQueue AsyncQueue{..} = do
  allItems <- atomically $ do
    closeTBMQueue asyncQueue
    get asyncQueue asyncQueueChunkSize
  case allItems of
    Nothing -> pure Nothing
    Just ne -> leftToMaybe <$> (Async.waitCatch =<< Async.async (asyncQueueProcessor ne))
  where
    leftToMaybe :: Either a b -> Maybe a
    leftToMaybe = either Just (const Nothing)

-- | Writes to @asyncQueue, and if the queue is full, retrieves all entries
-- and processes them with @asyncQueueProcessor
writeAndFlush :: AsyncQueue a -> a -> STM (Maybe (NonEmpty a))
writeAndFlush AsyncQueue{..} item = do
  freeSlots <- estimateFreeSlotsTBMQueue asyncQueue
  if freeSlots == 1
    then tryWriteTBMQueue asyncQueue item >> get asyncQueue asyncQueueChunkSize
    else tryWriteTBMQueue asyncQueue item >> pure Nothing

-- | Retrieve number of items from the @asyncQueue.
get :: TBMQueue a -> Int -> STM (Maybe (NonEmpty a))
get q numItems = do
  items <- replicateM numItems (tryReadTBMQueue q)
  pure $ nonEmpty . catMaybes $ (fromMaybe Nothing) <$> items