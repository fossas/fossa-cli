{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use writeTMVar" #-}
{-# HLINT ignore "Use tryReadTMVar" #-}
module Control.Carrier.Threaded (
  fork,
  kill,
  wait,
  Handle (..),
) where

import Control.Carrier.Lift
import Control.Concurrent qualified as Conc
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Effect.Exception
import Control.Monad.IO.Class
import Data.Functor (void, ($>))
import Prelude

data Handle = Handle
  { handleTid :: Conc.ThreadId
  , handleWait :: STM (Either SomeException ())
  }

fork :: Has (Lift IO) sig m => m a -> m Handle
fork m = liftWith @IO $ \hdl ctx -> do
  var <- newEmptyTMVarIO
  tid <- mask $ \restore -> Conc.forkIO $ try (void (restore (hdl (m <$ ctx)))) >>= atomically . putTMVar var
  pure (Handle tid (readTMVar var) <$ ctx)

kill :: Has (Lift IO) sig m => Handle -> m ()
kill h = liftWith @IO $ \_ ctx -> liftIO (throwTo (handleTid h) Async.AsyncCancelled) $> ctx

wait :: Has (Lift IO) sig m => Handle -> m ()
wait h = liftWith @IO $ \_ ctx -> liftIO (atomically (handleWait h)) $> ctx
