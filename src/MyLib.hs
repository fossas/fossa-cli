
{-# language QuasiQuotes #-}

module MyLib
  ( app
  ) where

import Prologue

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TMChan
import           Control.Monad.STM
import           Path.IO
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output

import Config
import Discovery
import Effect.ReadFS
import Strategy

app :: IO ()
app = do
  setCurrentDir [absdir|/Users/connor/.go/src/github.com/fossas/fossa-cli/|]

  work <- newTMChanIO
  capabilities <- getNumCapabilities
  workThreads <- replicateM capabilities (async (worker work))

  _ <- discovery & readFSToIO
                 & errorToIOFinal @ConfigErr
                 & runOutputSem (embed . atomically . writeTMChan work)
                 & embedToFinal @IO
                 & runFinal

  -- close work queue and wait for workers to finish
  atomically $ closeTMChan work
  for_ workThreads wait -- TODO: exceptions
  pure ()

worker :: TMChan ConfiguredStrategy -> IO ()
worker work = do
  maybeItem <- atomically $ readTMChan work
  for_ maybeItem $ \(ConfiguredStrategy strat opt) -> do
    print =<< strategyAnalyze strat opt
    worker work
