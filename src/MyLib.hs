
{-# language QuasiQuotes #-}

module MyLib
  ( app
  ) where

import Prologue

import           Control.Concurrent
import           Control.Concurrent.STM.TMChan
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Path.IO
import           Polysemy
import           Polysemy.Async
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.Trace

import Config
import Discovery
import Effect.ErrorTrace
import Effect.Exec
import Effect.ReadFS
import Strategy

app :: IO ()
app = do
  setCurrentDir [absdir|/Users/connor/.go/src/github.com/fossas/fossa-cli/|]
  --setCurrentDir [absdir|/Users/connor/Desktop/tmp3|]

  -- TODO: actual work queue abstraction
  work <- newTMChanIO
  capabilities <- getNumCapabilities

  let readWork :: MonadIO m => m (Maybe ConfiguredStrategy)
      readWork = liftIO $ atomically $ readTMChan work

      writeWork :: MonadIO m => ConfiguredStrategy -> m ()
      writeWork = liftIO . atomically . writeTMChan work

  void . runFinal
       . embedToFinal @IO
       . asyncToIOFinal
       . runOutputSem @CLIErr (\err -> embed $ putStrLn $ "Traced error: " <> show err)
       . readFSToIO
       . execToIO
       . traceToIO -- temporary
       . errorTraceToOutput $ do

    workThreads <- replicateM capabilities $ do
      let worker' = runInputSem readWork $ worker
      async worker'

    _ <- discovery & runOutputSem writeWork

    -- close work queue and wait for workers to finish
    embed $ atomically $ closeTMChan work
    for_ workThreads await -- TODO: exceptions
    pure ()

worker :: Members '[Embed IO, ErrorTrace, Exec, ReadFS, Input (Maybe ConfiguredStrategy), Trace] r => Sem r ()
worker = do
  maybeItem <- input @(Maybe ConfiguredStrategy)
  for_ maybeItem $ \(ConfiguredStrategy strat opt) -> do
    result <- runError @CLIErr $ strategyAnalyze strat opt
    case result of
      Left err -> traceErr err
      Right a -> trace $ show a
    worker
