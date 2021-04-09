{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Logger
  ( Logger(..)
  , LogMsg(..)
  , LogQueue
  , Severity(..)

  , LoggerC(..)
  , IgnoreLoggerC(..)
  , withLogger
  , withLogQueue
  , runLogger
  , ignoreLogger

  , log
  , logSticky

  , logTrace
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logStdout

  , module X
  ) where

import Control.Algebra as X
import Control.Applicative (Alternative)
import Control.Carrier.Reader
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue, newTMQueueIO, readTMQueue, writeTMQueue)
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Bool (bool)
import Data.Functor (void)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc as X
import Data.Text.Prettyprint.Doc.Render.Terminal as X
import Prelude hiding (log)
import System.IO (hIsTerminalDevice, hSetBuffering, BufferMode(NoBuffering), stdout, stderr)

data Logger (m :: Type -> Type) k where
  Log :: LogMsg -> Logger m ()

data LogMsg
  = LogNormal Severity (Doc AnsiStyle)
  | LogSticky (Doc AnsiStyle)
  | LogStdout (Doc AnsiStyle)
  deriving Show

-- | Log a message with the given severity
log :: Has Logger sig m => Severity -> Doc AnsiStyle -> m ()
log severity logLine = send (Log (LogNormal severity logLine))

-- | Log a "sticky" line -- a log line that sticks to the bottom of the terminal until cleared or overwritten by other sticky line.
--
-- NOTE: The 'Doc' must not contain newlines
logSticky :: Has Logger sig m => Doc AnsiStyle -> m ()
logSticky logLine = send (Log (LogSticky logLine))

-- | Log a line to stdout. Usually, you want to use 'log', 'logInfo', ..., instead
logStdout :: Has Logger sig m => Doc AnsiStyle -> m ()
logStdout logLine = send (Log (LogStdout logLine))

data Severity =
    SevTrace
  | SevDebug
  | SevInfo
  | SevWarn
  | SevError
  deriving (Eq, Ord, Show)

logTrace :: Has Logger sig m => Doc AnsiStyle -> m ()
logTrace = log SevTrace

logDebug :: Has Logger sig m => Doc AnsiStyle -> m ()
logDebug = log SevDebug

logInfo :: Has Logger sig m => Doc AnsiStyle -> m ()
logInfo = log SevInfo

logWarn :: Has Logger sig m => Doc AnsiStyle -> m ()
logWarn = log SevWarn

logError :: Has Logger sig m => Doc AnsiStyle -> m ()
logError = log SevError

newtype LogQueue = LogQueue (TMQueue LogMsg)

withLogger :: Has (Lift IO) sig m => Severity -> LoggerC m a -> m a
withLogger sev f = withLogQueue sev $ \qu -> runLogger qu f

withLogQueue :: Has (Lift IO) sig m => Severity -> (LogQueue -> m a) -> m a
withLogQueue minSeverity f = do
  isTerminal <- sendIO $ hIsTerminalDevice stderr
  let logger = bool rawLogger termLogger isTerminal
  queue <- sendIO (hSetBuffering stdout NoBuffering *> hSetBuffering stderr NoBuffering *> newTMQueueIO @LogMsg)

  let mkLogger = sendIO $ async $ logger minSeverity queue

      flushLogger tid = sendIO $ do
        atomically $ closeTMQueue queue
        void (wait tid)

  bracket mkLogger
          flushLogger
          (\_ -> f (LogQueue queue))

runLogger :: LogQueue -> LoggerC m a -> m a
runLogger (LogQueue queue) = runReader queue . runLoggerC

rawLogger :: Severity -> TMQueue LogMsg -> IO ()
rawLogger minSeverity queue = go
  where
  go = do
    maybeMsg <- atomically $ readTMQueue queue
    case maybeMsg of
      Nothing -> pure ()
      Just (LogNormal sev logLine) -> do
        when (sev >= minSeverity) $ printIt (unAnnotate logLine <> line)
        go
      Just (LogSticky logLine) -> do
        printIt (unAnnotate logLine <> line)
        go
      Just (LogStdout logLine) -> do
        TIO.putStrLn (renderIt logLine)
        go

termLogger :: Severity -> TMQueue LogMsg -> IO ()
termLogger minSeverity queue = go ""
  where
  go sticky = do
    maybeMsg <- atomically $ readTMQueue queue
    case maybeMsg of
      Nothing -> pure ()
      Just msg -> do
        let stickyLen   = T.length sticky
            clearSticky = TIO.hPutStr stderr (T.replicate stickyLen "\b" <> T.replicate stickyLen " " <> T.replicate stickyLen "\b")
        case msg of
          LogNormal sev logLine -> do
            when (sev >= minSeverity) $ do
              clearSticky
              printIt $ logLine <> line
              TIO.hPutStr stderr sticky
            go sticky
          LogSticky logLine -> do
            clearSticky
            let rendered = renderIt logLine
            TIO.hPutStr stderr rendered
            go rendered
          LogStdout logLine -> do
            clearSticky
            TIO.hPutStr stdout $ renderIt $ logLine <> line
            TIO.hPutStr stderr sticky
            go sticky

newtype LoggerC m a = LoggerC { runLoggerC :: ReaderC (TMQueue LogMsg) m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (Logger :+: sig) (LoggerC m) where
  alg hdl sig ctx = LoggerC $ case sig of
    L (Log msg) -> do
      queue <- ask
      sendIO $ atomically $ writeTMQueue queue msg
      pure ctx
    R other -> alg (runLoggerC . hdl) (R other) ctx

newtype IgnoreLoggerC m a = IgnoreLoggerC { runIgnoreLoggerC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

ignoreLogger :: IgnoreLoggerC m a -> m a
ignoreLogger = runIgnoreLoggerC

instance Algebra sig m => Algebra (Logger :+: sig) (IgnoreLoggerC m) where
  alg hdl sig ctx = IgnoreLoggerC $ case sig of
    L (Log _) -> pure ctx
    R other -> alg (runIgnoreLoggerC . hdl) other ctx

printIt :: Doc AnsiStyle -> IO ()
printIt = renderIO stderr . layoutPretty defaultLayoutOptions

renderIt :: Doc AnsiStyle -> Text
renderIt = renderStrict . layoutPretty defaultLayoutOptions
