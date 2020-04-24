module Effect.Logger
  ( Logger(..)
  , LogMsg(..)
  , Severity(..)

  , LoggerC(..)
  , IgnoreLoggerC(..)
  , withLogger
  , ignoreLogger

  , log
  , logSticky

  , logTrace
  , logDebug
  , logInfo
  , logWarn
  , logError

  , module X
  ) where

import Prologue

import Control.Algebra as X
import Control.Carrier.Reader
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue, newTMQueueIO, readTMQueue, writeTMQueue)
import Control.Effect.Exception
import Control.Effect.Lift
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc as X
import Data.Text.Prettyprint.Doc.Render.Terminal as X
import System.IO (hIsTerminalDevice, hSetBuffering, BufferMode(NoBuffering), stderr, stderr)

data Logger m k
  = Log LogMsg (m k)
  deriving (Generic1)

data LogMsg
  = LogNormal Severity (Doc AnsiStyle)
  | LogSticky (Doc AnsiStyle)
  deriving (Show, Generic)

instance HFunctor Logger
instance Effect Logger

-- | Log a message with the given severity
log :: Has Logger sig m => Severity -> Doc AnsiStyle -> m ()
log severity logLine = send (Log (LogNormal severity logLine) (pure ()))

-- | Log a "sticky" line -- a log line that sticks to the bottom of the terminal until cleared or overwritten by other sticky line.
--
-- NOTE: The 'Doc' must not contain newlines
logSticky :: Has Logger sig m => Doc AnsiStyle -> m ()
logSticky logLine = send (Log (LogSticky logLine) (pure ()))

data Severity =
    SevTrace
  | SevDebug
  | SevInfo
  | SevWarn
  | SevError
  deriving (Eq, Ord, Show, Generic)

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

withLogger :: Has (Lift IO) sig m => Severity -> LoggerC m a -> m a
withLogger minSeverity (LoggerC act) = do
  isTerminal <- sendIO $ hIsTerminalDevice stderr
  let logger = bool rawLogger termLogger isTerminal
  queue <- sendIO (hSetBuffering stderr NoBuffering *> newTMQueueIO @LogMsg)

  let mkLogger = sendIO $ async $ logger minSeverity queue

      flushLogger tid = sendIO $ do
        atomically $ closeTMQueue queue
        void (wait tid)

  bracket mkLogger
          flushLogger
          (\_ -> runReader queue act)

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


newtype LoggerC m a = LoggerC { runLoggerC :: ReaderC (TMQueue LogMsg) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Logger :+: sig) (LoggerC m) where
  alg (L (Log msg k)) = do
    queue <- LoggerC ask
    liftIO $ atomically $ writeTMQueue queue msg
    k
   
  alg (R other) = LoggerC (alg (R (handleCoercible other)))

newtype IgnoreLoggerC m a = IgnoreLoggerC { runIgnoreLoggerC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

ignoreLogger :: IgnoreLoggerC m a -> m a
ignoreLogger = runIgnoreLoggerC

instance (Algebra sig m, Effect sig) => Algebra (Logger :+: sig) (IgnoreLoggerC m) where
  alg (L (Log _ k)) = k
  alg (R other) = IgnoreLoggerC (alg (handleCoercible other))

printIt :: Doc AnsiStyle -> IO ()
printIt = renderIO stderr . layoutPretty defaultLayoutOptions

renderIt :: Doc AnsiStyle -> Text
renderIt = renderStrict . layoutPretty defaultLayoutOptions
