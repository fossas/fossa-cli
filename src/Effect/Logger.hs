{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Logger (
  Logger (..),
  Severity (..),
  LoggerC,
  IgnoreLoggerC,
  withLogger,
  withDefaultLogger,
  runLogger,
  ignoreLogger,
  log,
  logDebug,
  logInfo,
  logWarn,
  logError,
  logStdout,
  module X,
) where

import Control.Algebra as X
import Control.Carrier.Simple
import Control.Effect.ConsoleRegion
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Monad (when)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc as X
import Data.Text.Prettyprint.Doc.Render.Terminal as X
import System.Console.ANSI (hSupportsANSI)
import System.Console.Concurrent (outputConcurrent, errorConcurrent)
import System.IO (stderr)
import Prelude hiding (log)

data LogCtx m = LogCtx
  { logCtxSeverity :: Severity
  , logCtxFormatter :: LogFormatter
  , logCtxWrite :: Text -> m ()
  }

type LogFormatter = Severity -> Doc AnsiStyle -> Text

data Logger (m :: Type -> Type) k where
  Log :: Severity -> Doc AnsiStyle -> Logger m ()
  -- | A dummy effect constructor that ensures stdout logging happens within the
  -- context of a Logger carrier that can ensure output gets flushed
  LogStdout :: Logger m ()

-- | Log a message with the given severity
log :: Has Logger sig m => Severity -> Doc AnsiStyle -> m ()
log severity logLine = send (Log severity logLine)

-- | Log a line to stdout. Usually, you want to use 'log', 'logInfo', ..., instead
logStdout :: (Has Logger sig m, Has (Lift IO) sig m) => Text -> m ()
logStdout txt = do
  send LogStdout
  sendIO $ outputConcurrent txt

data Severity
  = SevDebug
  | SevInfo
  | SevWarn
  | SevError
  deriving (Eq, Ord, Show)

logDebug :: Has Logger sig m => Doc AnsiStyle -> m ()
logDebug = log SevDebug

logInfo :: Has Logger sig m => Doc AnsiStyle -> m ()
logInfo = log SevInfo

logWarn :: Has Logger sig m => Doc AnsiStyle -> m ()
logWarn = log SevWarn

logError :: Has Logger sig m => Doc AnsiStyle -> m ()
logError = log SevError

withLogger :: Has (Lift IO) sig m => LogCtx m -> LoggerC m a -> m a
withLogger ctx act = displayConsoleRegions (runLogger ctx act)

withDefaultLogger :: Has (Lift IO) sig m => Severity -> LoggerC m a -> m a
withDefaultLogger sev act = do
  formatter <- determineDefaultLogFormatter
  let ctx =
        LogCtx
          { logCtxSeverity = sev
          , logCtxFormatter = formatter
          , logCtxWrite = sendIO . errorConcurrent
          }
  withConcurrentOutput $ withLogger ctx act

-- | Determine the default LogAction to use by checking whether the terminal
-- supports ANSI rendering
determineDefaultLogFormatter :: Has (Lift IO) sig m => m LogFormatter
determineDefaultLogFormatter = do
  ansiSupported <- sendIO $ hSupportsANSI stderr
  if ansiSupported
    then pure termLoggerFormatter
    else pure rawLoggerFormatter

rawLoggerFormatter :: LogFormatter
rawLoggerFormatter sev msg = renderIt . unAnnotate $ formatCommon sev msg <> line

termLoggerFormatter :: LogFormatter
termLoggerFormatter sev msg = renderIt $ formatCommon sev msg <> line

formatCommon :: Severity -> Doc AnsiStyle -> Doc AnsiStyle
formatCommon sev msg = hang 2 (pretty '[' <> showSev sev <> pretty @String "] " <> msg)
  where
    showSev SevError = annotate (color Red) (pretty @String "ERROR")
    showSev SevWarn = annotate (color Yellow) (pretty @String " WARN")
    showSev SevInfo = annotate (color Cyan) (pretty @String " INFO")
    showSev SevDebug = annotate (color White) (pretty @String "DEBUG")

type LoggerC = SimpleC Logger

runLogger :: Applicative m => LogCtx m -> LoggerC m a -> m a
runLogger LogCtx{logCtxWrite, logCtxFormatter, logCtxSeverity} = interpret $ \case
  Log sev msg -> do
    when (logCtxSeverity <= sev) $
      logCtxWrite $ logCtxFormatter sev msg
  LogStdout -> pure ()

type IgnoreLoggerC = SimpleC Logger

ignoreLogger :: Applicative m => IgnoreLoggerC m a -> m a
ignoreLogger = interpret $ \case
  Log{} -> pure ()
  LogStdout -> pure ()

renderIt :: Doc AnsiStyle -> Text
renderIt = renderStrict . layoutPretty defaultLayoutOptions
