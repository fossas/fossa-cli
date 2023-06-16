{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Logger (
  Logger,
  LoggerF (..),
  Severity (..),
  LoggerC,
  IgnoreLoggerC,
  withLogger,
  withDefaultLogger,
  withWriterLogger,
  withConcurrentWriterLogger,
  runLogger,
  ignoreLogger,
  log,
  logDebug,
  logInfo,
  logWarn,
  logError,
  logStdout,
  renderIt,
  module X,
  newlineTrailing,
  newlinePreceding,
) where

import Control.Algebra as X
import Control.Carrier.Simple
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue, writeTMQueue)
import Control.Effect.ConsoleRegion
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Effect.Reader (Reader, ask)
import Control.Effect.Record (RecordableValue)
import Control.Effect.Record.TH (deriveRecordable)
import Control.Effect.Writer (Writer, tell)
import Control.Monad (when)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (Value (String), toJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter as X
import Prettyprinter.Render.Terminal as X
import System.Console.ANSI (hSupportsANSI)
import System.Console.Concurrent (errorConcurrent, outputConcurrent)
import System.IO (stderr)
import Prelude hiding (log)

data LogCtx l m = LogCtx
  { logCtxSeverity :: Severity
  , logCtxFormatter :: LogFormatter l
  , logCtxWrite :: l -> m ()
  }

type LogFormatter l = Severity -> Doc AnsiStyle -> l

data Severity
  = SevDebug
  | SevInfo
  | SevWarn
  | SevError
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Severity where
  toJSON SevDebug = String "debug"
  toJSON SevInfo = String "info"
  toJSON SevWarn = String "warn"
  toJSON SevError = String "error"

instance RecordableValue Severity

data LoggerF a where
  Log :: Severity -> Doc AnsiStyle -> LoggerF ()
  -- | A dummy effect constructor that ensures stdout logging happens within the
  -- context of a Logger carrier that can ensure output gets flushed
  LogStdout :: LoggerF ()

$(deriveRecordable ''LoggerF)

type Logger = Simple LoggerF

-- | Log a message with the given severity
log :: Has Logger sig m => Severity -> Doc AnsiStyle -> m ()
log severity logLine = sendSimple (Log severity logLine)

-- | Log a line to stdout. Usually, you want to use 'log', 'logInfo', ..., instead
logStdout :: (Has Logger sig m, Has (Lift IO) sig m) => Text -> m ()
logStdout txt = do
  sendSimple LogStdout
  sendIO $ outputConcurrent txt

logDebug :: Has Logger sig m => Doc AnsiStyle -> m ()
logDebug = log SevDebug

logInfo :: Has Logger sig m => Doc AnsiStyle -> m ()
logInfo = log SevInfo

logWarn :: Has Logger sig m => Doc AnsiStyle -> m ()
logWarn = log SevWarn

logError :: Has Logger sig m => Doc AnsiStyle -> m ()
logError = log SevError

withLogger :: Has (Lift IO) sig m => LogCtx Text m -> LoggerC m a -> m a
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

-- | Runs a Logger into a Writer. Useful for collecting logged messages, such as
-- in testing.
--
-- Note that you will need to specify the type variable @f@ (the monoidal
-- container of each log message) at the call-site using @TypeApplications@.
--
-- The log is constructed with left-associative `mappend`s, so pick an @f@ where
-- left-associative append is performant (like `Seq`, unlike `[]`).
withWriterLogger ::
  forall f sig m a.
  (Applicative f, Has (Writer (f (Doc AnsiStyle))) sig m) =>
  Severity ->
  LoggerC m a ->
  m a
withWriterLogger sev = runLogger ctx
  where
    ctx :: LogCtx (Doc AnsiStyle) m
    ctx =
      LogCtx
        { logCtxSeverity = sev
        , logCtxFormatter = const id
        , logCtxWrite = tell . pure @f
        }

-- | Like `withWriterLogger`, except it works even when the logged action forks
-- threads.
--
-- In particular, using `withWriterLogger` with `TaskPool` will cause log
-- messages from forked threads to be missed. Use `withConcurrentWriterLogger`
-- wherever you would use `withWriterLogger` to collect log messages even when
-- the underlying action forks.
withConcurrentWriterLogger ::
  forall sig m a.
  ( Has (Reader (TMQueue (Doc AnsiStyle))) sig m
  , Has (Lift IO) sig m
  ) =>
  Severity ->
  LoggerC m a ->
  m a
withConcurrentWriterLogger sev action = do
  result <- runLogger ctx action
  logs <- ask @(TMQueue (Doc AnsiStyle))
  sendIO $ atomically $ closeTMQueue logs
  pure result
  where
    ctx :: LogCtx (Doc AnsiStyle) m
    ctx =
      LogCtx
        { logCtxSeverity = sev
        , logCtxFormatter = const id
        , logCtxWrite = \msg -> do
            logs <- ask
            sendIO $ atomically $ writeTMQueue logs msg
        }

-- | Determine the default LogAction to use by checking whether the terminal
-- supports ANSI rendering
determineDefaultLogFormatter :: Has (Lift IO) sig m => m (LogFormatter Text)
determineDefaultLogFormatter = do
  ansiSupported <- sendIO $ hSupportsANSI stderr
  if ansiSupported
    then pure termLoggerFormatter
    else pure rawLoggerFormatter

rawLoggerFormatter :: LogFormatter Text
rawLoggerFormatter sev msg = renderIt . unAnnotate $ formatCommon sev msg <> line

termLoggerFormatter :: LogFormatter Text
termLoggerFormatter sev msg = renderIt $ formatCommon sev msg <> line

formatCommon :: Severity -> Doc AnsiStyle -> Doc AnsiStyle
formatCommon sev msg = hang 2 (showSev sev <> msg)
  where
    showSev SevError = withBrackets $ annotate (color Red) (pretty @String "ERROR")
    showSev SevWarn = withBrackets $ annotate (color Yellow) (pretty @String "WARN")
    showSev SevInfo = ""
    showSev SevDebug = withBrackets $ annotate (color White) (pretty @String "DEBUG")

    withBrackets :: Doc AnsiStyle -> Doc AnsiStyle
    withBrackets s = pretty '[' <> s <> pretty @String "] "

type LoggerC = SimpleC LoggerF

runLogger :: Applicative m => LogCtx l m -> LoggerC m a -> m a
runLogger LogCtx{logCtxWrite, logCtxFormatter, logCtxSeverity} = interpret $ \case
  Log sev msg -> do
    when (logCtxSeverity <= sev) $
      logCtxWrite $
        logCtxFormatter sev msg
  LogStdout -> pure ()

type IgnoreLoggerC = SimpleC LoggerF

ignoreLogger :: Applicative m => IgnoreLoggerC m a -> m a
ignoreLogger = interpret $ \case
  Log{} -> pure ()
  LogStdout -> pure ()

renderIt :: Doc AnsiStyle -> Text
renderIt = renderStrict . layoutPretty defaultLayoutOptions

newlineTrailing :: Doc ann -> Doc ann
newlineTrailing doc = vsep [doc, ""]

newlinePreceding :: Doc ann -> Doc ann
newlinePreceding doc = vsep ["", doc]
