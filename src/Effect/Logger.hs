{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Logger
  ( Logger (..),
    Severity (..),
    LoggerC (..),
    IgnoreLoggerC (..),
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
  )
where

import Control.Algebra as X
import Control.Applicative (Alternative)
import Control.Carrier.Reader
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Effect.ConsoleRegion
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc as X
import Data.Text.Prettyprint.Doc.Render.Terminal as X
import System.Console.Concurrent
import System.IO (stderr)
import Prelude hiding (log)
import System.Console.ANSI (hSupportsANSI)
import Control.Monad.Trans (lift)

data LogCtx m = LogCtx
  { logCtxSeverity :: Severity
  , logCtxFormatter :: LogFormatter
  , logCtxWrite :: Text -> m ()
  }

type LogFormatter = Severity -> Doc AnsiStyle -> Text

data Logger (m :: Type -> Type) k where
  Log :: Severity -> Doc AnsiStyle -> Logger m ()

-- | Log a message with the given severity
log :: Has Logger sig m => Severity -> Doc AnsiStyle -> m ()
log severity logLine = send (Log severity logLine)

-- | Log a line to stdout. Usually, you want to use 'log', 'logInfo', ..., instead
logStdout :: Has (Lift IO) sig m => Text -> m ()
logStdout = sendIO . outputConcurrent

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
withLogger ctx act = displayConsoleRegions (runLogger ctx act) <* sendIO flushConcurrentOutput

withDefaultLogger :: Has (Lift IO) sig m => Severity -> LoggerC m a -> m a
withDefaultLogger sev act = do
  formatter <- determineDefaultLogFormatter
  let ctx =
        LogCtx
          { logCtxSeverity = sev
          , logCtxFormatter = formatter
          , logCtxWrite = sendIO . errorConcurrent
          }
  withLogger ctx act

runLogger :: LogCtx m -> LoggerC m a -> m a
runLogger act = runReader act . runLoggerC

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

newtype LoggerC m a = LoggerC {runLoggerC :: ReaderC (LogCtx m) m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (Logger :+: sig) (LoggerC m) where
  alg hdl sig ctx = LoggerC $ case sig of
    L (Log sev msg) -> do
      LogCtx{logCtxWrite,logCtxFormatter,logCtxSeverity} <- ask
      when (logCtxSeverity <= sev) $
        lift $ logCtxWrite $ logCtxFormatter sev msg
      pure ctx
    R other -> alg (runLoggerC . hdl) (R other) ctx

newtype IgnoreLoggerC m a = IgnoreLoggerC {runIgnoreLoggerC :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

ignoreLogger :: IgnoreLoggerC m a -> m a
ignoreLogger = runIgnoreLoggerC

instance Algebra sig m => Algebra (Logger :+: sig) (IgnoreLoggerC m) where
  alg hdl sig ctx = IgnoreLoggerC $ case sig of
    L (Log _ _) -> pure ctx
    R other -> alg (runIgnoreLoggerC . hdl) other ctx

renderIt :: Doc AnsiStyle -> Text
renderIt = renderStrict . layoutPretty defaultLayoutOptions
