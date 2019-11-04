{-# language TemplateHaskell #-}

module Effect.Logger
  ( Logger(..)
  , Severity(..)

  , log
  , logSticky

  , logTrace
  , logDebug
  , logInfo
  , logWarn
  , logError

  , ignoreLogger
  , loggerToIO
  , module Data.Text.Prettyprint.Doc
  , module Data.Text.Prettyprint.Doc.Render.Terminal
  ) where

import Prologue

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically, check, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TQueue (newTQueueIO, tryReadTQueue, writeTQueue)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Polysemy
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

data Logger m a where
  Log       :: Severity -> Doc AnsiStyle -> Logger m ()
  LogSticky :: Doc AnsiStyle -> Logger m ()

data Severity =
    Trace
  | Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Ord, Show, Generic)

makeSem_ ''Logger

-- | Log a message with the given severity
log :: Member Logger r => Severity -> Doc AnsiStyle -> Sem r ()

-- | Log a "sticky" line -- a log line that sticks to the bottom of the terminal until cleared or overwritten by other sticky line.
--
-- NOTE: The 'Doc' must not contain newlines
logSticky :: Member Logger r => Doc AnsiStyle -> Sem r ()

logTrace :: Member Logger r => Doc AnsiStyle -> Sem r ()
logTrace = log Trace

logDebug :: Member Logger r => Doc AnsiStyle -> Sem r ()
logDebug = log Debug

logInfo :: Member Logger r => Doc AnsiStyle -> Sem r ()
logInfo = log Info

logWarn :: Member Logger r => Doc AnsiStyle -> Sem r ()
logWarn = log Warn

logError :: Member Logger r => Doc AnsiStyle -> Sem r ()
logError = log Error

ignoreLogger :: Sem (Logger ': r) a -> Sem r a
ignoreLogger = interpret $ \case
  Log _ _ -> pure ()
  LogSticky _ -> pure ()
{-# INLINE ignoreLogger #-}

-- | A thread-safe interpreter for the Logger effect
loggerToIO :: Member (Embed IO) r => Severity -> Sem (Logger ': r) a -> Sem r a
loggerToIO minSeverity act = do
  queue <- embed (hSetBuffering stdout NoBuffering *> newTQueueIO @(Logger Void ()))
  cancelVar <- embed (newTVarIO False)

  let loop :: Text -> IO ()
      loop sticky = do
        maybeMsg <- atomically $ do
          msg <- tryReadTQueue queue
          case msg of
            Just a -> pure (Just a)
            Nothing -> do
              canceled <- readTVar cancelVar
              check canceled
              pure Nothing

        case maybeMsg of
          Nothing -> pure () -- exit
          Just msg -> do
            let stickyLen   = T.length sticky
                clearSticky = TIO.putStr (T.replicate stickyLen "\b" <> T.replicate stickyLen " " <> T.replicate stickyLen "\b")
            case msg of
              Log sev doc -> do
                when (sev >= minSeverity) $ do
                  clearSticky
                  printIt $ doc <> line
                  TIO.putStr sticky
                loop sticky
              LogSticky doc -> do
                clearSticky
                let rendered = renderIt doc
                TIO.putStr rendered
                loop rendered

  tid <- embed $ async $ loop ""

  result <- interpret (\case
    Log sev text -> embed $ atomically $ writeTQueue queue (Log sev text)
    LogSticky text -> embed $ atomically $ writeTQueue queue (LogSticky text)) act

  -- wait for log queue to flush, or async logging task to end
  embed $ atomically $ writeTVar cancelVar True
  embed $ void (wait tid)

  pure result
{-# INLINE loggerToIO #-}

printIt :: Doc AnsiStyle -> IO ()
printIt = renderIO stdout . layoutPretty defaultLayoutOptions

renderIt :: Doc AnsiStyle -> Text
renderIt = renderStrict . layoutPretty defaultLayoutOptions
