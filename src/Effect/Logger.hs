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
  ) where

import Prologue

import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
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

makeSem ''Logger

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

ignoreLogger :: InterpreterFor Logger r
ignoreLogger = interpret $ \case
  Log _ _ -> pure ()
  LogSticky _ -> pure ()
{-# INLINE ignoreLogger #-}

-- | A thread-safe interpreter for the Logger effect
loggerToIO :: Member (Embed IO) r => Severity -> InterpreterFor Logger r
loggerToIO minSeverity act = do
  queue <- embed (hSetBuffering stdout NoBuffering *> newTQueueIO @(Logger Void ()))

  let loop :: Text -> IO ()
      loop sticky = do
        msg <- atomically $ readTQueue queue

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

  _ <- embed $ async $ loop ""

  interpret (\case
    Log sev text -> embed $ atomically $ writeTQueue queue (Log sev text)
    LogSticky text -> embed $ atomically $ writeTQueue queue (LogSticky text)) act
{-# INLINE loggerToIO #-}

printIt :: Doc AnsiStyle -> IO ()
printIt = renderIO stdout . layoutPretty defaultLayoutOptions

renderIt :: Doc AnsiStyle -> Text
renderIt = renderStrict . layoutPretty defaultLayoutOptions
