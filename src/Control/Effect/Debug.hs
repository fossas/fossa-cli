module Control.Effect.Debug (
  Debug (..),
  debugScope,
  debugMetadata,
  debugError,
  debugEffect,
  debugLog,
  module X,
) where

import Control.Algebra as X
import Control.Effect.Diagnostics (ToDiagnostic)
import Control.Effect.Record (Recordable)
import Data.Aeson (ToJSON)
import Data.Text (Text)

data Debug m a where
  DebugScope :: Text -> m a -> Debug m a
  DebugMetadata :: ToJSON a => Text -> a -> Debug m ()
  DebugError :: ToDiagnostic err => err -> Debug m ()
  DebugEffect :: Recordable r => r a -> a -> Debug m ()
  DebugLog :: Text -> Debug m ()

-- | Wrap an action into a new scope. This is analagous to Diagnostics' 'context'
debugScope :: Has Debug sig m => Text -> m a -> m a
debugScope nm act = send (DebugScope nm act)

-- | Add a key/value pair to the top-level scope. When using an overlapping key,
-- only the latest value is used
debugMetadata :: (ToJSON a, Has Debug sig m) => Text -> a -> m ()
debugMetadata key val = send (DebugMetadata key val)

-- | Add an error event to this scope
debugError :: (ToDiagnostic err, Has Debug sig m) => err -> m ()
debugError = send . DebugError

-- | Add an effect event -- an effect constructor and its result value -- to this scope
debugEffect :: (Recordable r, Has Debug sig m) => r a -> a -> m ()
debugEffect k v = send (DebugEffect k v)

-- | Add a log event to this scope
debugLog :: Has Debug sig m => Text -> m ()
debugLog = send . DebugLog
