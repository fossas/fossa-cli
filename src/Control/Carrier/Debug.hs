{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Debug (
  DebugC,
  IgnoreDebugC,
  runDebug,
  ignoreDebug,
  recording,
  ignoring,
  Scope (..),
  ScopeEvent (..),
  module X,
) where

import Control.Carrier.AtomicState (AtomicStateC, modify, runAtomicState)
import Control.Carrier.Diagnostics
import Control.Carrier.Output.IO (OutputC, output, runOutput)
import Control.Carrier.Simple (Simple, sendSimple)
import Control.Effect.Debug as X
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Record (Recordable, SomeEffectResult (SomeEffectResult), recordEff)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Aeson (Key, KeyValue ((.=)), ToJSON (toJSON), Value (String), object)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import Data.Bifunctor (first)
import Data.Fixed (Fixed (MkFixed), Nano)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock.System (SystemTime (MkSystemTime), getSystemTime)

newtype DebugC m a = DebugC {runDebugC :: OutputC ScopeEvent (AtomicStateC (Map.Map Text Value) m) a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans DebugC where
  lift = DebugC . lift . lift

newtype Duration = Duration {unDuration :: Nano}
  deriving (Show)

data Scope = Scope
  { scopeTiming :: Duration
  , scopeEvents :: [ScopeEvent]
  , scopeMetadata :: Map.Map Text Value
  }
  deriving (Show)

instance ToJSON Scope where
  toJSON = object . scopePairs

scopePairs :: Scope -> [Pair]
scopePairs Scope{..} =
  [ "duration" .= show (unDuration scopeTiming)
  ]
    ++ whenNonEmpty "events" scopeEvents
    ++ map (first Key.fromText) (Map.toList scopeMetadata)

whenNonEmpty :: ToJSON a => Key -> [a] -> [Pair]
whenNonEmpty _ [] = []
whenNonEmpty key val = [key .= val]

data ScopeEvent
  = EventEffect SomeEffectResult
  | EventScope Text Scope
  | EventError SomeDiagnostic
  | EventLog Text

instance ToJSON ScopeEvent where
  toJSON (EventEffect (SomeEffectResult k v)) =
    object
      [ "effect" .= encodedK
      , "result" .= encodedV
      ]
    where
      (encodedK, encodedV) = recordEff k v
  toJSON (EventError (SomeDiagnostic _ err)) =
    object
      [ "error" .= show (renderDiagnostic err)
      ]
  toJSON (EventScope nm scope) =
    object $ ("scope" .= nm) : scopePairs scope
  toJSON (EventLog txt) = String txt

instance Show ScopeEvent where
  show (EventEffect (SomeEffectResult k v)) = "SomeEffectResult " <> show (recordEff k v)
  show (EventScope txt sc) = "EventScope " <> show txt <> " " <> show sc
  show (EventError (SomeDiagnostic _ err)) = "EventError " <> show (renderDiagnostic err)
  show (EventLog txt) = "EventLog " <> show txt

timeBetween :: SystemTime -> SystemTime -> Duration
timeBetween (MkSystemTime sec ns) (MkSystemTime sec' ns') =
  Duration (realToFrac (sec' - sec) + MkFixed (fromIntegral ns' - fromIntegral ns))

runDebug :: Has (Lift IO) sig m => DebugC m a -> m (Scope, a)
runDebug act = do
  before <- sendIO getSystemTime
  (metadata, (evs, res)) <- runAtomicState Map.empty . runOutput @ScopeEvent $ runDebugC act
  after <- sendIO getSystemTime
  let duration = timeBetween before after
  pure (Scope duration evs metadata, res)

instance (Has (Lift IO) sig m, Has Diagnostics sig m) => Algebra (Debug :+: sig) (DebugC m) where
  alg hdl sig ctx = DebugC $
    case sig of
      L (DebugScope nm act) -> do
        let act' = errorBoundaryIO $ hdl (act <$ ctx)
        (inner, res) <- lift . lift $ runDebug act'
        output (EventScope nm inner)
        rethrow res
      L (DebugMetadata k v) -> do
        modify (Map.insert k (toJSON v))
        pure ctx
      L (DebugEffect k v) -> do
        output (EventEffect (SomeEffectResult k v))
        pure ctx
      L (DebugError err) -> do
        output (EventError (SomeDiagnostic [] err)) -- FIXME: empty path?
        pure ctx
      L (DebugLog txt) -> do
        output (EventLog txt)
        pure ctx
      R other -> alg (runDebugC . hdl) (R (R other)) ctx

-----------------------------------------------

newtype IgnoreDebugC m a = IgnoreDebugC {runIgnoreDebugC :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Algebra sig m => Algebra (Debug :+: sig) (IgnoreDebugC m) where
  alg hdl sig ctx = IgnoreDebugC $
    case sig of
      L (DebugScope _ act) -> runIgnoreDebugC (hdl (act <$ ctx))
      L DebugMetadata{} -> pure ctx
      L DebugError{} -> pure ctx
      L DebugEffect{} -> pure ctx
      L DebugLog{} -> pure ctx
      R other -> alg (runIgnoreDebugC . hdl) other ctx

ignoreDebug :: IgnoreDebugC m a -> m a
ignoreDebug = runIgnoreDebugC

-----------------------------------------------

-- | Wrap an effect invocation, recording its input/output values with 'debugEffect'
recording :: (Recordable r, Has Debug sig m, Has (Simple r) sig m) => r a -> m a
recording r = do
  res <- sendSimple r
  debugEffect r res
  pure res

-- | Ignore an effect invocation (just @send@ it)
ignoring :: Has (Simple r) sig m => r a -> m a
ignoring = sendSimple
{-# INLINE ignoring #-}
