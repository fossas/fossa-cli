{-# LANGUAGE GADTs #-}

-- | The Diagnostics effect is an augmented Error effect. It models an unchecked
-- exceptions pattern with support for stack traces, error aggregation, and
-- warnings.
--
-- See /docs/contributing/diagnostics.md for motivation and usage details.
module Control.Effect.Diagnostics (
  -- * Diagnostics
  Diagnostics,
  context,

  -- * Diag effect and primitives
  Diag (..),
  fatal,
  recover,
  errCtx,
  errorBoundary,
  rethrow,
  warn,
  withWarn,
  (<||>),

  -- * Diagnostic helpers
  fatalText,
  fatalOnIOException,
  fromEither,
  fromEitherShow,
  fromMaybe,
  fromMaybeText,
  tagError,
  combineSuccessful,

  -- * Algebra re-exports
  module X,

  -- * ToDiagnostic typeclass
  ToDiagnostic (..),
  SomeDiagnostic (..),
  module Diagnostic,
) where

import Control.Algebra as X
import Control.Carrier.Stack
import Control.Effect.Lift (Lift)
import Control.Exception (IOException)
import Control.Exception.Extra (safeCatch)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic as Diagnostic
import Diag.Result (Result)

---------- Diagnostics

-- | 'Stack' (the callstack effect) is separate from Diag, but most code using
-- Diag also requires Stack.
--
-- This helper alias can be used to require both:
--
-- @
--     foo :: Has Diagnostics sig m => m ()
-- @
type Diagnostics = Diag :+: Stack

---------- Diag effect and primitives

data Diag m k where
  Fatal :: ToDiagnostic diag => diag -> Diag m a
  Recover :: m a -> Diag m (Maybe a)
  ErrCtx :: ToDiagnostic ctx => ctx -> m a -> Diag m a
  ErrorBoundary :: m a -> Diag m (Result a)
  Rethrow :: Result a -> Diag m a
  Warn :: ToDiagnostic warn => warn -> Diag m ()
  WithWarn :: ToDiagnostic warn => warn -> m a -> Diag m a
  FirstToSucceed :: m a -> m a -> Diag m a

-- | Throw an error
fatal :: (Has Diagnostics sig m, ToDiagnostic diag) => diag -> m a
fatal = send . Fatal

-- | Recover from an error
recover :: Has Diagnostics sig m => m a -> m (Maybe a)
recover = send . Recover

-- | When the provided action fails, annotate its error with the provided
-- context
errCtx :: (ToDiagnostic ctx, Has Diagnostics sig m) => ctx -> m a -> m a
errCtx ctx m = send (ErrCtx ctx m)

-- | Nearly identical to @runDiagnostics@, run an action, returning its
-- underlying 'Result'. Most often, you'll want to use 'recover' instead.
--
-- Warnings and errors that occurred outside the scope of the @errorBoundary@ do
-- not impact the returned 'Result'.
errorBoundary :: Has Diagnostics sig m => m a -> m (Result a)
errorBoundary = send . ErrorBoundary

-- | Lift a Result value. Most often used in combination with 'errorBoundary'
rethrow :: Has Diagnostics sig m => Result a -> m a
rethrow = send . Rethrow

-- | Emit a warning
warn :: (ToDiagnostic diag, Has Diagnostics sig m) => diag -> m ()
warn = send . Warn

-- | When the provided action fails, emit a warning
withWarn :: (ToDiagnostic warn, Has Diagnostics sig m) => warn -> m a -> m a
withWarn w m = send (WithWarn w m)

-- | Analagous to @Alternative@'s @<|>@. Try the provided actions, returning the
-- value of the first to succeed
(<||>) :: Has Diagnostics sig m => m a -> m a -> m a
(<||>) ma mb = send (FirstToSucceed ma mb)

infixl 3 <||>

---------- Helpers

-- | Throw an untyped string error
fatalText :: Has Diagnostics sig m => Text -> m a
fatalText = fatal

-- | Lift an Either result into the Diagnostics effect, given a ToDiagnostic instance for the error type
fromEither :: (ToDiagnostic err, Has Diagnostics sig m) => Either err a -> m a
fromEither = either fatal pure

-- | Lift an Either result into the Diagnostics effect, given a Show instance for the error type
fromEitherShow :: (Show err, Has Diagnostics sig m) => Either err a -> m a
fromEitherShow = either (fatal . toText . show) pure

-- | Lift a Maybe result into Diagnostics, with the given diagnostic thrown for @Nothing@
fromMaybe :: (ToDiagnostic err, Has Diagnostics sig m) => err -> Maybe a -> m a
fromMaybe msg = maybe (fatal msg) pure

-- | Lift a Maybe result into Diagnostics, with a Text error message for @Nothing@
fromMaybeText :: Has Diagnostics sig m => Text -> Maybe a -> m a
fromMaybeText = fromMaybe

-- | Lift an Either result into the Diagnostics effect, given a function from the error type to another type that implements 'ToDiagnostic'
tagError :: (ToDiagnostic e', Has Diagnostics sig m) => (e -> e') -> Either e a -> m a
tagError f (Left e) = fatal (f e)
tagError _ (Right a) = pure a

-- | Throw a generic error message on IO error, wrapped in a new 'context' using the provided @Text@.
fatalOnIOException :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> m a -> m a
fatalOnIOException ctx go = context ctx $ safeCatch go die'
  where
    die' (e :: IOException) = fatalText ("io exception: " <> toText (show e))

-- FIXME: kill/replace with better abstraction

-- | Run a list of actions, combining the successful ones. If all actions fail, 'fatalText' is invoked with the provided @Text@ message.
combineSuccessful :: (Semigroup a, Has Diagnostics sig m) => Text -> [m a] -> m a
combineSuccessful msg actions = do
  results <- traverse recover actions
  let successful = NE.nonEmpty $ catMaybes results
  case successful of
    Nothing -> fatalText msg
    Just xs -> pure (sconcat xs)
