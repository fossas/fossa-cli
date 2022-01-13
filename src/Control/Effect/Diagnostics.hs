{-# LANGUAGE GADTs #-}

-- | The Diagnostics effect is a replacement for the Error effect in most cases. It models an unchecked exceptions pattern, and provides for:
--
-- - "stack trace"-like behavior, closely resembling the golang pattern of errors.Wrap (see: 'context')
--
-- - recovery from failures, recording them as "warnings" (see: 'recover' or '<||>')
module Control.Effect.Diagnostics (
  -- * Diagnostics effect and operations

  -- FIXME
  Diagnostics,
  DiagErr (..),
  fatal,
  context,
  recover,
  errorBoundary,
  rethrow,

  -- * Diagnostic helpers
  fatalText,
  fatalOnIOException,
  fromEither,
  fromEitherShow,
  fromMaybe,
  fromMaybeText,
  warnMaybe,
  warnMaybeText,
  tagError,
  (<||>),
  combineSuccessful,

  -- * Algebra re-exports
  module X,

  -- * ToDiagnostic typeclass
  ToDiagnostic (..),
  SomeDiagnostic (..),
  module Diagnostic,

  -- * FIXME: temporary DiagWarn
  DiagWarn (..),
  warn,
  withWarn,
) where

import Control.Algebra as X
import Control.Carrier.Stack
import Control.Effect.Lift (Lift)
import Control.Exception (IOException)
import Control.Exception.Extra (safeCatch)
import Data.Diagnostic as Diagnostic
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Effect.Logger (Logger, logWarn)

type Diagnostics = Stack :+: DiagErr :+: DiagWarn

data DiagErr m k where
  -- FIXME: separate constraint that's not ToDiagnostic?
  ErrCtx :: ToDiagnostic ctx => ctx -> m a -> DiagErr m a
  Fatal :: ToDiagnostic diag => diag -> DiagErr m a
  Recover :: m a -> DiagErr m (Maybe a)
  ErrorBoundary :: m a -> DiagErr m (Either FailureBundle a)
  Rethrow :: FailureBundle -> DiagErr m a

-- | Analagous to @throwError@ from the error effect
fatal :: (Has Diagnostics sig m, ToDiagnostic diag) => diag -> m a
fatal = send . Fatal

-- | Throw an untyped string error
fatalText :: Has Diagnostics sig m => Text -> m a
fatalText = fatal

-- | Throw a generic error message on IO error, wrapped in a new 'context' using the provided @Text@.
fatalOnIOException :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> m a -> m a
fatalOnIOException ctx go = context ctx $ safeCatch go die'
  where
    die' (e :: IOException) = fatalText ("io exception: " <> toText (show e))

-- | Recover from a fatal error. The error will be recorded as a warning instead.
recover :: Has Diagnostics sig m => m a -> m (Maybe a)
recover = send . Recover

-- | Form an "error boundary" around an action, where:
-- - errors and warnings cannot "escape" the scope of the action
-- - warnings from outside of the error boundary do not impact the FailureBundles produced by the action
--
-- This returns a FailureBundle if the action failed; otherwise returns the result of the action
errorBoundary :: Has Diagnostics sig m => m a -> m (Either FailureBundle a)
errorBoundary = send . ErrorBoundary

-- | Rethrow a FailureBundle from an 'errorBoundary'
rethrow :: Has Diagnostics sig m => FailureBundle -> m a
rethrow = send . Rethrow

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

warnMaybe :: (ToDiagnostic err, Has Diagnostics sig m) => err -> Maybe a -> m (Maybe a)
warnMaybe msg = recover . maybe (fatal msg) pure

warnMaybeText :: Has Diagnostics sig m => Text -> Maybe a -> m (Maybe a)
warnMaybeText = warnMaybe

-- | Lift an Either result into the Diagnostics effect, given a function from the error type to another type that implements 'ToDiagnostic'
tagError :: (ToDiagnostic e', Has Diagnostics sig m) => (e -> e') -> Either e a -> m a
tagError f (Left e) = fatal (f e)
tagError _ (Right a) = pure a

infixl 3 <||>

-- | Analagous to @Alternative@'s @<|>@. Tries both actions and chooses the result that succeeds, invoking 'recover' semantics for errors.
(<||>) :: Has Diagnostics sig m => m a -> m a -> m a
(<||>) ma mb = recover ma >>= maybe mb pure

-- | Run a list of actions, combining the successful ones. If all actions fail, 'fatalText' is invoked with the provided @Text@ message.
combineSuccessful :: (Semigroup a, Has Diagnostics sig m) => Text -> [m a] -> m a
combineSuccessful msg actions = do
  results <- traverse recover actions
  let successful = NE.nonEmpty $ catMaybes results
  case successful of
    Nothing -> fatalText msg
    Just xs -> pure (sconcat xs)

---------- FIXME: temporary separate DiagWarn. incorporate the Warn operation into DiagErr

data DiagWarn m a where
  Warn :: ToDiagnostic warn => warn -> DiagWarn m ()

-- | Emit a warning
warn :: (ToDiagnostic diag, Has Diagnostics sig m) => diag -> m ()
warn = send . Warn

-- | Contextualize a thrown error with a warning
--
-- FIXME: this doesn't currently use the provided diagnostic, and just logs the failure bundle (to maintain present behavior)
withWarn :: (Has Logger sig m, Has Diagnostics sig m) => diag -> m a -> m a
withWarn _ m = do
  maybeBundle <- errorBoundary m
  case maybeBundle of
    Left bundle -> do
      logWarn (renderFailureBundle bundle)
      rethrow bundle
    Right res -> pure res
