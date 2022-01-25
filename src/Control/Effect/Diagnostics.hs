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
  warnOnErr,
  (<||>),

  -- * Diagnostic result types
  FailureBundle (..),
  Validator,
  Validation (..),
  renderFailureBundle,
  renderSomeDiagnostic,

  -- * Diagnostic helpers
  fatalText,
  fatalOnIOException,
  fatalOnSomeException,
  fromEither,
  fromEitherShow,
  fromMaybe,
  fromMaybeText,
  tagError,
  combineSuccessful,
  validationBoundary,
  runValidation,

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
import Control.Exception (IOException, SomeException (..))
import Control.Exception.Extra (safeCatch)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Bifunctor (first)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic as Diagnostic
import Diag.Result (Result)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  annotate,
  hsep,
  indent,
  line,
  vsep,
 )
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Cyan, Yellow),
  color,
 )
import Validation (Validation (Failure, Success), eitherToValidation)

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
  WarnOnErr :: ToDiagnostic warn => warn -> m a -> Diag m a
  FirstToSucceed :: m a -> m a -> Diag m a

instance ToDiagnostic (Doc AnsiStyle) where
  renderDiagnostic = id

instance ToDiagnostic SomeException where
  renderDiagnostic (SomeException exc) =
    "An exception occurred: " <> pretty (show exc)

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
warnOnErr :: (ToDiagnostic warn, Has Diagnostics sig m) => warn -> m a -> m a
warnOnErr w m = send (WarnOnErr w m)

-- | Analagous to @Alternative@'s @<|>@. Try the provided actions, returning the
-- value of the first to succeed
(<||>) :: Has Diagnostics sig m => m a -> m a -> m a
(<||>) ma mb = send (FirstToSucceed ma mb)

infixl 3 <||>

---------- Helpers

-- | Throw an untyped string error
fatalText :: Has Diagnostics sig m => Text -> m a
fatalText = fatal

-- | Throw a generic error message on IO error, wrapped in a new 'context' using the provided @Text@.
fatalOnIOException :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> m a -> m a
fatalOnIOException ctx go = context ctx $ catch go die'
  where
    die' (e :: IOException) = fatalText ("io exception: " <> toText (show e))

-- | Throw a generic error message on any exception, wrapped in a new 'context' using the provided @Text@.
fatalOnSomeException :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> m a -> m a
fatalOnSomeException ctx go = context ctx $ safeCatch go die'
  where
    die' (e :: SomeException) = fatalText ("caught exception: " <> toText (show e))

-- | Recover from a fatal error. The error will be recorded as a warning instead.
recover :: Has Diagnostics sig m => m a -> m (Maybe a)
recover = fmap (either (const Nothing) Just) . recover'

-- | Recover from a fatal error. The error will be recorded as a warning instead.
recover' :: Has Diagnostics sig m => m a -> m (Either SomeDiagnostic a)
recover' = send . Recover'

-- | Form an "error boundary" around an action, where:
-- - errors and warnings cannot "escape" the scope of the action
-- - warnings from outside of the error boundary do not impact the FailureBundles produced by the action
--
-- This returns a FailureBundle if the action failed; otherwise returns the result of the action
errorBoundary :: Has Diagnostics sig m => m a -> m (Either FailureBundle a)
errorBoundary = send . ErrorBoundary

-- | A convenience type for the 'Validation' returned by 'validationBoundary'.
type Validator = Validation (NonEmpty SomeDiagnostic)

-- | Same as 'errorBoundary', but converted to a 'Validator'.
validationBoundary :: Has Diagnostics sig m => m a -> m (Validator a)
validationBoundary act = eitherToValidation . first (toNEList . failureCause) <$> errorBoundary act
  where
    toNEList = (NE.:| [])

-- | Convert a @Validator a@ to @m a@, throwing 'fatal' if the validator was not successful.
runValidation :: Has Diagnostics sig m => Validator a -> m a
runValidation (Success a) = pure a
runValidation (Failure bundles) = fatal . renderValidationFailure $ NE.toList bundles

renderValidationFailure :: [SomeDiagnostic] -> Doc AnsiStyle
renderValidationFailure msgs =
  vsep $
    ["One or more errors occurred during validation"]
      <> map (indent 2 . renderSingle) msgs
  where
    -- Because we report the errors together, we don't care about the individual contexts.
    renderSingle (SomeDiagnostic _ cause) = hsep ["-", renderDiagnostic cause]

-- | Rethrow a FailureBundle from an 'errorBoundary'
rethrow :: Has Diagnostics sig m => FailureBundle -> m a
rethrow = send . Rethrow

-- | Push context onto the stack for "stack traces"/"tracebacks" in diagnostics.
--
-- This is spiritually similar to @errors.Wrap@ from golang.
--
-- In the default Diagnostics carrier from Control.Carrier.Diagnostics, context
-- messages are additive and scoped:
--
--     context "foo" $ do
--       -- context is [foo] here
--       context "bar" $ do
--         -- context is [foo,bar] here
--         pure ()
--       context "baz" $ do
--         -- context is [foo,baz] here
--         pure ()
--       -- context is [foo] here
context :: Has Diagnostics sig m => Text -> m a -> m a
context ctx go = send (Context ctx go)

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

-- | Throw a generic error message on any exception, wrapped in a new 'context' using the provided @Text@.
fatalOnSomeException :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> m a -> m a
fatalOnSomeException ctx go = context ctx $ safeCatch go die'
  where
    die' (e :: SomeException) = fatalText ("caught exception: " <> toText (show e))

-- | Run a list of actions, combining the results of successful actions.
--
-- A warning @warn@ is attached to and emitted for each of the failing actions.
--
-- When all actions fail, 'fatal' is invoked with the provided @err@.
combineSuccessful :: (ToDiagnostic err, ToDiagnostic warn, Semigroup a, Has Diagnostics sig m) => err -> warn -> [m a] -> m a
combineSuccessful err war actions = do
  results <- traverse (recover . warnOnErr war) actions
  let successful = NE.nonEmpty $ catMaybes results
  case successful of
    Nothing -> fatal err
    Just xs -> pure (sconcat xs)
