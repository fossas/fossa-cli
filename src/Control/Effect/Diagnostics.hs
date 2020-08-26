{-# LANGUAGE GADTs #-}

-- | The Diagnostics effect is a replacement for the Error effect in most cases. It models an unchecked exceptions pattern, and provides for:
--
-- - "stack trace"-like behavior, closely resembling the golang pattern of errors.Wrap (see: 'context')
--
-- - recovery from failures, recording them as "warnings" (see: 'recover' or '<||>')
module Control.Effect.Diagnostics
  ( -- * Diagnostics effect and operations
    Diagnostics (..),
    fatal,
    recover,
    context,

    -- * Diagnostic helpers
    fatalText,
    fromEither,
    fromEitherShow,
    tagError,
    (<||>),

    -- * ToDiagnostic typeclass
    ToDiagnostic (..),
    SomeDiagnostic (..),
    module X,
  )
where

import Control.Algebra as X
import Control.Exception (SomeException (..))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Prelude

data Diagnostics m k where
  Fatal :: ToDiagnostic diag => diag -> Diagnostics m a
  Recover :: m a -> Diagnostics m (Maybe a)
  Context :: Text -> m a -> Diagnostics m a

-- | Analagous to @throwError@ from the error effect
fatal :: (Has Diagnostics sig m, ToDiagnostic diag) => diag -> m a
fatal = send . Fatal

-- | Throw an untyped string error
fatalText :: Has Diagnostics sig m => Text -> m a
fatalText = fatal

-- | Recover from a fatal error. The error will be recorded as a warning instead.
recover :: Has Diagnostics sig m => m a -> m (Maybe a)
recover = send . Recover

-- | Push context onto the stack for "stack traces" in diagnostics.
--
-- This is spiritually similar to @errors.Wrap@ from golang
context :: Has Diagnostics sig m => Text -> m a -> m a
context ctx go = send (Context ctx go)

-- | Lift an Either result into the Diagnostics effect, given a ToDiagnostic instance for the error type
fromEither :: (ToDiagnostic err, Has Diagnostics sig m) => Either err a -> m a
fromEither = either fatal pure

-- | Lift an Either result into the Diagnostics effect, given a Show instance for the error type
fromEitherShow :: (Show err, Has Diagnostics sig m) => Either err a -> m a
fromEitherShow = either (fatal . T.pack . show) pure

-- | Lift an Either result into the Diagnostics effect, given a function from the error type to another type that implements 'ToDiagnostic'
tagError :: (ToDiagnostic e', Has Diagnostics sig m) => (e -> e') -> Either e a -> m a
tagError f (Left e) = fatal (f e)
tagError _ (Right a) = pure a

infixl 3 <||>

-- | Analagous to @Alternative@'s @<|>@. Tries both actions and chooses the result that succeeds, invoking 'recover' semantics for errors.
(<||>) :: Has Diagnostics sig m => m a -> m a -> m a
(<||>) ma mb = do
  maybeA <- recover $ ma
  case maybeA of
    Nothing -> mb
    Just a -> pure a

-- | A class of diagnostic types that can be rendered in a user-friendly way
class ToDiagnostic a where
  renderDiagnostic :: a -> Doc AnsiStyle

instance ToDiagnostic Text where
  renderDiagnostic = pretty

instance ToDiagnostic SomeException where
  renderDiagnostic (SomeException exc) =
    "An exception occurred: " <> pretty (show exc)

-- | An error with a ToDiagnostic instance and an associated stack trace
data SomeDiagnostic where
  SomeDiagnostic :: ToDiagnostic a => [Text] -> a -> SomeDiagnostic
