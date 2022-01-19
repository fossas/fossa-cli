module Diag.Result (
  -- * Result type
  Result (..),
  EmittedWarn (..),
  ErrGroup (..),
  ErrWithStack (..),

  -- * Diagnostics
  SomeErr (..),
  SomeWarn (..),
  ErrCtx (..),
  Stack (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)

-- FIXME: considerations about ordering of warnings/errors
data Result a = Failure [EmittedWarn] ErrGroup | Success [EmittedWarn] a
  deriving (Show)

-- TODO: add UncaughtErrGroup constructor?
data EmittedWarn = StandaloneWarn SomeWarn | WarnOnErrGroup (NonEmpty SomeWarn) [ErrCtx] (NonEmpty ErrWithStack)
  deriving (Show)

data ErrGroup = ErrGroup [SomeWarn] [ErrCtx] (NonEmpty ErrWithStack)
  deriving (Show)

instance Semigroup ErrGroup where
  ErrGroup sws ectx nee <> ErrGroup sws' ectx' nee' = ErrGroup (sws <> sws') (ectx <> ectx') (nee <> nee')

data ErrWithStack = ErrWithStack Stack SomeErr
  deriving (Show)

--------------

instance Functor Result where
  fmap _ (Failure ws eg) = Failure ws eg
  fmap f (Success ws a) = Success ws (f a)

instance Applicative Result where
  Success ws f <*> Success ws' a = Success (ws' <> ws) (f a)
  Success ws _ <*> Failure ws' eg' = Failure (ws' <> ws) eg'
  Failure ws eg <*> Success ws' _ = Failure (ws' <> ws) eg
  Failure ws eg <*> Failure ws' eg' = Failure (ws' <> ws) (eg' <> eg)

  pure = Success []

instance Monad Result where
  Failure ws eg >>= _ = Failure ws eg
  Success ws a >>= k =
    case k a of
      Failure ws' eg' -> Failure (ws' <> ws) eg'
      Success ws' b' -> Success (ws' <> ws) b'

----------

newtype Stack = Stack [Text]
  deriving (Show)

data SomeWarn where
  SomeWarn :: ToDiagnostic diag => diag -> SomeWarn

instance Show SomeWarn where
  show (SomeWarn w) = "\"" <> show (renderDiagnostic w) <> "\""

data SomeErr where
  SomeErr :: ToDiagnostic diag => diag -> SomeErr

instance Show SomeErr where
  show (SomeErr e) = "\"" <> show (renderDiagnostic e) <> "\""

data ErrCtx where
  ErrCtx :: ToDiagnostic diag => diag -> ErrCtx

instance Show ErrCtx where
  show (ErrCtx c) = "\"" <> show (renderDiagnostic c) <> "\""
