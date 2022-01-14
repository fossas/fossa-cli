module Data.Errors (
  Result (..),
  fatal,
  warn,
  recover,
  errorBoundary,
  rethrow,
  withWarn,
  errCtx,
  (<||>),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

data Result a = Failure [EmittedWarn] ErrGroup | Success [EmittedWarn] a

data EmittedWarn = StandaloneWarn SomeWarn | WarnOnErrGroup (NonEmpty SomeWarn) [ErrCtx] (NonEmpty ErrWithStack)

data ErrGroup = ErrGroup [SomeWarn] [ErrCtx] (NonEmpty ErrWithStack)

data ErrWithStack = ErrWithStack Stack SomeErr

--------------

fatal :: SomeErr -> Result a
fatal e = Failure [] (ErrGroup [] [] (ErrWithStack _TODO e NE.:| []))

warn :: SomeWarn -> Result ()
warn w = Success [StandaloneWarn w] ()

recover :: Result a -> Result (Maybe a)
recover (Failure ws (ErrGroup sws ectx es)) =
  case NE.nonEmpty sws of
    -- NOTE: errors from errgroup get dropped when no warning is present
    Nothing -> Success ws Nothing
    Just ws' -> Success (WarnOnErrGroup ws' ectx es : ws) Nothing
recover (Success ws a) = Success ws (Just a)

errorBoundary :: Result a -> Result (Result a)
errorBoundary = pure

rethrow :: Result a -> Result a
rethrow = id

withWarn :: SomeWarn -> Result a -> Result a
withWarn w (Failure ws (ErrGroup sws ectx es)) = Failure ws (ErrGroup (w : sws) ectx es)
withWarn _ (Success ws a) = Success ws a

errCtx :: ErrCtx -> Result a -> Result a
errCtx c (Failure ws (ErrGroup sws ectx es)) = Failure ws (ErrGroup sws (c : ectx) es)
errCtx _ (Success ws a) = Success ws a

instance Functor Result where
  fmap _ (Failure ws eg) = Failure ws eg
  fmap f (Success ws a) = Success ws (f a)

instance Applicative Result where
  Success ws f <*> Success ws' a = Success (ws' <> ws) (f a)
  Success ws _ <*> Failure ws' eg' = Failure (ws' <> ws) eg'
  Failure ws eg <*> Success ws' _ = Failure (ws' <> ws) eg
  Failure ws eg <*> Failure ws' eg' = Failure (ws' <> ws) (mergeErrGroup eg eg')

  pure = Success []

mergeErrGroup :: ErrGroup -> ErrGroup -> ErrGroup
mergeErrGroup (ErrGroup sws ectx nee) (ErrGroup sws' ectx' nee') = ErrGroup (sws' <> sws) (ectx' <> ectx) (nee' <> nee)

(<||>) :: Result a -> Result a -> Result a
Success ws a <||> _ = Success ws a
-- NOTE: eg on left gets dropped
Failure ws _ <||> Success ws' a = Success (ws' <> ws) a
Failure ws eg <||> Failure ws' eg' = Failure (ws' <> ws) (mergeErrGroup eg eg')

instance Monad Result where
  Failure ws eg >>= _ = Failure ws eg
  Success ws a >>= k =
    case k a of
      Failure ws' eg' -> Failure (ws' <> ws) eg'
      Success ws' b' -> Success (ws' <> ws) b'

----------

_TODO :: a
_TODO = undefined

data Stack
data SomeWarn
data SomeErr
data ErrCtx
