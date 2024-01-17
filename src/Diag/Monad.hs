-- | This module defines a 'Result' monad transformer -- 'ResultT' -- and
-- associated operations
module Diag.Monad (
  -- * Result transformer and operations
  ResultT (..),
  fatalT,
  warnT,
  recoverT,
  errorBoundaryT,
  rethrowT,
  warnOnErrT,
  errCtxT,
  errHelpT,
  errSupportT,
  errDocT,
  (<||>),
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.List.NonEmpty qualified as NE
import Diag.Result (EmittedWarn (..), ErrCtx (..), ErrDoc (..), ErrGroup (..), ErrHelp (..), ErrSupport, ErrWithStack (..), Result (..), SomeErr (..), SomeWarn (..), Stack (..))

-- | A monad transformer that adds error-/warning-reporting capabilities to
-- other monads
--
-- Like the underlying 'Result' type, the Applicative and Monad instances for
-- 'ResultT' carry different semantics.
--
-- When building up a ResultT computation in the applicative style with '<*>' we
-- accumulate Failures encountered into a single Failure.
--
-- When building up a result in the Monadic style with '>>=', we short-circuit
-- on the first Failure.
newtype ResultT m a = ResultT {runResultT :: m (Result a)}

instance Functor m => Functor (ResultT m) where
  fmap :: (a -> b) -> ResultT m a -> ResultT m b
  fmap f = ResultT . fmap (fmap f) . runResultT
  {-# INLINE fmap #-}

instance MonadIO m => MonadIO (ResultT m) where
  liftIO :: IO a -> ResultT m a
  liftIO = ResultT . fmap pure . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans ResultT where
  lift :: Monad m => m a -> ResultT m a
  lift = ResultT . fmap pure
  {-# INLINE lift #-}

instance Applicative m => Applicative (ResultT m) where
  (<*>) :: ResultT m (a -> b) -> ResultT m a -> ResultT m b
  ResultT mf <*> ResultT ma = ResultT $ (<*>) <$> mf <*> ma
  {-# INLINE (<*>) #-}

  pure :: a -> ResultT m a
  pure = ResultT . pure . pure
  {-# INLINE pure #-}

instance Monad m => Monad (ResultT m) where
  (>>=) :: ResultT m a -> (a -> ResultT m b) -> ResultT m b
  ResultT m >>= k = ResultT $ do
    resA <- m
    case resA of
      Failure ws eg -> pure (Failure ws eg)
      Success ws a -> do
        resB <- runResultT (k a)
        case resB of
          Failure ws' eg' -> pure (Failure (ws' <> ws) eg')
          Success ws' b' -> pure (Success (ws' <> ws) b')
  {-# INLINE (>>=) #-}

-- | Fail with the given stacktrace and error
fatalT :: Applicative m => Stack -> SomeErr -> ResultT m a
fatalT stack err = ResultT (pure (fatalR stack err))
{-# INLINE fatalT #-}

-- | Emit a standalone warning
warnT :: Applicative m => SomeWarn -> ResultT m ()
warnT = ResultT . pure . warnR
{-# INLINE warnT #-}

-- | Recover from a possibly-failing computation, returning @Nothing@ on Failure
-- and @Just a@ on Success
recoverT :: Functor m => ResultT m a -> ResultT m (Maybe a)
recoverT = ResultT . fmap recoverR . runResultT
{-# INLINE recoverT #-}

-- | Isolate and extract the Result of a given computation
errorBoundaryT :: Functor m => ResultT m a -> ResultT m (Result a)
errorBoundaryT = ResultT . fmap pure . runResultT
{-# INLINE errorBoundaryT #-}

-- | Lift a Result value into ResultT
--
-- @
--     rethrowT =<< errorBoundary m === m
-- @
rethrowT :: Applicative m => Result a -> ResultT m a
rethrowT = ResultT . pure
{-# INLINE rethrowT #-}

-- | Attach a warning to the ErrGroup of a possibly-failing computation. No-op
-- on Success
warnOnErrT :: Functor m => SomeWarn -> ResultT m a -> ResultT m a
warnOnErrT w = ResultT . fmap (warnOnErrR w) . runResultT
{-# INLINE warnOnErrT #-}

-- | Attach error context to the ErrGroup of a possibly-failing computation.
-- No-op on Success
errCtxT :: Functor m => ErrCtx -> ResultT m a -> ResultT m a
errCtxT c = ResultT . fmap (errCtxR c) . runResultT
{-# INLINE errCtxT #-}

-- | Attach error help to the ErrGroup of a possibly-failing computation.
-- No-op on Success
errHelpT :: Functor m => ErrHelp -> ResultT m a -> ResultT m a
errHelpT h = ResultT . fmap (errHelpR h) . runResultT
{-# INLINE errHelpT #-}

-- | Attach error support to the ErrGroup of a possibly-failing computation.
-- No-op on Success
errSupportT :: Functor m => ErrSupport -> ResultT m a -> ResultT m a
errSupportT s = ResultT . fmap (errSupportR s) . runResultT
{-# INLINE errSupportT #-}

-- | Attach error doc to the ErrGroup of a possibly-failing computation.
-- No-op on Success
errDocT :: Functor m => ErrDoc -> ResultT m a -> ResultT m a
errDocT d = ResultT . fmap (errDocR d) . runResultT
{-# INLINE errDocT #-}

-- | Try both actions, returning the value of the first to succeed.
--
-- Similar to the Applicative instance, this accumulates errors from encountered
-- Failures
--
-- Dissimilar to the Applicative instance, this won't run the second action when
-- the first action results in Success
--
-- @<||>@ is like @<|>@ from 'Control.Applicative.Alternative', but ResultT
-- cannot have a useful Alternative instance, because it cannot implement
-- @empty@
(<||>) :: Monad m => ResultT m a -> ResultT m a -> ResultT m a
ResultT ma <||> ResultT ma' = ResultT $ do
  resA <- ma
  case resA of
    Success ws a -> pure (Success ws a)
    Failure ws eg -> do
      resA' <- ma'
      case resA' of
        Success ws' a' -> pure (Success (ws' <> (errGroupToWarning eg : ws)) a')
        Failure ws' eg' -> pure (Failure (ws' <> ws) (eg <> eg')) -- match the order of alternatives for error group aggregation
{-# INLINE (<||>) #-}

---------- Base Result operations

-- | Fail with the given stacktrace and error
fatalR :: Stack -> SomeErr -> Result a
fatalR stack e = Failure [] (ErrGroup [] [] [] [] [] (ErrWithStack stack e NE.:| []))
{-# INLINE fatalR #-}

-- | Emit a standalone warning
warnR :: SomeWarn -> Result ()
warnR w = Success [StandaloneWarn w] ()
{-# INLINE warnR #-}

-- | Recover from a possibly-failing computation, returning @Nothing@ on Failure
-- and @Just a@ on Success
recoverR :: Result a -> Result (Maybe a)
recoverR (Failure ws eg) = Success (errGroupToWarning eg : ws) Nothing
recoverR (Success ws a) = Success ws (Just a)
{-# INLINE recoverR #-}

-- | Attach a warning to the ErrGroup of a possibly-failing computation. No-op
-- on Success
warnOnErrR :: SomeWarn -> Result a -> Result a
warnOnErrR w (Failure ws (ErrGroup sws ectx ehlp esup edoc es)) = Failure ws (ErrGroup (w : sws) ectx ehlp esup edoc es)
warnOnErrR _ (Success ws a) = Success ws a
{-# INLINE warnOnErrR #-}

-- | Attach error context to the ErrGroup of a possibly-failing computation.
-- No-op on Success
errCtxR :: ErrCtx -> Result a -> Result a
errCtxR c (Failure ws (ErrGroup sws ectx ehlp esup edoc es)) = Failure ws (ErrGroup sws (c : ectx) ehlp esup edoc es)
errCtxR _ (Success ws a) = Success ws a
{-# INLINE errCtxR #-}

-- | Attach error help to the ErrGroup of a possibly-failing computation.
-- No-op on Success
errHelpR :: ErrHelp -> Result a -> Result a
errHelpR h (Failure ws (ErrGroup sws ectx ehlp esup edoc es)) = Failure ws (ErrGroup sws ectx (h : ehlp) esup edoc es)
errHelpR _ (Success ws a) = Success ws a
{-# INLINE errHelpR #-}

-- | Attach error support to the ErrGroup of a possibly-failing computation.
-- No-op on Success
errSupportR :: ErrSupport -> Result a -> Result a
errSupportR s (Failure ws (ErrGroup sws ectx ehlp esup edoc es)) = Failure ws (ErrGroup sws ectx ehlp (s : esup) edoc es)
errSupportR _ (Success ws a) = Success ws a
{-# INLINE errSupportR #-}

-- | Attach error context to the ErrGroup of a possibly-failing computation.
-- No-op on Success
errDocR :: ErrDoc -> Result a -> Result a
errDocR d (Failure ws (ErrGroup sws ectx ehlp esup edoc es)) = Failure ws (ErrGroup sws ectx ehlp esup (d : edoc) es)
errDocR _ (Success ws a) = Success ws a
{-# INLINE errDocR #-}

-- | Convert an ErrGroup into an EmittedWarn, for the purpose of emitting a
-- warning when recovering from a Failure
--
-- When the ErrGroup contains no warnings, this produces 'IgnoredErrGroup'.
-- Otherwise, this produces 'WarnOnErrGroup'
errGroupToWarning :: ErrGroup -> EmittedWarn
errGroupToWarning (ErrGroup sws ectx ehlp esup edoc es) =
  case NE.nonEmpty sws of
    Nothing -> IgnoredErrGroup ectx ehlp esup edoc es
    Just sws' -> WarnOnErrGroup sws' ectx ehlp esup edoc es
{-# INLINE errGroupToWarning #-}
