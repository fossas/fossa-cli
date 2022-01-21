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
  withWarnT,
  errCtxT,
  (<||>),
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.List.NonEmpty qualified as NE
import Diag.Result (EmittedWarn (..), ErrCtx (..), ErrGroup (..), ErrWithStack (..), Result (..), SomeErr (..), SomeWarn (..), Stack (..))

-- | A monad transformer that adds error-/warning-reporting capabilities to
-- other monads
newtype ResultT m a = ResultT {runResultT :: m (Result a)}

instance Functor m => Functor (ResultT m) where
  fmap f = ResultT . fmap (fmap f) . runResultT
  {-# INLINE fmap #-}

instance MonadIO m => MonadIO (ResultT m) where
  liftIO = ResultT . fmap pure . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans ResultT where
  lift = ResultT . fmap pure
  {-# INLINE lift #-}

instance Monad m => Applicative (ResultT m) where
  ResultT mf <*> ResultT ma = ResultT $ (<*>) <$> mf <*> ma
  {-# INLINE (<*>) #-}

  pure = ResultT . pure . pure
  {-# INLINE pure #-}

instance Monad m => Monad (ResultT m) where
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

fatalT :: Applicative m => Stack -> SomeErr -> ResultT m a
fatalT stack err = ResultT (pure (fatalR stack err))
{-# INLINE fatalT #-}

warnT :: Applicative m => SomeWarn -> ResultT m ()
warnT = ResultT . pure . warnR
{-# INLINE warnT #-}

recoverT :: Functor m => ResultT m a -> ResultT m (Maybe a)
recoverT = ResultT . fmap recoverR . runResultT
{-# INLINE recoverT #-}

errorBoundaryT :: Functor m => ResultT m a -> ResultT m (Result a)
errorBoundaryT = ResultT . fmap pure . runResultT
{-# INLINE errorBoundaryT #-}

rethrowT :: Applicative m => Result a -> ResultT m a
rethrowT = ResultT . pure
{-# INLINE rethrowT #-}

withWarnT :: Functor m => SomeWarn -> ResultT m a -> ResultT m a
withWarnT w = ResultT . fmap (withWarnR w) . runResultT
{-# INLINE withWarnT #-}

errCtxT :: Functor m => ErrCtx -> ResultT m a -> ResultT m a
errCtxT c = ResultT . fmap (errCtxR c) . runResultT
{-# INLINE errCtxT #-}

(<||>) :: Monad m => ResultT m a -> ResultT m a -> ResultT m a
ResultT ma <||> ResultT ma' = ResultT $ do
  resA <- ma
  case resA of
    Success ws a -> pure (Success ws a)
    Failure ws eg -> do
      resA' <- ma'
      case resA' of
        Success ws' a' -> pure (Success (ws' <> (errGroupToWarning eg : ws)) a')
        Failure ws' eg' -> pure (Failure (ws' <> ws) (eg' <> eg))
{-# INLINE (<||>) #-}

---------- Base Result operations

-- | Fail with the given stacktrace and error
fatalR :: Stack -> SomeErr -> Result a
fatalR stack e = Failure [] (ErrGroup [] [] (ErrWithStack stack e NE.:| []))
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
withWarnR :: SomeWarn -> Result a -> Result a
withWarnR w (Failure ws (ErrGroup sws ectx es)) = Failure ws (ErrGroup (w : sws) ectx es)
withWarnR _ (Success ws a) = Success ws a
{-# INLINE withWarnR #-}

-- | Attach error context to the ErrGroup of a possibly-failing computation.
-- No-op on Success
errCtxR :: ErrCtx -> Result a -> Result a
errCtxR c (Failure ws (ErrGroup sws ectx es)) = Failure ws (ErrGroup sws (c : ectx) es)
errCtxR _ (Success ws a) = Success ws a
{-# INLINE errCtxR #-}

-- | Convert an ErrGroup into an EmittedWarn, for the purpose of emitting a
-- warning when recovering from a Failure
--
-- When the ErrGroup contains no warnings, this produces 'IgnoredErrGroup'.
-- Otherwise, this produces 'WarnOnErrGroup'
errGroupToWarning :: ErrGroup -> EmittedWarn
errGroupToWarning (ErrGroup sws ectx es) =
  case NE.nonEmpty sws of
    Nothing -> IgnoredErrGroup ectx es
    Just sws' -> (WarnOnErrGroup sws' ectx es)
{-# INLINE errGroupToWarning #-}
