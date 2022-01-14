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
  deriving Show

data EmittedWarn = StandaloneWarn SomeWarn | WarnOnErrGroup (NonEmpty SomeWarn) [ErrCtx] (NonEmpty ErrWithStack)
  deriving Show

data ErrGroup = ErrGroup [SomeWarn] [ErrCtx] (NonEmpty ErrWithStack)
  deriving Show

data ErrWithStack = ErrWithStack Stack SomeErr
  deriving Show

--------------

newtype ResultT m a = ResultT { runResultT :: m (Result a) }
  deriving Functor

instance Monad m => Applicative (ResultT m) where
  ResultT mf <*> ResultT ma = ResultT $ do
    resF <- mf
    resA <- ma
    pure (resF <*> resA)

  pure = ResultT . pure . pure

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

fatalT :: Applicative m => Stack -> SomeErr -> ResultT m a
fatalT stack err = ResultT (pure (fatal stack err))

warnT :: Applicative m => SomeWarn -> ResultT m ()
warnT = ResultT . pure . warn

recoverT :: Functor m => ResultT m a -> ResultT m (Maybe a)
recoverT = ResultT . fmap recover . runResultT

errorBoundaryT :: Functor m => ResultT m a -> ResultT m (Result a)
errorBoundaryT = ResultT . fmap pure . runResultT

rethrowT :: Applicative m => Result a -> ResultT m a
rethrowT = ResultT . pure

withWarnT :: Functor m => SomeWarn -> ResultT m a -> ResultT m a
withWarnT w = ResultT . fmap (withWarn w) . runResultT

errCtxT :: Functor m => ErrCtx -> ResultT m a -> ResultT m a
errCtxT c = ResultT . fmap (errCtx c) . runResultT

(<||>) :: Monad m => ResultT m a -> ResultT m a -> ResultT m a
ResultT ma <||> ResultT ma' = ResultT $ do
  resA <- ma
  case resA of
    Success ws a -> pure (Success ws a)
    Failure ws eg@(ErrGroup sws ectx es) -> do
      resA' <- ma'
      case resA' of
        -- FIXME: not great..
        Success ws' a' ->
          case NE.nonEmpty sws of
            Nothing -> pure (Success (ws' <> ws) a')
            Just sws' -> pure (Success (WarnOnErrGroup sws' ectx es : ws' <> ws) a')
        Failure ws' eg' -> pure (Failure (ws' <> ws) (mergeErrGroup eg eg'))

--------------

fatal :: Stack -> SomeErr -> Result a
fatal stack e = Failure [] (ErrGroup [] [] (ErrWithStack stack e NE.:| []))

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

tryBoth :: Result a -> Result a -> Result a
tryBoth (Success ws a) _ = Success ws a
-- NOTE: eg on left gets dropped when no warning is present
tryBoth (Failure ws (ErrGroup sws ectx es)) (Success ws' a) =
  case NE.nonEmpty sws of
    Nothing -> Success (ws' <> ws) a
    Just sws' -> Success (WarnOnErrGroup sws' ectx es : ws' <> ws) a
tryBoth (Failure ws eg) (Failure ws' eg') = Failure (ws' <> ws) (mergeErrGroup eg eg')

instance Monad Result where
  Failure ws eg >>= _ = Failure ws eg
  Success ws a >>= k =
    case k a of
      Failure ws' eg' -> Failure (ws' <> ws) eg'
      Success ws' b' -> Success (ws' <> ws) b'

----------

data Stack = Stack
  deriving Show
data SomeWarn = SomeWarn
  deriving Show
data SomeErr = SomeErr
  deriving Show
data ErrCtx = ErrCtx
  deriving Show
