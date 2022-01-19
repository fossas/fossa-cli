module Diag.Result (
  -- * Result carrier
  ResultT (..),
  fatalT,
  warnT,
  recoverT,
  errorBoundaryT,
  rethrowT,
  withWarnT,
  errCtxT,
  (<||>),

  -- * Result type
  Result (..),
  SomeErr (..),
  SomeWarn (..),
  ErrCtx (..),
  Stack (..),

  -- * Result operations
  fatal,
  warn,
  recover,
  errorBoundary,
  rethrow,
  tryBoth,
  getWarnings,
  withWarn,
  errCtx,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
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

data ErrWithStack = ErrWithStack Stack SomeErr
  deriving (Show)

--------------

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
fatalT stack err = ResultT (pure (fatal stack err))
{-# INLINE fatalT #-}

warnT :: Applicative m => SomeWarn -> ResultT m ()
warnT = ResultT . pure . warn
{-# INLINE warnT #-}

recoverT :: Functor m => ResultT m a -> ResultT m (Maybe a)
recoverT = ResultT . fmap recover . runResultT
{-# INLINE recoverT #-}

errorBoundaryT :: Functor m => ResultT m a -> ResultT m (Result a)
errorBoundaryT = ResultT . fmap pure . runResultT
{-# INLINE errorBoundaryT #-}

rethrowT :: Applicative m => Result a -> ResultT m a
rethrowT = ResultT . pure
{-# INLINE rethrowT #-}

withWarnT :: Functor m => SomeWarn -> ResultT m a -> ResultT m a
withWarnT w = ResultT . fmap (withWarn w) . runResultT
{-# INLINE withWarnT #-}

errCtxT :: Functor m => ErrCtx -> ResultT m a -> ResultT m a
errCtxT c = ResultT . fmap (errCtx c) . runResultT
{-# INLINE errCtxT #-}

(<||>) :: Monad m => ResultT m a -> ResultT m a -> ResultT m a
ResultT ma <||> ResultT ma' = ResultT $ do
  resA <- ma
  case resA of
    Success ws a -> pure (Success ws a)
    failure@(Failure ws eg) -> do
      resA' <- ma'
      case resA' of
        Success ws' a' -> pure (Success (ws' <> getWarnings failure) a')
        Failure ws' eg' -> pure (Failure (ws' <> ws) (mergeErrGroup eg eg'))
{-# INLINE (<||>) #-}

--------------

fatal :: Stack -> SomeErr -> Result a
fatal stack e = Failure [] (ErrGroup [] [] (ErrWithStack stack e NE.:| []))

warn :: SomeWarn -> Result ()
warn w = Success [StandaloneWarn w] ()

recover :: Result a -> Result (Maybe a)
recover failure@Failure{} = Success (getWarnings failure) Nothing
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

getWarnings :: Result a -> [EmittedWarn]
getWarnings (Success ws _) = ws
getWarnings (Failure ws (ErrGroup sws ectx es)) =
  -- FIXME: errors from errgroup get dropped when no warning is present
  case NE.nonEmpty sws of
    Nothing -> ws
    Just sws' -> (WarnOnErrGroup sws' ectx es : ws)

mergeErrGroup :: ErrGroup -> ErrGroup -> ErrGroup
mergeErrGroup (ErrGroup sws ectx nee) (ErrGroup sws' ectx' nee') = ErrGroup (sws' <> sws) (ectx' <> ectx) (nee' <> nee)

tryBoth :: Result a -> Result a -> Result a
tryBoth (Success ws a) _ = Success ws a
tryBoth failure@Failure{} (Success ws' a) = Success (ws' <> getWarnings failure) a
tryBoth (Failure ws eg) (Failure ws' eg') = Failure (ws' <> ws) (mergeErrGroup eg eg')

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

----------

gradleExample :: Monad m => ResultT m ()
gradleExample =
  errCtxT (ErrCtx @Text "some context about the gradle command") $
    tryGradleW
      <||> tryGradleWExe
      <||> tryGradle
  where
    tryGradleW = fatalT (Stack []) (SomeErr @Text "blah gradlew")
    tryGradleWExe = fatalT (Stack []) (SomeErr @Text "blah gradlewexe")
    tryGradle = fatalT (Stack []) (SomeErr @Text "blah gradle")

pipenvExample :: Monad m => ResultT m ()
pipenvExample = do
  direct <-
    errCtxT (ErrCtx @Text "some context about parsing pipfile lock") $
      pure ()

  deep <-
    recoverT
      . withWarnT (SomeWarn @Text "missing deps")
      . withWarnT (SomeWarn @Text "missing edges")
      . errCtxT (ErrCtx @Text "blah blah pipenv command")
      $ fatalT (Stack []) (SomeErr @Text "oh no pipenv command failed")

  pure ()
