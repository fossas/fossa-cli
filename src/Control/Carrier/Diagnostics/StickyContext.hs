{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Diagnostics.StickyContext (
  StickyDiagC (..),
  stickyDiag,
) where

import Console.Sticky qualified as Sticky
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Sum (Member (inj))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (intersperse)
import Data.Text qualified as T
import Effect.Logger
import Control.Effect.AtomicCounter

stickyDiag :: (Has AtomicCounter sig m, Has (Lift IO) sig m) => StickyDiagC m a -> m a
stickyDiag act = do
  taskId <- generateId
  Sticky.withStickyRegion $ \region ->
    runReader (StickyCtx (TaskId taskId) [] region) . runStickyDiagC $ act

data StickyCtx = StickyCtx
  { ctxTaskId :: TaskId
  , ctxBacktrace :: [T.Text]
  , ctxRegion :: Sticky.StickyRegion
  }

newtype TaskId = TaskId Int

newtype StickyDiagC m a = StickyDiagC {runStickyDiagC :: ReaderC StickyCtx m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance (Algebra sig m, Member Diag.Diagnostics sig, Member (Lift IO) sig, Member Logger sig) => Algebra (Diag.Diagnostics :+: sig) (StickyDiagC m) where
  alg hdl sig ctx = StickyDiagC $ case sig of
    L thing@(Diag.Context txt _) -> local (\stickyCtx -> stickyCtx { ctxBacktrace = txt : ctxBacktrace stickyCtx }) $ do
      TaskId taskId <- asks ctxTaskId
      region <- asks ctxRegion
      context <- asks ctxBacktrace
      let chevron = annotate (color Yellow) (pretty (" > " :: String))
      let path = map pretty (reverse context)
      let formatted = "[" <> annotate (color Green) ("TASK " <> pretty taskId) <> "] " <> hcat (intersperse chevron path)
      Sticky.setSticky' region formatted
      alg (runStickyDiagC . hdl) (inj thing) ctx
    L somethingElse -> alg (runStickyDiagC . hdl) (inj somethingElse) ctx
    R other -> alg (runStickyDiagC . hdl) (R other) ctx
