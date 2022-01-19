{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Stack.StickyLog (
  StickyLogStackC (..),
  stickyLogStack,
) where

import Console.Sticky qualified as Sticky
import Control.Carrier.Reader
import Control.Effect.AtomicCounter
import Control.Effect.Lift
import Control.Effect.Stack
import Control.Effect.Sum (Member (inj))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (intersperse)
import Effect.Logger

stickyLogStack :: (Has AtomicCounter sig m, Has (Lift IO) sig m) => StickyLogStackC m a -> m a
stickyLogStack act = do
  taskId <- generateId
  Sticky.withStickyRegion SevDebug $ \region ->
    runReader (StickyCtx (TaskId taskId) region) . runStickyLogStackC $ act

data StickyCtx = StickyCtx
  { ctxTaskId :: TaskId
  , ctxRegion :: Sticky.StickyRegion
  }

newtype TaskId = TaskId Int

newtype StickyLogStackC m a = StickyLogStackC {runStickyLogStackC :: ReaderC StickyCtx m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance (Algebra sig m, Member Stack sig, Member (Lift IO) sig, Member Logger sig) => Algebra (Stack :+: sig) (StickyLogStackC m) where
  alg hdl sig ctx = StickyLogStackC $ case sig of
    L thing@(Context txt _) -> do
      TaskId taskId <- asks ctxTaskId
      region <- asks ctxRegion
      stack <- getStack
      let chevron = annotate (color Yellow) (pretty (" > " :: String))
      let path = map pretty (stack <> [txt])
      let formatted = "[" <> annotate (color Green) ("TASK " <> pretty taskId) <> "] " <> hcat (intersperse chevron path)
      Sticky.setSticky' region formatted
      alg (runStickyLogStackC . hdl) (inj thing) ctx
    L somethingElse -> alg (runStickyLogStackC . hdl) (inj somethingElse) ctx
    R other -> alg (runStickyLogStackC . hdl) (R other) ctx
