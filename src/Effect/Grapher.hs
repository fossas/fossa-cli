-- | Grapher is a thin State wrapper effect around 'G.Graphing'
--
-- It defines @direct@ and @edge@ combinators analagous to 'G.direct' and
-- 'G.edge' from 'G.Graphing'
module Effect.Grapher
  ( Grapher(..)
  , direct
  , edge

  , evalGrapher
  , runGrapher

  , module X
  ) where

import Prologue hiding (parent)

import Control.Algebra as X
import Control.Carrier.State.Strict

import qualified Graphing as G

data Grapher ty m k
  = Direct ty (m k)
  | Edge ty ty (m k)
  deriving (Generic1)

instance HFunctor (Grapher ty)
instance Effect (Grapher ty)

direct :: Has (Grapher ty) sig m => ty -> m ()
direct ty = send (Direct ty (pure ()))

edge :: Has (Grapher ty) sig m => ty -> ty -> m ()
edge parent child = send (Edge parent child (pure ()))

runGrapher :: GrapherC ty m a -> m (G.Graphing ty, a)
runGrapher = runState G.empty . runGrapherC

evalGrapher :: Functor m => GrapherC ty m a -> m (G.Graphing ty)
evalGrapher = execState G.empty . runGrapherC

newtype GrapherC ty m a = GrapherC { runGrapherC :: StateC (G.Graphing ty) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Effect sig, Ord ty) => Algebra (Grapher ty :+: sig) (GrapherC ty m) where
  alg (L act) = case act of
    Direct ty k -> GrapherC (modify (G.direct ty)) *> k
    Edge parent child k -> GrapherC (modify (G.edge parent child)) *> k
  alg (R other) = GrapherC (alg (R (handleCoercible other)))
