-- | LabeledGrapher is a thin State wrapper effect around 'L.LabeledGraphing'
--
-- It defines @direct@, @edge@, and @label@ combinators analagous to 'L.direct',
-- 'L.edge', and 'L.label' from 'L.LabeledGraphing'
--
-- See also: 'withLabeling', which captures a common pattern when building graphs
module Effect.LabeledGrapher
  ( LabeledGrapher(..)
  , direct
  , edge
  , label

  , LabeledGrapherC(..)
  , runLabeledGrapher
  , evalLabeledGrapher
  , withLabeling

  -- re-exports
  , L.PkgLabel
  , module X
  ) where

import Prologue hiding (parent)

import Control.Algebra as X
import Control.Carrier.State.Strict

import qualified Graphing as G
import qualified LabeledGraphing as L

data LabeledGrapher ty m k
  = Direct ty (m k)
  | Edge ty ty (m k)
  | Label ty (L.PkgLabel ty) (m k)
  deriving (Generic1)

instance HFunctor (LabeledGrapher ty)
instance Effect (LabeledGrapher ty)

direct :: Has (LabeledGrapher ty) sig m => ty -> m ()
direct ty = send (Direct ty (pure ()))

edge :: Has (LabeledGrapher ty) sig m => ty -> ty -> m ()
edge parent child = send (Edge parent child (pure ()))

label :: Has (LabeledGrapher ty) sig m => ty -> (L.PkgLabel ty) -> m ()
label ty lbl = send (Label ty lbl (pure ()))

runLabeledGrapher :: LabeledGrapherC ty m a -> m (L.LabeledGraphing ty, a)
runLabeledGrapher = runState L.empty . runLabeledGrapherC

evalLabeledGrapher :: Functor m => LabeledGrapherC ty m a -> m (L.LabeledGraphing ty)
evalLabeledGrapher = execState L.empty . runLabeledGrapherC

newtype LabeledGrapherC ty m a = LabeledGrapherC { runLabeledGrapherC :: StateC (L.LabeledGraphing ty) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Effect sig, Ord ty, Ord (L.PkgLabel ty)) => Algebra (LabeledGrapher ty :+: sig) (LabeledGrapherC ty m) where
  alg (L act) = case act of
    Direct ty k -> LabeledGrapherC (modify (L.direct ty)) *> k
    Edge parent child k -> LabeledGrapherC (modify (L.edge parent child)) *> k
    Label ty lbl k -> LabeledGrapherC (modify (L.label ty lbl)) *> k
  alg (R other) = LabeledGrapherC (alg (R (handleCoercible other)))

withLabeling :: (Functor m, Ord ty, Ord res) => (ty -> Set (L.PkgLabel ty) -> res) -> LabeledGrapherC ty m a -> m (G.Graphing res)
withLabeling f = fmap (L.unlabel f) . evalLabeledGrapher
