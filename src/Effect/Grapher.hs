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

  -- * Labeling
  , LabeledGrapher
  , LabeledGrapherC
  , label
  , withLabeling

  -- * Re-exports
  , module X
  ) where

import Prologue hiding (parent)

import Control.Algebra as X
import Control.Carrier.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

----- Labeling

-- | An extension of 'Grapher' that tracks a set of associated /labels/ for
-- each node in the graph.
--
-- For example, given a node type of:
--
-- > data PipPkg = PipPkg { pipPkgName :: Text, pipPkgVersion :: Text }
-- >   deriving (Eq, Ord, Show)
--
-- We also want to keep track of additional metadata we encounter (or "labels"):
--
-- > data PipLabel =
-- >     PipLocation Text
-- >   | PipEnvironment Text
-- >   deriving (Eq, Ord, Show)
--
-- We eventually use these labels to construct the final @Dependency@s in our
-- graph.
--
-- We create a type synonym:
--
-- > type PipGrapher = LabeledGrapher PipPkg PipLabel
--
-- and now we can use our newly-minted "PipGrapher" effect when describing our
-- dependency graph:
--
-- > doTheThing :: Has PipGrapher sig m => ...
--
-- This allows us to use 'direct' and 'edge' like before, but also gives us a
-- third primitive, 'label'.
--
-- > label :: Has PipGrapher sig m => PipPkg -> PipLabel -> m ()
type LabeledGrapher ty lbl = State (Labels ty lbl) :+: Grapher ty

type Labels ty lbl = Map ty (Set lbl)

type LabeledGrapherC ty lbl m a = StateC (Labels ty lbl) (GrapherC ty m) a

label :: (Ord ty, Ord lbl, Has (LabeledGrapher ty lbl) sig m) => ty -> lbl -> m ()
label ty lbl = modify (insertLabel ty lbl)

insertLabel :: (Ord ty, Ord lbl) => ty -> lbl -> Labels ty lbl -> Labels ty lbl
insertLabel ty lbl = M.insertWith (<>) ty (S.singleton lbl)

-- | An interpreter for @LabeledGrapher@. See existing strategies for examples
withLabeling :: (Monad m, Ord ty, Ord res) => (ty -> Set lbl -> res) -> LabeledGrapherC ty lbl m a -> m (G.Graphing res)
withLabeling f act = do
  (graph,(labels,_)) <- runGrapher . runState M.empty $ act
  pure (unlabel f labels graph)

-- | Transform a @Graphing ty@ into a @Graphing ty'@, given a function that
-- transforms the old node type @ty@ and a set of labels on that node
unlabel :: (Ord ty, Ord ty') => (ty -> Set lbl -> ty') -> Labels ty lbl -> G.Graphing ty -> G.Graphing ty'
unlabel f labels = G.gmap (\ty -> f ty (findLabels ty))
  where
    findLabels ty = fromMaybe S.empty (M.lookup ty labels)
