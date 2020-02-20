-- | Legacy/deprecated GraphBuilder interface. Kept for compatibility with old code
module App.Scan.GraphBuilder
  ( GraphBuilder(..)
  , addNode
  , addEdge
  , addDirect
  , runGraphBuilder
  , evalGraphBuilder
  )
  where

import Prologue hiding (parent)

import Control.Algebra
import Control.Carrier.State.Strict
import qualified App.Scan.Graph as G
import DepTypes

data GraphBuilder m k
  = AddNode Dependency (G.DepRef -> m k)
  | AddEdge G.DepRef G.DepRef (m k)
  | AddDirect G.DepRef (m k)
  deriving Generic1

instance HFunctor GraphBuilder
instance Effect GraphBuilder

-- | Add a node to the graph. See 'G.addNode'
addNode :: Has GraphBuilder sig m => Dependency -> m G.DepRef
addNode node = send (AddNode node pure)

-- | @addEdge parent child@ adds an edge to the graph between parent and child nodes. See 'G.addEdge'
addEdge :: Has GraphBuilder sig m => G.DepRef -> G.DepRef -> m ()
addEdge parent child = send (AddEdge parent child (pure ()))

-- | Add a direct dependency to the graph. See 'G.addDirect'
addDirect :: Has GraphBuilder sig m => G.DepRef -> m ()
addDirect direct = send (AddDirect direct (pure ()))

runGraphBuilder :: G.Graph -> GraphBuilderC m a -> m (G.Graph, a)
runGraphBuilder start = runState start . coerce

evalGraphBuilder :: Functor m => G.Graph -> GraphBuilderC m a -> m G.Graph
evalGraphBuilder start = execState start . runGraphBuilderC

newtype GraphBuilderC m a = GraphBuilderC { runGraphBuilderC :: StateC G.Graph m a }
 deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Effect sig) => Algebra (GraphBuilder :+: sig) (GraphBuilderC m) where
  alg (R other) = alg (R (handleCoercible other))

  alg (L act) = case act of
    AddNode dep k -> GraphBuilderC (state (G.addNode dep)) >>= k
    AddEdge parent child k -> GraphBuilderC (modify (G.addEdge parent child)) *> k
    AddDirect dep k -> GraphBuilderC (modify (G.addDirect dep)) *> k

state :: Has (State s) sig m => (s -> (a,s)) -> m a
state f = do
  before <- get
  let (result, after) = f before
  result <$ put after

