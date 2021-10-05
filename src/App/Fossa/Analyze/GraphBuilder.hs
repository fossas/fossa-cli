{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Legacy/deprecated GraphBuilder interface. Kept for compatibility with old code
module App.Fossa.Analyze.GraphBuilder (
  GraphBuilder,
  SGraphBuilder (..),
  addNode,
  addEdge,
  addDirect,
  runGraphBuilder,
  evalGraphBuilder,
) where

import App.Fossa.Analyze.Graph qualified as G
import Control.Algebra
import Control.Carrier.Simple
import Control.Carrier.State.Strict
import DepTypes

data SGraphBuilder a where
  AddNode :: Dependency -> SGraphBuilder G.DepRef
  AddEdge :: G.DepRef -> G.DepRef -> SGraphBuilder ()
  AddDirect :: G.DepRef -> SGraphBuilder ()

type GraphBuilder = Simple SGraphBuilder

-- | Add a node to the graph. See 'G.addNode'
addNode :: Has GraphBuilder sig m => Dependency -> m G.DepRef
addNode node = sendSimple (AddNode node)

-- | @addEdge parent child@ adds an edge to the graph between parent and child nodes. See 'G.addEdge'
addEdge :: Has GraphBuilder sig m => G.DepRef -> G.DepRef -> m ()
addEdge parent child = sendSimple (AddEdge parent child)

-- | Add a direct dependency to the graph. See 'G.addDirect'
addDirect :: Has GraphBuilder sig m => G.DepRef -> m ()
addDirect direct = sendSimple (AddDirect direct)

type GraphBuilderC m = SimpleStateC G.Graph SGraphBuilder m

runGraphBuilder :: Algebra sig m => G.Graph -> GraphBuilderC m a -> m (G.Graph, a)
runGraphBuilder start = interpretState start $ \case
  AddNode dep -> state (G.addNode dep)
  AddEdge parent child -> modify (G.addEdge parent child)
  AddDirect dep -> modify (G.addDirect dep)

evalGraphBuilder :: Algebra sig m => G.Graph -> GraphBuilderC m a -> m G.Graph
evalGraphBuilder start = fmap fst . runGraphBuilder start
