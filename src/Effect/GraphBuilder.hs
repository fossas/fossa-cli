{-# language TemplateHaskell #-}

module Effect.GraphBuilder
  ( GraphBuilder(..)
  , addNode
  , addEdge
  , addDirect
  , runGraphBuilder
  , evalGraphBuilder

  -- Utility functions
  , unfold
  )
  where

import Prologue hiding (parent)

import qualified Graph as G
import           Polysemy
import           Polysemy.State

data GraphBuilder m a where
  AddNode   :: G.Dependency -> GraphBuilder m G.DepRef
  AddEdge   :: G.DepRef -> G.DepRef -> GraphBuilder m ()
  AddDirect :: G.DepRef -> GraphBuilder m ()

makeSem_ ''GraphBuilder

-- | Add a node to the graph. See 'G.addNode'
addNode :: Member GraphBuilder r => G.Dependency -> Sem r G.DepRef

-- | @addEdge parent child@ adds an edge to the graph between parent and child nodes. See 'G.addEdge'
addEdge :: Member GraphBuilder r => G.DepRef -> G.DepRef -> Sem r ()

-- | Add a direct dependency to the graph. See 'G.addDirect'
addDirect :: Member GraphBuilder r => G.DepRef -> Sem r ()

-- | Run a GraphBuilder computation, returning both the graph and the result
runGraphBuilder :: G.Graph -> Sem (GraphBuilder ': r) a -> Sem r (G.Graph, a)
runGraphBuilder initial = runState initial . reinterpret (\case
  AddNode dep -> state (G.addNode dep)
  AddEdge parent child -> modify (G.addEdge parent child)
  AddDirect ref -> modify (G.addDirect ref))
{-# INLINE runGraphBuilder #-}

-- | Discard the result from a GraphBuilder computation, returning the graph
evalGraphBuilder :: G.Graph -> Sem (GraphBuilder ': r) a -> Sem r G.Graph
evalGraphBuilder initial = fmap fst . runGraphBuilder initial
{-# INLINE evalGraphBuilder #-}

-- | @unfold direct getDeps toDependency@ unfolds a graph, given:
--
-- - The @direct@ dependencies in the graph
--
-- - A way to @getDeps@ for a dependency
--
-- - A way to convert a dependency @toDependency@
unfold :: [dep] -> (dep -> [dep]) -> (dep -> G.Dependency) -> G.Graph
unfold direct getDeps toDependency = run . evalGraphBuilder G.empty $ do
  topLevel <- traverse buildNode direct
  traverse_ addDirect topLevel

  where

  -- buildNode :: dep -> Sem r ()
  buildNode dep = do
    parentRef <- addNode (toDependency dep)
    children <- traverse buildNode (getDeps dep)
    traverse_ (addEdge parentRef) children
    pure parentRef

state :: Member (State s) r => (s -> (a,s)) -> Sem r a
state f = do
  before <- get
  let (result, after) = f before
  result <$ put after

