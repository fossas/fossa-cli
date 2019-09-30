{-# language TemplateHaskell #-}

module Effect.GraphBuilder
  ( GraphBuilder(..)
  , addNode
  , addEdge
  , addDirect
  , runGraphBuilder
  , runGraphBuilderIO
  , evalGraphBuilder
  , evalGraphBuilderIO

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

makeSem ''GraphBuilder

-- | Run a GraphBuilder computation, returning both the graph and the result
runGraphBuilder :: Sem (GraphBuilder ': r) a -> Sem r (G.Graph, a)
runGraphBuilder = runState G.empty . graphBuilderToState
{-# INLINE runGraphBuilder #-}

-- | Discard the result from a GraphBuilder computation, returning the graph
evalGraphBuilder :: Sem (GraphBuilder ': r) a -> Sem r G.Graph
evalGraphBuilder = fmap fst . runGraphBuilder
{-# INLINE evalGraphBuilder #-}

-- | Run a GraphBuilder computation, returning both the graph and the result
-- This can be significanty faster than 'runGraphBuilder'
runGraphBuilderIO :: Member (Embed IO) r => Sem (GraphBuilder ': r) a -> Sem r (G.Graph, a)
runGraphBuilderIO = stateToIO G.empty . graphBuilderToState
{-# INLINE runGraphBuilderIO #-}

-- | Discard the result from a GraphBuilder computation, returning the graph
-- This can be significantly faster than 'evalGraphBuilder'
evalGraphBuilderIO :: Member (Embed IO) r => Sem (GraphBuilder ': r) a -> Sem r G.Graph
evalGraphBuilderIO = fmap fst . runGraphBuilderIO
{-# INLINE evalGraphBuilderIO #-}

graphBuilderToState :: Sem (GraphBuilder ': r) a -> Sem (State G.Graph ': r) a
graphBuilderToState = reinterpret $ \case
  AddNode dep -> state (G.addNode dep)
  AddEdge parent child -> modify (G.addEdge parent child)
  AddDirect ref -> modify (G.addDirect ref)
{-# INLINE graphBuilderToState #-}

-- | @unfold direct getDeps toDependency@ unfolds a graph, given:
--
-- - The @direct@ dependencies in the graph
--
-- - A way to @getDeps@ for a dependency
--
-- - A way to convert a dependency @toDependency@
unfold :: Member GraphBuilder r => [dep] -> (dep -> [dep]) -> (dep -> G.Dependency) -> Sem r ()
unfold direct getDeps toDependency = do
  topLevel <- traverse buildNode direct
  traverse_ addDirect topLevel

  where

  -- buildNode :: dep -> Sem r ()
  buildNode dep = do
    children <- traverse buildNode (getDeps dep)
    parentRef <- addNode (toDependency dep)
    traverse_ (addEdge parentRef) children
    pure parentRef

-- TODO: unfoldOrd -- unfold, but deduplicating and eliminating cycles via Ord

state :: Member (State s) r => (s -> (a,s)) -> Sem r a
state f = do
  before <- get
  let (result, after) = f before
  result <$ put after

