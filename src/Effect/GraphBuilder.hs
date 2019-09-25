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
  )
  where

import qualified Graph as G
import Polysemy
import Polysemy.State

data GraphBuilder m a where
  AddNode   :: G.Dependency -> GraphBuilder m G.DepRef
  AddEdge   :: G.DepRef -> G.DepRef -> GraphBuilder m ()
  AddDirect :: G.DepRef -> GraphBuilder m ()

makeSem ''GraphBuilder

-- | Run a GraphBuilder computation, returning both the graph and the result
runGraphBuilder :: Sem (GraphBuilder ': r) a -> Sem r (G.Graph, a)
runGraphBuilder = runState G.empty . graphBuilderToState

-- | Discard the result from a GraphBuilder computation, returning the graph
evalGraphBuilder :: Sem (GraphBuilder ': r) a -> Sem r G.Graph
evalGraphBuilder = fmap fst . runGraphBuilder

-- | Run a GraphBuilder computation, returning both the graph and the result
-- This can be significanty faster than 'runGraphBuilder'
runGraphBuilderIO :: Member (Embed IO) r => Sem (GraphBuilder ': r) a -> Sem r (G.Graph, a)
runGraphBuilderIO = stateToIO G.empty . graphBuilderToState

-- | Discard the result from a GraphBuilder computation, returning the graph
-- This can be significantly faster than evalGraphBuilder
evalGraphBuilderIO :: Member (Embed IO) r => Sem (GraphBuilder ': r) a -> Sem r G.Graph
evalGraphBuilderIO = fmap fst . runGraphBuilderIO

graphBuilderToState :: Sem (GraphBuilder ': r) a -> Sem (State G.Graph ': r) a
graphBuilderToState = reinterpret $ \case
  AddNode dep -> state (G.addNode dep)
  AddEdge parent child -> modify (G.addEdge parent child)
  AddDirect ref -> modify (G.addDirect ref)

state :: Member (State s) r => (s -> (a,s)) -> Sem r a
state f = do
  before <- get
  let (result, after) = f before
  result <$ put after

