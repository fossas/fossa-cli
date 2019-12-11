{-# language TemplateHaskell #-}

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

import Polysemy
import Polysemy.State

import qualified App.Scan.Graph as G
import DepTypes

data GraphBuilder m a where
  AddNode   :: Dependency -> GraphBuilder m G.DepRef
  AddEdge   :: G.DepRef -> G.DepRef -> GraphBuilder m ()
  AddDirect :: G.DepRef -> GraphBuilder m ()

makeSem_ ''GraphBuilder

-- | Add a node to the graph. See 'G.addNode'
addNode :: Member GraphBuilder r => Dependency -> Sem r G.DepRef

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

state :: Member (State s) r => (s -> (a,s)) -> Sem r a
state f = do
  before <- get
  let (result, after) = f before
  result <$ put after

