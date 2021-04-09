{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Legacy/deprecated GraphBuilder interface. Kept for compatibility with old code
module App.Fossa.Analyze.GraphBuilder
  ( GraphBuilder(..)
  , addNode
  , addEdge
  , addDirect
  , runGraphBuilder
  , evalGraphBuilder
  )
  where

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Monad.IO.Class (MonadIO)
import qualified App.Fossa.Analyze.Graph as G
import Data.Kind (Type)
import DepTypes

data GraphBuilder (m :: Type -> Type) k where
  AddNode :: Dependency -> GraphBuilder m G.DepRef
  AddEdge :: G.DepRef -> G.DepRef -> GraphBuilder m ()
  AddDirect :: G.DepRef -> GraphBuilder m ()

-- | Add a node to the graph. See 'G.addNode'
addNode :: Has GraphBuilder sig m => Dependency -> m G.DepRef
addNode node = send (AddNode node)

-- | @addEdge parent child@ adds an edge to the graph between parent and child nodes. See 'G.addEdge'
addEdge :: Has GraphBuilder sig m => G.DepRef -> G.DepRef -> m ()
addEdge parent child = send (AddEdge parent child)

-- | Add a direct dependency to the graph. See 'G.addDirect'
addDirect :: Has GraphBuilder sig m => G.DepRef -> m ()
addDirect direct = send (AddDirect direct)

runGraphBuilder :: G.Graph -> GraphBuilderC m a -> m (G.Graph, a)
runGraphBuilder start = runState start . runGraphBuilderC

evalGraphBuilder :: Functor m => G.Graph -> GraphBuilderC m a -> m G.Graph
evalGraphBuilder start = execState start . runGraphBuilderC

newtype GraphBuilderC m a = GraphBuilderC { runGraphBuilderC :: StateC G.Graph m a }
 deriving (Functor, Applicative, Monad, MonadIO)

instance Algebra sig m => Algebra (GraphBuilder :+: sig) (GraphBuilderC m) where
  alg hdl sig ctx = GraphBuilderC $ case sig of
    R other -> alg (runGraphBuilderC . hdl) (R other) ctx
    L (AddNode dep) -> (<$ ctx) <$> state (G.addNode dep)
    L (AddEdge parent child) -> (<$ ctx) <$> modify (G.addEdge parent child)
    L (AddDirect dep) -> (<$ ctx) <$> modify (G.addDirect dep)
