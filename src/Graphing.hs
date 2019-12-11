module Graphing
  ( Graphing(..)
  , empty
  , direct
  , edge
  , gmap

  , unfold
  ) where

import Prologue hiding (empty, map, parent)

import           Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Set as S

data Graphing ty = Graphing
  { graphingDirect   :: Set ty
  , graphingAdjacent :: AdjacencyMap ty
  } deriving (Eq, Ord, Show, Generic)

-- Graphing doesn't follow the functor laws
gmap :: (Ord ty, Ord ty') => (ty -> ty') -> Graphing ty -> Graphing ty'
gmap f gr = gr { graphingDirect = direct', graphingAdjacent = adjacent' }
  where
  direct' = S.map f (graphingDirect gr)
  adjacent' = AM.gmap f (graphingAdjacent gr)

empty :: Graphing ty
empty = Graphing S.empty AM.empty

direct :: Ord ty => ty -> Graphing ty -> Graphing ty
direct dep gr = gr { graphingDirect = direct', graphingAdjacent = adjacent' }
  where
  direct' = S.insert dep (graphingDirect gr)
  adjacent' = AM.overlay (AM.vertex dep) (graphingAdjacent gr)

edge :: Ord ty => ty -> ty -> Graphing ty -> Graphing ty
edge parent child gr = gr { graphingAdjacent = adjacent' }
  where
  adjacent' = AM.overlay (AM.edge parent child) (graphingAdjacent gr)

-- | @unfold direct getDeps toDependency@ unfolds a graph, given:
--
-- - The @direct@ dependencies in the graph
--
-- - A way to @getDeps@ for a dependency
--
-- - A way to convert a dependency @toDependency@
unfold :: Ord res => [dep] -> (dep -> [dep]) -> (dep -> res) -> Graphing res
unfold seed getDeps toDependency = foldr addNode empty seed
  where
  addNode dep gr = direct res (foldr (edge res . toDependency) gr children)
    where
    children = getDeps dep
    res = toDependency dep
