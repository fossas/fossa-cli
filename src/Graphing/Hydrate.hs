module Graphing.Hydrate (
  hydrate,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AMA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Graphing (Graphing)
import Graphing qualified

-- | Given some 'Graphing a' with an instance of @Hydrateable a b@, update the
-- nodes such that all items (which are type @b@) of a node are copied down to
-- all of its successor nodes.
hydrate :: forall a b. (Ord a, Ord b) => (a -> Set b) -> (Set b -> a -> a) -> Graphing a -> Graphing a
hydrate extractList update gr = Graphing.gmap doPromotion gr
  where
    adjMap :: AM.AdjacencyMap a
    adjMap = Graphing.toAdjacencyMap gr
    -- Get all current nodes which contain a specified subitem
    topVia :: b -> [a]
    topVia subItem = AM.vertexList $ AM.induce (elem subItem . extractList) adjMap
    -- Get all nodes reachable from a list of nodes
    allFrom :: b -> Set a
    allFrom item = Set.fromList $ concatMap (adjMap `AMA.reachable`) (topVia item)

    -- Dedup'd sub-items from all vertices in the AdjMap
    allSubItems :: Set b
    allSubItems = foldMap extractList $ AM.vertexList adjMap

    -- Final map of all promotions, with de-duplicated keys
    promotionMap :: Map a (Set b)
    promotionMap = foldr extract Map.empty allSubItems

    -- Update the map for each sub item
    extract :: b -> Map a (Set b) -> Map a (Set b)
    extract item mapping = foldr (addItemToMap item) mapping $ allFrom item

    addItemToMap :: b -> a -> Map a (Set b) -> Map a (Set b)
    addItemToMap v k = Map.insertWith (<>) k (Set.singleton v)

    doPromotion :: a -> a
    doPromotion node = update (Map.findWithDefault mempty node promotionMap) node
