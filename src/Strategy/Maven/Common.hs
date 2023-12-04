{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.Common (
  MavenDependency (..),
  mavenDependencyToDependency,
  filterMavenSubmodules,
  --   filterMavenDependencyByScope,
) where

import Data.Set (Set, unions)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (Dependency (..))

-- import Discovery.Filters (FilterSet (scopes), MavenScopeFilters (..))

import Algebra.Graph.AdjacencyMap qualified as AM
import Data.Foldable (Foldable (foldl'), for_)
import Debug.Trace (trace, traceM)
import Graphing (Graphing, Node (Node), deleteNodeAndChildren, filter, reachableNodes, shrink, vertexList)

data MavenDependency = MavenDependency
  { dependency :: Dependency
  , dependencyScopes :: Set Text
  }
  deriving (Eq, Ord, Show)

mavenDependencyToDependency :: MavenDependency -> Dependency
mavenDependencyToDependency MavenDependency{..} = dependency

filterMavenSubmodules :: Monad m => Set Text -> Set Text -> Graphing MavenDependency -> m (Graphing MavenDependency)
filterMavenSubmodules targetSet submoduleSet graph = do
  let submoduleNodesToDelete = Set.fromList . vertexList $ Graphing.filter isSubmoduleToRemove graph
      reachableNodesFromDeletedSubmodules = (reachableNodes submoduleNodesToDelete graph)
      allReachableNodes = Set.fromList $ vertexList graph
      nodesToKeep = allReachableNodes `Set.difference` reachableNodesFromDeletedSubmodules
  traceM ("These are reachable Nodes from deleted submodules +++++++ " ++ show (reachableNodesFromDeletedSubmodules))
  traceM ("All Reachable Nodes +++++++ " ++ show (allReachableNodes))
  traceM ("The difference +++++++ " ++ show (nodesToKeep))

  pure $ Graphing.filter (`Set.member` nodesToKeep) graph
  where
    -- foldl' removeSubmoduleAndDependencies graph submoduleNodesToDelete

    isSubmoduleToRemove :: MavenDependency -> Bool
    isSubmoduleToRemove MavenDependency{..} = dependencyName dependency `Set.notMember` targetSet && dependencyName dependency `Set.member` submoduleSet

    keepSubmodule :: MavenDependency -> Bool
    keepSubmodule MavenDependency{..} = dependencyName dependency `Set.member` targetSet

    removeSubmoduleAndDependencies :: Graphing MavenDependency -> MavenDependency -> Graphing MavenDependency
    removeSubmoduleAndDependencies g (mavenDep) = deleteNodeAndChildren keepSubmodule mavenDep g

-- nodes :: Set MavenDependency -> [MavenDependency]
-- nodes submoduleNodes = Prelude.filter (`Set.member` submoduleNodes) $ AM.vertexList graph

-- filterMavenDependencyByScope :: MavenScopeFilters -> Graphing MavenDependency -> Graphing MavenDependency
-- filterMavenDependencyByScope scopeFilters = Graphing.shrink isMavenDependencyIncluded
--   where
--     isMavenDependencyIncluded :: MavenDependency -> Bool
--     isMavenDependencyIncluded MavenDependency{..} = case scopeFilters of
--       MavenScopeIncludeFilters includeSet -> do
--         let includeScopes = scopes includeSet
--         case (Set.null dependencyScopes, Set.null includeScopes) of
--           (False, False) -> dependencyScopes `Set.isSubsetOf` includeScopes
--           (False, True) -> True
--           (True, False) -> False
--           (True, True) -> True
--       MavenScopeExcludeFilters excludeSet -> do
--         let excludeScopes = scopes excludeSet
--         case (Set.null dependencyScopes, Set.null excludeScopes) of
--           (False, False) -> dependencyScopes `Set.disjoint` excludeScopes
--           (False, True) -> True
--           (True, _) -> True{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.Common (
  MavenDependency (..),
  mavenDependencyToDependency,
  filterMavenDependencyByScope,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (Dependency)
import Discovery.Filters (FilterSet (scopes), MavenScopeFilters (..))
import Graphing (Graphing, shrink)

data MavenDependency = MavenDependency
  { dependency :: Dependency
  , dependencyScopes :: Set Text
  }
  deriving (Eq, Ord, Show)

mavenDependencyToDependency :: MavenDependency -> Dependency
mavenDependencyToDependency MavenDependency{..} = dependency

filterMavenDependencyByScope :: MavenScopeFilters -> Graphing MavenDependency -> Graphing MavenDependency
filterMavenDependencyByScope scopeFilters = Graphing.shrink isMavenDependencyIncluded
  where
    isMavenDependencyIncluded :: MavenDependency -> Bool
    isMavenDependencyIncluded MavenDependency{..} = case scopeFilters of
      MavenScopeIncludeFilters includeSet -> do
        let includeScopes = scopes includeSet
        case (Set.null dependencyScopes, Set.null includeScopes) of
          (False, False) -> dependencyScopes `Set.isSubsetOf` includeScopes
          (False, True) -> True
          (True, False) -> False
          (True, True) -> True
      MavenScopeExcludeFilters excludeSet -> do
        let excludeScopes = scopes excludeSet
        case (Set.null dependencyScopes, Set.null excludeScopes) of
          (False, False) -> dependencyScopes `Set.disjoint` excludeScopes
          (False, True) -> True
          (True, _) -> True
