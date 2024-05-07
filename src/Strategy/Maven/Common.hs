{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.Common (
  MavenDependency (..),
  mavenDependencyToDependency,
  filterMavenSubmodules,
  filterMavenDependencyByScope,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (Dependency (..))

import Discovery.Filters (MavenScopeFilterPredicate (..), MavenScopeFilters (..))

import Data.List qualified as List
import Data.Maybe (isNothing)
import Graphing (Graphing, color, edgesList, reachableSuccessorsWithCondition, vertexList)
import Graphing qualified

data MavenDependency = MavenDependency
  { dependency :: Dependency
  , dependencyScopes :: Set Text
  , -- The submodules that this dependency belongs to
    dependencySubmodules :: Set Text
  }
  deriving (Eq, Ord, Show)

mavenDependencyToDependency :: MavenDependency -> Dependency
mavenDependencyToDependency MavenDependency{..} = dependency

-- | Filter all submodules (including their dependencies) that are not in `includedSubmoduleSet`.
--
--   This is achieved by:
--      1. Retrieving every submodule dependency in the graph
--      2. Traversing each submodule's children dependencies
--      3. Coloring every dependency with contextual information about which submodules they belong to
--
--  NOTE: We cannot naively perform submodule filtering by removing every dependency originating from a filtered submodule dependency
--
--  In this example:
--
--    MavenDependency graph:
--
-- >        d1
-- >        |    \
-- >        |      \
-- >        d2  ->   d3
-- >        |
-- >        d4
-- >    d1 has edges to d2 & d3
-- >    d2 has edges to d3 & d4
--
--    Where d1, d2, d3, and d4 are instantiated as:
--
-- >      d1 = MavenDependency
-- >        { dependency : Dependency {dependencyName: "exec"}
-- >        , dependencySubmodules : Set.empty
-- >        }
--
-- >      d2 = MavenDependency
-- >        { dependency : Dependency {dependencyName: "lib"}
-- >        , dependencySubmodules : Set.empty
-- >        }
--
-- >      d3 = MavenDependency
-- >        { dependency : Dependency {dependencyName: "junit"}
-- >        , dependencySubmodules : Set.empty
-- >        }
--
-- >      d4 = MavenDependency
-- >        { dependency : Dependency {dependencyName: "joda-time"}
-- >        , dependencySubmoduls : Set.empty
-- >        }
--
-- >   completeSubmoduleSet = Set ("exec", "lib")
-- >   includedSubmoduleSet = Set ("lib")
--
--   Naively filtering the `lib` submodule and its dependencies would result in a graph only containing d1.
--   This is incorrect as d3 should have been included as it was a dependency to d1, irrespective to its transitive dependencies from d2.
--
--   For these reasons, we need a different mechanism to accurately filter submodules, which is described above.
--
--   We use `reachableSuccessorsWithCondition` to retrieve a submodule's dependencies so long as the dependency is not
--   also a submodule in the project. If we do not check if a child dependency is a submodule when calling
--   `reachableSuccessorsWithCondition` it can lead to including dependencies that should have been excluded in the final graph!
--
--        Consider the following scenario using the same MavenDependency graph:
--
-- >          completeSubmoduleSet = Set ("exec", "lib")
-- >          includedSubmoduleSet = Set ("exec")
--
--        If we did not check if a child was submodule, then d2's `dependencySubmodules` would be:
--
-- >          Set ("exec" , "lib")
--
--        This means that nothing would be filtered as d1 can reach every node in the graph.
--        Instead, the final graph should consist of d1 and d3.
--        d2 and d4 are filtered because d2 is not in `includedSubmoduleSet`.
--        d3 remains because d1 has a direct edge to d3.
filterMavenSubmodules :: Set Text -> Set Text -> Graphing MavenDependency -> Graphing MavenDependency
filterMavenSubmodules includedSubmoduleSet completeSubmoduleSet graph = do
  let submoduleNodes = Set.fromList $ filter (\dep -> depNameFromMavenDependency dep `Set.member` completeSubmoduleSet) $ vertexList graph
  Graphing.filter isMavenDependencyIncluded $ coloredGraph submoduleNodes graph
  where
    isMavenDependencyIncluded :: MavenDependency -> Bool
    isMavenDependencyIncluded MavenDependency{..} = not $ null $ dependencySubmodules `Set.intersection` includedSubmoduleSet

    notSubmodule :: Text -> Set Text -> Bool
    notSubmodule depName submodules = depName `Set.notMember` submodules

    edgeList :: [(Text, Text)]
    edgeList = mapToDependencyNames $ edgesList graph

    mapToDependencyNames :: [(MavenDependency, MavenDependency)] -> [(Text, Text)]
    mapToDependencyNames = concatMap (\(dep1, dep2) -> [(depNameFromMavenDependency dep1, depNameFromMavenDependency dep2)])

    depNameFromMavenDependency :: MavenDependency -> Text
    depNameFromMavenDependency = dependencyName . dependency

    updateDependencySubmodules :: Set Text -> MavenDependency -> MavenDependency
    updateDependencySubmodules updatedSubmoduleSet mavenDep = mavenDep{dependencySubmodules = updatedSubmoduleSet}

    -- Given a submodule name,  reachableNodesFromSubmodule finds all dependencies of a submodule (excluding dependencies that are submodules)
    -- reachableNodesOnCondition returns all of the submodule's children, so we need to union the set with the submodule to get a complete set (origin + children)
    reachableNodesFromSubmodule :: Text -> Set Text
    reachableNodesFromSubmodule mavenDep = do
      let children = reachableSuccessorsWithCondition edgeList mavenDep notSubmodule completeSubmoduleSet Set.empty
      children `Set.union` Set.fromList [mavenDep]

    -- For each submodule, color all of its dependencies.
    -- Tag each dependency with the submodule that it belongs to.
    -- If a submodule has a dependency to another submodule it will not tag the dependency submodule and their successors.
    -- This allows us to maintain shared depenedencies between filtered and included submodules.
    coloredGraph :: Set MavenDependency -> Graphing MavenDependency -> Graphing MavenDependency
    coloredGraph submodules g =
      foldr (\submodule acc -> color acc dependencySubmodules updateDependencySubmodules submodule depNameFromMavenDependency (reachableNodesFromSubmodule $ depNameFromMavenDependency submodule)) g submodules

filterMavenDependencyByScope :: MavenScopeFilters -> Graphing MavenDependency -> Graphing MavenDependency
filterMavenDependencyByScope scopeFilters = Graphing.shrink $ mavenDependencyShouldBeIncluded scopeFilters
  where
    mavenDependencyShouldBeIncluded :: MavenScopeFilters -> MavenDependency -> Bool
    mavenDependencyShouldBeIncluded (MavenScopeOnlyFilters filters) MavenDependency{dependencyScopes} | Set.null dependencyScopes = Set.null filters
    mavenDependencyShouldBeIncluded (MavenScopeExcludeFilters _) MavenDependency{dependencyScopes} | Set.null dependencyScopes = True
    mavenDependencyShouldBeIncluded (MavenScopeOnlyFilters predicates) MavenDependency{dependencyScopes} = Set.isSubsetOf predicates dependencyScopes
    mavenDependencyShouldBeIncluded (MavenScopeExcludeFilters predicates) dep = isNothing . List.find (mavenScopeFilterPredicateMatches dep) $ Set.toList predicates

    mavenScopeFilterPredicateMatches :: MavenDependency -> MavenScopeFilterPredicate -> Bool
    mavenScopeFilterPredicateMatches MavenDependency{dependencyScopes} _ | Set.null dependencyScopes = True
    mavenScopeFilterPredicateMatches MavenDependency{dependencyScopes} (MavenScopeFilterPredicateSingle predicates) = Set.member predicates dependencyScopes
    mavenScopeFilterPredicateMatches MavenDependency{dependencyScopes} (MavenScopeFilterPredicateCombined predicates) = Set.isSubsetOf predicates dependencyScopes
