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
import Discovery.Filters (FilterSet (scopes), MavenScopeFilters (..))
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

-- | Filter all submodules (including their dependencies) that are not in includedSubmoduleSet
--
--   Given a MavenDependency graph with structure:
--
-- >       d1
-- >      |    \
-- >      |      \
-- >      d2  ->   d3
-- >      |
-- >      d4
-- >   d1 has edges to d2 & d3
-- >   d2 has edges to d3 & d4
--
--   Where d1, d2, d3, and d4 are instaniated as:
--
-- >    d1 = MavenDependency
-- >       { dependency : Dependency {dependencyName: "exec"}
-- >       , dependencySubmodules : mempty
-- >       }
--
-- >    d2 = MavenDependency
-- >       { dependency : Dependency {dependencyName: "lib"}
-- >       , dependencySubmodules : mempty
-- >       }
--
-- >    d3 = MavenDependency
-- >       { dependency : Dependency {dependencyName: "junit"}
-- >       , dependencySubmodules : mempty
-- >       }
--
-- >    d4 = MavenDependency
-- >       { dependency : Dependency {dependencyName: "joda-time"}
-- >       , dependencySubmoduls : mempty
-- >       }
--
--   In this example, the filter function works as follows:
--      1. Retrieve the submodule nodes from the graph in the form of a list. In this case, it would be: [d1, d2]
--         This is done by checking if the MavenDependency's dependencyName is is the includedSubmoduleSet.
--
--      2. Obtain the submodule's reachable nodes by calling `reachableSuccessorsWithCondition`.
--         The condition that we pass to this function is `notSubmodule`, which ensures that we do not retrieve
--         children nodes that submodules. This happens when submodules have dependencies to other submodules.
--
--      3. Update `dependencySubmodules` for the current submodule and all of its reachable nodes.
--         The `dependencyName` of the current submodule will be added to this set.
--
--      4. Repeat steps 2 & 3 for every submodule node. This is done by calling `coloredGraph`.
--
--      5. Traverse the graph and check if the current node's `dependencySubmodules` contains at least one submodule
--         in `includedSubmoduleSet`. If the check passes, we include the node in our final graph.
--
--  NOTE: If we do not check if a child is a submodule when calling `reachableSuccessorsWithCondition`,
--        it can lead to including dependencies that should not have been excluded in the final graph!
--
--        Consider the following:
-- >          includedSubmoduleSet = Set ("exec")
-- >          completeSubmoduleSet = Set ("exec", "lib")
--
--        If we did not check if a child was submodule, then d2's `dependencySubmodules` would be:
--
-- >          Set ("exec" , "lib")
--
--        This means that nothing would be filtered and the graph remains the same.
--        Instead, the final graph should consist of d1 and d3.
--        d2 and d4 are filtered because d2 is not in `includedSubmoduleSet`.
--        d3 remains because d1 should be included and it has a direct edge to d3.
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
      let children = reachableSuccessorsWithCondition edgeList mavenDep notSubmodule completeSubmoduleSet $ Set.fromList []
      children `Set.union` Set.fromList [mavenDep]

    -- For each submodule, color all of its dependencies.
    -- Tag each dependency with the submodule that it belongs to.
    -- If a submodule has a dependency to another submodule it will not tag the dependency submodule and their successors.
    -- This allows us to maintain shared depenedencies between filtered and included submodules
    coloredGraph :: Set MavenDependency -> Graphing MavenDependency -> Graphing MavenDependency
    coloredGraph submodules g =
      foldr (\submodule acc -> color acc dependencySubmodules updateDependencySubmodules submodule depNameFromMavenDependency (reachableNodesFromSubmodule $ depNameFromMavenDependency submodule)) g submodules

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
