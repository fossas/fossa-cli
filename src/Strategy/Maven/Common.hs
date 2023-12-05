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
import Graphing (Graphing, Node (Node), deleteNodeAndChildren, edgesList, filter, reachableNodes, shrink, vertexList)

data MavenDependency = MavenDependency
  { dependency :: Dependency
  , dependencyScopes :: Set Text
  }
  deriving (Eq, Ord, Show)

mavenDependencyToDependency :: MavenDependency -> Dependency
mavenDependencyToDependency MavenDependency{..} = dependency

filterMavenSubmodules :: Monad m => Set Text -> Set Text -> Graphing MavenDependency -> m (Graphing MavenDependency)
filterMavenSubmodules targetSet submoduleSet graph = do
  let filteredSubmoduleNames = submoduleSet `Set.difference` targetSet
      filteredSubmoduleNodes = Set.fromList $ Prelude.filter (\dep -> depNameFromMavenDependency dep `Set.member` filteredSubmoduleNames) $ vertexList graph
      -- Find all the dependencies of the submodules that will be included in the scan
      targetSubmoduleDependencies = resultSet targetSet
      -- Find the dependencies of the filtered submodules
      -- After, we want to take the intersection of filteredSubmoduleDependencies && targetSubmoduleDependencies to get all the shared dependencies
      -- Next, take the difference of filteredSubmoduleDependencies && sharedDependencies to not filter out any shared dependencies
      -- We also want to union all the filteredSubmoduleNodes with our previous result to get a fully complete set of nodes to exclude.
      filteredSubmoduleDependencies = resultSet filteredSubmoduleNames
      sharedDependencies = filteredSubmoduleDependencies `Set.intersection` targetSubmoduleDependencies
      mavenDependenciesToRemove = filteredSubmoduleNodes `Set.union` (filteredSubmoduleDependencies `Set.difference` sharedDependencies)

  pure $ Graphing.filter (shouldIncludeMavenDependency mavenDependenciesToRemove) graph
  where
    shouldIncludeMavenDependency :: Set MavenDependency -> MavenDependency -> Bool
    shouldIncludeMavenDependency mavenDepExclusionSet mavenDep = mavenDep `Set.notMember` mavenDepExclusionSet

    edgeList :: [(MavenDependency, MavenDependency)]
    edgeList = edgesList graph

    depNameFromMavenDependency :: MavenDependency -> Text
    depNameFromMavenDependency MavenDependency{..} = dependencyName dependency

    -- findTransitiveDependencies is used to find the all depedencies of a submodule excluding its submodule dependencies
    -- it also does not include the input submodule as part of its set, just strictly its dependencies
    findTransitiveDependencies :: Text -> [(MavenDependency, MavenDependency)] -> Set MavenDependency
    findTransitiveDependencies vertexName edges = do
      let directDependencies = [neighborDep | (sourceDep, neighborDep) <- edges, depNameFromMavenDependency sourceDep == vertexName && (depNameFromMavenDependency neighborDep) `Set.notMember` submoduleSet]
          transitiveDependencies = Set.unions $ map (\dep -> findTransitiveDependencies (depNameFromMavenDependency dep) edges) directDependencies
       in Set.fromList directDependencies `Set.union` transitiveDependencies

    -- given a set of submodules, resultSet combines all the transitive dependencies of every submodule in from the input set
    resultSet :: Set Text -> Set MavenDependency
    resultSet submodules = Set.foldr (\vertexName acc -> (findTransitiveDependencies vertexName edgeList) `Set.union` acc) Set.empty submodules
{-# LANGUAGE RecordWildCards #-}

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
