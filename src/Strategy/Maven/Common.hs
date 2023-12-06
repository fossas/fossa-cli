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

import Graphing (Graphing, edgesList, filter, shrink, vertexList)

data MavenDependency = MavenDependency
  { dependency :: Dependency
  , dependencyScopes :: Set Text
  }
  deriving (Eq, Ord, Show)

mavenDependencyToDependency :: MavenDependency -> Dependency
mavenDependencyToDependency MavenDependency{..} = dependency

filterMavenSubmodules :: Set Text -> Set Text -> Graphing MavenDependency -> (Graphing MavenDependency)
filterMavenSubmodules includedSubmoduleSet completeSubmoduleSet graph = do
  let excludedSubmoduleSet = completeSubmoduleSet `Set.difference` includedSubmoduleSet
      excludedSubmodules = Set.fromList $ Prelude.filter (\dep -> depNameFromMavenDependency dep `Set.member` excludedSubmoduleSet) $ vertexList graph
      -- Find all the dependencies of the submodules that will be included in the scan
      includedSubmoduleDependencies = combinedMavenDependencySet includedSubmoduleSet
      -- Find the dependencies of the excluded submodules
      -- After, we want to take the intersection of excludedSubmoduleDependencies && includedSubmoduleDependencies to get all the shared dependencies
      -- Next, take the difference of excludedSubmoduleDependencies && sharedDependencies so we don't filter out any shared dependencies
      -- We also want to union all the excludedSubmodules with our previous result to add the submodules back in order to get a complete set of nodes to exclude.
      excludedSubmoduleDependencies = combinedMavenDependencySet excludedSubmoduleSet
      sharedDependencies = excludedSubmoduleDependencies `Set.intersection` includedSubmoduleDependencies
      mavenDependenciesToRemove = excludedSubmodules `Set.union` (excludedSubmoduleDependencies `Set.difference` sharedDependencies)

  Graphing.filter (shouldIncludeMavenDependency mavenDependenciesToRemove) graph
  where
    shouldIncludeMavenDependency :: Set MavenDependency -> MavenDependency -> Bool
    shouldIncludeMavenDependency mavenDepExclusionSet mavenDep = mavenDep `Set.notMember` mavenDepExclusionSet

    edgeList :: [(MavenDependency, MavenDependency)]
    edgeList = edgesList graph

    depNameFromMavenDependency :: MavenDependency -> Text
    depNameFromMavenDependency MavenDependency{..} = dependencyName dependency

    -- Used to find the all depedencies of a submodule (excluding dependencies that are submodules)
    -- It also does not include the input submodule as part of its set, just strictly its dependencies
    findTransitiveDependencies :: Text -> [(MavenDependency, MavenDependency)] -> Set MavenDependency
    findTransitiveDependencies vertexName edges = do
      let directDependencies = [neighborDep | (sourceDep, neighborDep) <- edges, depNameFromMavenDependency sourceDep == vertexName && (depNameFromMavenDependency neighborDep) `Set.notMember` completeSubmoduleSet]
          transitiveDependencies = Set.unions $ map (\dep -> findTransitiveDependencies (depNameFromMavenDependency dep) edges) directDependencies
       in Set.fromList directDependencies `Set.union` transitiveDependencies

    -- Given a set of submodule names, combine all the transitive dependencies of every submodule from the input set
    combinedMavenDependencySet :: Set Text -> Set MavenDependency
    combinedMavenDependencySet = Set.foldr (\vertexName acc -> (findTransitiveDependencies vertexName edgeList) `Set.union` acc) Set.empty

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
