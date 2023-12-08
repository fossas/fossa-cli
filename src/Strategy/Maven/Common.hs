{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.Common (
  MavenDependency (..),
  mavenDependencyToDependency,
  filterMavenSubmodules,
  filterMavenDependencyByScope,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, foldl', foldr')
import DepTypes (Dependency (..))

import Discovery.Filters (FilterSet (scopes), MavenScopeFilters (..))

import Control.Monad (forM_)
import Data.Foldable (fold, for_)
import Data.Traversable (for)

import Data.Set (toList)
import Debug.Trace (traceM)
import Graphing (Graphing, color, edgesList, gmap, reachableNodesOnCondition, vertexList)
import Graphing qualified

-- import Effect.Grapher (direct, edge, evalGrapher)
-- import Control.Algebra (Has, run)

data MavenDependency = MavenDependency
  { dependency :: Dependency
  , dependencyScopes :: Set Text
  , -- The submodules that this dependency belongs to
    dependencySubmodules :: Set Text
  }
  deriving (Eq, Ord, Show)

mavenDependencyToDependency :: MavenDependency -> Dependency
mavenDependencyToDependency MavenDependency{..} = dependency

filterMavenSubmodules :: Monad m => Set Text -> Set Text -> Graphing MavenDependency -> m (Graphing MavenDependency)
filterMavenSubmodules includedSubmoduleSet completeSubmoduleSet graph = do
  -- let excludedSubmoduleSet = completeSubmoduleSet `Set.difference` includedSubmoduleSet
  --     excludedSubmodules = Set.fromList $ filter (\dep -> depNameFromMavenDependency dep `Set.member` excludedSubmoduleSet) $ vertexList graph
  --     -- Find all the dependencies of the submodules that will be included in the scan
  --     includedSubmoduleDependencies = combinedMavenDependencySet includedSubmoduleSet
  --     -- Find the dependencies of the excluded submodules
  --     -- After, we want to take the intersection of excludedSubmoduleDependencies && includedSubmoduleDependencies to get all the shared dependencies
  --     -- Next, take the difference of excludedSubmoduleDependencies && sharedDependencies so we don't filter out any shared dependencies
  --     -- We also want to union all the excludedSubmodules with our previous result to add the submodules back in order to get a complete set of nodes to exclude.
  --     excludedSubmoduleDependencies = combinedMavenDependencySet excludedSubmoduleSet
  --     sharedDependencies = excludedSubmoduleDependencies `Set.intersection` includedSubmoduleDependencies
  --     mavenDependenciesToRemove = excludedSubmodules `Set.union` (excludedSubmoduleDependencies `Set.difference` sharedDependencies)

  let submoduleNodes = Set.fromList $ filter (\dep -> depNameFromMavenDependency dep `Set.member` completeSubmoduleSet) $ vertexList graph
      -- includedSubmoduleNodes = Set.fromList $ filter (\dep -> depNameFromMavenDependency dep `Set.member` includedSubmoduleSet) $ vertexList graph
      colored = coloredGraph submoduleNodes graph
  traceM ("colored graph ************** " ++ show (colored))
  pure $ Graphing.filter shouldIncludeMavenDependency colored
  where
    shouldIncludeMavenDependency :: MavenDependency -> Bool
    shouldIncludeMavenDependency MavenDependency{..} = not (null (dependencySubmodules `Set.difference` includedSubmoduleSet))

    notSubmodule :: Text -> Set Text -> Bool
    notSubmodule depName submodules = depName `Set.notMember` submodules

    edgeList :: [(Text, Text)]
    edgeList = mapToDependencyNames $ edgesList graph

    mapToDependencyNames :: [(MavenDependency, MavenDependency)] -> [(Text, Text)]
    mapToDependencyNames = concatMap (\(dep1, dep2) -> [(depNameFromMavenDependency dep1, depNameFromMavenDependency dep2)])

    depNameFromMavenDependency :: MavenDependency -> Text
    depNameFromMavenDependency = dependencyName . dependency

    reachableNodesFromSubmodule :: Text -> Set Text
    reachableNodesFromSubmodule mavenDep = do
      let children = reachableNodesOnCondition edgeList notSubmodule mavenDep completeSubmoduleSet
      children `Set.union` Set.fromList [mavenDep]

    coloredGraph :: Set MavenDependency -> Graphing MavenDependency -> Graphing MavenDependency
    coloredGraph submodules g =
      foldr (\submodule acc -> color acc dependencySubmodules (\x dep -> dep{dependencySubmodules = x}) submodule depNameFromMavenDependency (reachableNodesFromSubmodule $ depNameFromMavenDependency submodule)) g submodules

-- reachableNodesFromSubmodule :: MavenDependency -> Set MavenDependency
-- reachableNodesFromSubmodule mavenDep = do
--   let children = reachableNodesOnCondition edgeList depNameFromMavenDependency notSubmodule (mavenDep) completeSubmoduleSet
--   children `Set.union` Set.fromList [depNameFromMavenDependency mavenDep]

-- coloredGraph :: Set MavenDependency -> Graphing MavenDependency -> Graphing MavenDependency
-- coloredGraph submodules g =
--   foldr (\submodule acc -> color acc dependencySubmodules (\x dep -> dep{dependencySubmodules = x}) (reachableNodesFromSubmodule submodule) submodule) g submodules

-- coloredGraph :: Set MavenDependency -> Graphing MavenDependency -> Graphing MavenDependency
-- coloredGraph submodules g = foldl' applyColor g (toList submodules)
--   where
--     applyColor acc submodule = color acc dependencySubmodules (\x dep -> dep{dependencySubmodules = x}) (reachableNodesFromSubmodule submodule) submodule

-- Used to find the all dependencies of a submodule (excluding dependencies that are submodules)
-- It also does not include the input submodule as part of its set, just strictly its dependencies
-- findTransitiveDependencies :: Text -> [(MavenDependency, MavenDependency)] -> Set MavenDependency
-- findTransitiveDependencies vertexName edges = do
--   let directDependencies = [neighborDep | (sourceDep, neighborDep) <- edges, depNameFromMavenDependency sourceDep == vertexName && (depNameFromMavenDependency neighborDep) `Set.notMember` completeSubmoduleSet]
--       transitiveDependencies = Set.unions $ map (\dep -> findTransitiveDependencies (depNameFromMavenDependency dep) edges) directDependencies
--    in Set.fromList directDependencies `Set.union` transitiveDependencies

-- -- Given a set of submodule names, combine all the transitive dependencies of every submodule from the input set
-- combinedMavenDependencySet :: Set Text -> Set MavenDependency
-- combinedMavenDependencySet = Set.foldr (\vertexName acc -> (findTransitiveDependencies vertexName edgeList) `Set.union` acc) Set.empty

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
