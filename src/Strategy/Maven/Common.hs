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
      let children = reachableSuccessorsWithCondition edgeList mavenDep notSubmodule completeSubmoduleSet
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
