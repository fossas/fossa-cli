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
