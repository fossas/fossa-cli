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
import Graphing (Graphing, shrink)

data MavenDependency = MavenDependency
  { dependency :: Dependency
  , dependencyScopes :: Set Text
  }
  deriving (Eq, Ord, Show)

mavenDependencyToDependency :: MavenDependency -> Dependency
mavenDependencyToDependency MavenDependency{..} = dependency

filterMavenDependencyByScope :: Set Text -> Set Text -> Graphing MavenDependency -> Graphing MavenDependency
filterMavenDependencyByScope scopeIncludeSet scopeExcludeSet = Graphing.shrink isMavenDependencyIncluded
  where
    isMavenDependencyIncluded :: MavenDependency -> Bool
    isMavenDependencyIncluded MavenDependency{..} = case (Set.null dependencyScopes, Set.null scopeIncludeSet, Set.null scopeExcludeSet) of
      (False, True, True) -> True
      (False, False, True) -> dependencyScopes `Set.isSubsetOf` scopeIncludeSet
      (False, True, False) -> dependencyScopes `Set.disjoint` scopeExcludeSet
      (False, False, False) -> not (Set.null includedIntersectSet) && (includedIntersectSet `Set.disjoint` scopeExcludeSet)
        where
          includedIntersectSet = dependencyScopes `Set.intersection` scopeIncludeSet
      (True, _, _) -> True
