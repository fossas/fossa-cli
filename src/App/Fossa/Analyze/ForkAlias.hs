{-# LANGUAGE OverloadedRecordDot #-}

module App.Fossa.Analyze.ForkAlias (
  -- * Fork alias map and labels
  mkForkAliasMap,
  collectForkAliasLabels,
  mergeForkAliasLabels,

  -- * Translation
  translateLocatorWithForkAliases,
  translateDependencyGraph,
  translateDependency,

  -- * Project building
  buildProject,
) where

import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.ManualDeps (
  ForkAlias (..),
  ForkAliasEntry (..),
  forkAliasEntryToLocator,
 )
import App.Fossa.Reachability.Upload (dependenciesOf)
import Control.Applicative ((<|>))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (Dependency (..), VerConstraint (CEq))
import Graphing (Graphing)
import Graphing qualified
import Srclib.Converter (toLocator)
import Srclib.Types (
  Locator (..),
  ProvidedPackageLabel,
  ProvidedPackageLabels (..),
  SourceUnit (..),
  buildProvidedPackageLabels,
  renderLocator,
  toProjectLocator,
 )

-- | Create a fork alias map from a list of fork aliases.
-- The map is keyed by project locator (type+name, no version) to allow lookup by type+name.
-- The value is the full ForkAlias to check version matching and get base translation info.
mkForkAliasMap :: [ForkAlias] -> Map.Map Locator ForkAlias
mkForkAliasMap = Map.fromList . map (\alias -> (toProjectLocator (forkAliasEntryToLocator alias.forkAliasFork), alias))

-- | Collect labels from fork aliases into a map keyed by locator string.
-- Labels are keyed by project locator (without version) so they match any version.
collectForkAliasLabels :: [ForkAlias] -> Map.Map Text [ProvidedPackageLabel]
collectForkAliasLabels = Map.fromListWith (++) . mapMaybe forkAliasToLabel
  where
    forkAliasToLabel :: ForkAlias -> Maybe (Text, [ProvidedPackageLabel])
    forkAliasToLabel forkAlias =
      -- Use project locator (without version) so labels match any version of the translated dependency
      let baseLocator = forkAliasEntryToLocator forkAlias.forkAliasBase
          projectLocator = toProjectLocator baseLocator
          labels = forkAlias.forkAliasLabels
       in if null labels
            then Nothing
            else Just (renderLocator projectLocator, labels)

-- | Merge fork alias labels into a source unit's existing labels.
-- Only applies labels for dependencies that actually exist in the source unit.
mergeForkAliasLabels :: Map.Map Text [ProvidedPackageLabel] -> SourceUnit -> SourceUnit
mergeForkAliasLabels forkAliasLabels unit
  | Map.null forkAliasLabels = unit
  | otherwise =
      let -- Get all project locators (without version) from this source unit
          unitProjectLocators = Set.fromList $ map (renderLocator . toProjectLocator) $ dependenciesOf unit
          -- Only include labels for dependencies that exist in this source unit
          matchingLabels = Map.filterWithKey (\locatorStr _ -> Set.member locatorStr unitProjectLocators) forkAliasLabels
       in if Map.null matchingLabels
            then unit
            else
              unit
                { sourceUnitLabels =
                    buildProvidedPackageLabels $
                      Map.unionWith (++) matchingLabels $
                        maybe Map.empty unProvidedPackageLabels (sourceUnitLabels unit)
                }

-- | Translate a locator using fork aliases
-- If the fork locator exists in the list of translations, then translate the fork locator to the base locator
-- The translated fetcher type and project name will be the fetcher type and project name of the base locator.
-- Versions are a bit more complex.
-- Versions are not required for either base or the fork.
-- version matching rules:
--   - If the fork version is specified, only that exact version matches
--   - If the fork version is not specified, any fork version matches
-- Translation rules:
--   - If the base version is specified, always convert to that version
--   - If the base version is not specified, preserve the original version from the fork
translateLocatorWithForkAliases :: Map.Map Locator ForkAlias -> Locator -> Locator
translateLocatorWithForkAliases forkAliasMap loc =
  let projectLocator = toProjectLocator loc
   in case Map.lookup projectLocator forkAliasMap of
        Nothing -> loc
        Just ForkAlias{forkAliasFork = fork, forkAliasBase = base} ->
          -- Check if version matches (if fork version is specified)
          let versionMatches =
                case (forkAliasEntryVersion fork, locatorRevision loc) of
                  (Nothing, _) -> True -- No version specified in fork, match any version
                  (Just forkVersion, Just locVersion) -> forkVersion == locVersion
                  (Just _, Nothing) -> False -- Fork specifies version but loc has none
           in if versionMatches
                then
                  let baseLocator = forkAliasEntryToLocator base
                      -- Use base version if specified, otherwise preserve original
                      finalVersion = forkAliasEntryVersion base <|> locatorRevision loc
                   in baseLocator{locatorRevision = finalVersion}
                else loc

-- | Translate dependencies in a graph using fork aliases.
translateDependencyGraph :: Map.Map Locator ForkAlias -> Graphing Dependency -> Graphing Dependency
translateDependencyGraph forkAliasMap = Graphing.gmap (translateDependency forkAliasMap)

-- | Translate a single dependency using fork aliases.
translateDependency :: Map.Map Locator ForkAlias -> Dependency -> Dependency
translateDependency forkAliasMap dep =
  let depLocator = toLocator dep
      translatedLocator = translateLocatorWithForkAliases forkAliasMap depLocator
   in if translatedLocator == depLocator
        then dep -- No translation occurred
        else
          -- Translation occurred, extract base info from fork alias
          let projectLocator = toProjectLocator depLocator
           in case Map.lookup projectLocator forkAliasMap of
                Just ForkAlias{forkAliasBase = base} ->
                  let baseDepType = forkAliasEntryType base
                      baseName = forkAliasEntryName base
                      -- Use base version if specified, otherwise preserve original
                      finalVersion = forkAliasEntryVersion base <|> locatorRevision depLocator
                   in dep
                        { dependencyType = baseDepType
                        , dependencyName = baseName
                        , dependencyVersion = finalVersion >>= Just . CEq
                        }
                Nothing -> dep -- Should not happen since translation occurred, but handle safely

-- | Build a project JSON object with fork alias translation applied to the graph.
buildProject :: Map.Map Locator ForkAlias -> ProjectResult -> Aeson.Value
buildProject forkAliasMap project =
  Aeson.object
    [ "path" .= projectResultPath project
    , "type" .= projectResultType project
    , "graph" .= graphingToGraph (translateDependencyGraph forkAliasMap (projectResultGraph project))
    ]
