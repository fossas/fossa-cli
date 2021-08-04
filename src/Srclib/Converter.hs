{-# LANGUAGE RecordWildCards #-}

module Srclib.Converter (
  toSourceUnit,
  depTypeToFetcher,
) where

import Prelude

import Algebra.Graph.AdjacencyMap qualified as AM
import App.Fossa.Analyze.Project
import Control.Applicative ((<|>))
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes
import Graphing (Graphing)
import Graphing qualified
import Path (toFilePath)
import Srclib.Types

toSourceUnit :: ProjectResult -> SourceUnit
toSourceUnit ProjectResult{..} =
  SourceUnit
    { sourceUnitName = renderedPath
    , sourceUnitType = projectResultType
    , sourceUnitManifest = renderedPath
    , sourceUnitBuild =
        Just $
          SourceUnitBuild
            { buildArtifact = "default"
            , buildSucceeded = True
            , buildImports = imports
            , buildDependencies = deps
            }
    , sourceUnitGraphBreadth = projectResultGraphBreadth
    , additionalData = Nothing
    }
  where
    renderedPath = toText (toFilePath projectResultPath)

    filteredGraph :: Graphing Dependency
    filteredGraph = Graphing.filter (\d -> shouldPublishDep d && isSupportedType d) projectResultGraph

    locatorGraph :: Graphing Locator
    locatorGraph = Graphing.gmap toLocator filteredGraph

    locatorAdjacent :: AM.AdjacencyMap Locator
    locatorAdjacent = Graphing.graphingAdjacent locatorGraph

    deps :: [SourceUnitDependency]
    deps = map (mkSourceUnitDependency locatorAdjacent) (AM.vertexList locatorAdjacent)

    imports :: [Locator]
    imports = Set.toList $ Graphing.graphingDirect locatorGraph

mkSourceUnitDependency :: AM.AdjacencyMap Locator -> Locator -> SourceUnitDependency
mkSourceUnitDependency gr locator =
  SourceUnitDependency
    { sourceDepLocator = locator
    , sourceDepImports = Set.toList $ AM.postSet locator gr
    }

shouldPublishDep :: Dependency -> Bool
shouldPublishDep Dependency{dependencyEnvironments} =
  null dependencyEnvironments || EnvProduction `elem` dependencyEnvironments || any isOtherEnv dependencyEnvironments

isOtherEnv :: DepEnvironment -> Bool
isOtherEnv (EnvOther _) = True
isOtherEnv _ = False

-- core can't handle subprojects
isSupportedType :: Dependency -> Bool
isSupportedType Dependency{dependencyType} = dependencyType /= SubprojectType && dependencyType /= GooglesourceType

toLocator :: Dependency -> Locator
toLocator dep =
  Locator
    { locatorFetcher = depTypeToFetcher (dependencyType dep)
    , locatorProject = dependencyName dep
    , locatorRevision = verConstraintToRevision =<< dependencyVersion dep
    }

verConstraintToRevision :: VerConstraint -> Maybe Text
verConstraintToRevision = \case
  CEq ver -> Just ver
  CURI _ -> Nothing -- we can't represent this in a locator
  CCompatible ver -> Just ver
  CAnd a b -> verConstraintToRevision a <|> verConstraintToRevision b
  COr a b -> verConstraintToRevision a <|> verConstraintToRevision b
  CLess ver -> Just ver -- ugh
  CLessOrEq ver -> Just ver -- ugh
  CGreater ver -> Just ver -- ugh
  CGreaterOrEq ver -> Just ver -- ugh
  CNot _ -> Nothing -- we can't represent this in a locator

depTypeToFetcher :: DepType -> Text
depTypeToFetcher = \case
  ArchiveType -> "archive"
  CarthageType -> "cart"
  CargoType -> "cargo"
  ComposerType -> "comp"
  CondaType -> "conda"
  CpanType -> "cpan"
  GemType -> "gem"
  GitType -> "git"
  GooglesourceType -> "git" -- FIXME. Yet another thing that's coming back to bite us
  GoType -> "go"
  HackageType -> "hackage"
  HexType -> "hex"
  MavenType -> "mvn"
  NodeJSType -> "npm"
  NuGetType -> "nuget"
  PipType -> "pip"
  PodType -> "pod"
  RPMType -> "rpm"
  SubprojectType -> "mvn" -- FIXME. I knew SubprojectType would come back to bite us.
  URLType -> "url"
  UserType -> "user"
