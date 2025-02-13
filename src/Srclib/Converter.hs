{-# LANGUAGE RecordWildCards #-}

module Srclib.Converter (
  projectToSourceUnit,
  toSourceUnit,
  depTypeToFetcher,
  fetcherToDepType,
  verConstraintToRevision,
  toLocator,
  shouldPublishDep,
) where

import Prelude

import Algebra.Graph.AdjacencyMap qualified as AM
import App.Fossa.Analyze.Project (ProjectResult (..))
import Control.Applicative ((<|>))
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvOther, EnvProduction),
  DepType (..),
  Dependency (
    Dependency,
    dependencyEnvironments,
    dependencyName,
    dependencyType,
    dependencyVersion
  ),
  VerConstraint (..),
 )
import Graphing (Graphing)
import Graphing qualified
import Path (toFilePath)
import Srclib.Types (
  Locator (..),
  OriginPath,
  SourceUnit (..),
  SourceUnitBuild (
    SourceUnitBuild,
    buildArtifact,
    buildDependencies,
    buildImports,
    buildSucceeded
  ),
  SourceUnitDependency (..),
  somePathToOriginPath,
 )
import Types (DiscoveredProjectType, GraphBreadth (..))

projectToSourceUnit :: Bool -> ProjectResult -> SourceUnit
projectToSourceUnit leaveUnfiltered ProjectResult{..} =
  toSourceUnit leaveUnfiltered renderedPath projectResultGraph projectResultType projectResultGraphBreadth originPaths
  where
    renderedPath = toText (toFilePath projectResultPath)
    originPaths = map somePathToOriginPath projectResultManifestFiles

toSourceUnit :: Bool -> Text -> Graphing Dependency -> DiscoveredProjectType -> GraphBreadth -> [OriginPath] -> SourceUnit
toSourceUnit leaveUnfiltered path dependencies projectType graphBreadth originPaths =
  SourceUnit
    { sourceUnitName = path
    , sourceUnitType = toText projectType
    , sourceUnitManifest = path
    , sourceUnitBuild =
        Just $
          SourceUnitBuild
            { buildArtifact = "default"
            , buildSucceeded = True
            , buildImports = imports
            , buildDependencies = deps
            }
    , sourceUnitGraphBreadth = graphBreadth
    , sourceUnitNoticeFiles = []
    , sourceUnitOriginPaths = originPaths
    , additionalData = Nothing
    , sourceUnitLabels = Nothing
    }
  where
    filteredGraph :: Graphing Dependency
    filteredGraph = Graphing.shrinkWithoutPromotionToDirect ff dependencies
      where
        ff =
          if leaveUnfiltered
            then isSupportedType
            else (\d -> shouldPublishDep d && isSupportedType d)

    locatorGraph :: Graphing Locator
    locatorGraph = Graphing.gmap toLocator filteredGraph

    locatorAdjacent :: AM.AdjacencyMap Locator
    locatorAdjacent = Graphing.toAdjacencyMap locatorGraph

    deps :: [SourceUnitDependency]
    deps = map (mkSourceUnitDependency locatorAdjacent) (AM.vertexList locatorAdjacent)

    imports :: [Locator]
    imports = Graphing.directList locatorGraph

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

-- core can't handle subprojects, conantype or path types
isSupportedType :: Dependency -> Bool
isSupportedType Dependency{dependencyType} =
  dependencyType /= SubprojectType
    && dependencyType /= GooglesourceType
    && dependencyType /= ConanType
    && dependencyType /= UnresolvedPathType

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
  BowerType -> "bower"
  CarthageType -> "cart"
  CargoType -> "cargo"
  ComposerType -> "comp"
  ConanType -> "conan"
  CondaType -> "conda"
  CpanType -> "cpan"
  CranType -> "cran"
  CustomType -> "custom"
  GemType -> "gem"
  GitType -> "git"
  GooglesourceType -> "git" -- FIXME. Yet another thing that's coming back to bite us
  GoType -> "go"
  HackageType -> "hackage"
  HexType -> "hex"
  LinuxAPK -> "apk"
  LinuxDEB -> "deb"
  LinuxRPM -> "rpm-generic"
  MavenType -> "mvn"
  NodeJSType -> "npm"
  NuGetType -> "nuget"
  PipType -> "pip"
  PodType -> "pod"
  RPMType -> "rpm"
  SubprojectType -> "mvn" -- FIXME. I knew SubprojectType would come back to bite us.
  URLType -> "url"
  UserType -> "user"
  PubType -> "pub"
  SwiftType -> "swift"
  UnresolvedPathType -> "upath"
  PathType -> "path"

-- | GooglesourceType and SubprojectType are not supported with this function, since they're ambiguous.
fetcherToDepType :: Text -> Maybe DepType
fetcherToDepType fetcher | depTypeToFetcher ArchiveType == fetcher = Just ArchiveType
fetcherToDepType fetcher | depTypeToFetcher BowerType == fetcher = Just BowerType
fetcherToDepType fetcher | depTypeToFetcher CarthageType == fetcher = Just CarthageType
fetcherToDepType fetcher | depTypeToFetcher CargoType == fetcher = Just CargoType
fetcherToDepType fetcher | depTypeToFetcher ComposerType == fetcher = Just ComposerType
fetcherToDepType fetcher | depTypeToFetcher CondaType == fetcher = Just CondaType
fetcherToDepType fetcher | depTypeToFetcher CpanType == fetcher = Just CpanType
fetcherToDepType fetcher | depTypeToFetcher CranType == fetcher = Just CranType
fetcherToDepType fetcher | depTypeToFetcher CustomType == fetcher = Just CustomType
fetcherToDepType fetcher | depTypeToFetcher GemType == fetcher = Just GemType
fetcherToDepType fetcher | depTypeToFetcher GitType == fetcher = Just GitType
fetcherToDepType fetcher | depTypeToFetcher GoType == fetcher = Just GoType
fetcherToDepType fetcher | depTypeToFetcher HackageType == fetcher = Just HackageType
fetcherToDepType fetcher | depTypeToFetcher HexType == fetcher = Just HexType
fetcherToDepType fetcher | depTypeToFetcher LinuxAPK == fetcher = Just LinuxAPK
fetcherToDepType fetcher | depTypeToFetcher LinuxDEB == fetcher = Just LinuxDEB
fetcherToDepType fetcher | depTypeToFetcher LinuxRPM == fetcher = Just LinuxRPM
fetcherToDepType fetcher | depTypeToFetcher MavenType == fetcher = Just MavenType
fetcherToDepType fetcher | depTypeToFetcher NodeJSType == fetcher = Just NodeJSType
fetcherToDepType fetcher | depTypeToFetcher NuGetType == fetcher = Just NuGetType
fetcherToDepType fetcher | depTypeToFetcher PipType == fetcher = Just PipType
fetcherToDepType fetcher | depTypeToFetcher PodType == fetcher = Just PodType
fetcherToDepType fetcher | depTypeToFetcher RPMType == fetcher = Just RPMType
fetcherToDepType fetcher | depTypeToFetcher URLType == fetcher = Just URLType
fetcherToDepType fetcher | depTypeToFetcher UserType == fetcher = Just UserType
fetcherToDepType fetcher | depTypeToFetcher PubType == fetcher = Just PubType
fetcherToDepType fetcher | depTypeToFetcher PathType == fetcher = Just PathType
fetcherToDepType _ = Nothing
