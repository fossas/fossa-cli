{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.Nuspec (
  discover,
  findProjects,
  getDeps,
  mkProject,
  buildGraph,
  Nuspec (..),
  Group (..),
  NuGetDependency (..),
  NuspecLicense (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import App.Pathfinder.Types (LicenseAnalyzeProject, licenseAnalyzeProject)
import Control.Applicative (optional)
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Foldable (find)
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text (Text)
import DepTypes (
  DepType (NuGetType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  fileName,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS, readContentsXML)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Graphing qualified
import Parse.XML (
  FromXML (..),
  attr,
  child,
  children,
  content,
  defaultsTo,
 )
import Path (Abs, Dir, File, Path, parent, toFilePath)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (NuspecProjectType),
  GraphBreadth (Partial),
  License (License),
  LicenseResult (LicenseResult),
  LicenseType (..),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject NuspecProject]
discover = simpleDiscover findProjects mkProject NuspecProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [NuspecProject]
findProjects = walkWithFilters' $ \_ _ files -> do
  case find (L.isSuffixOf ".nuspec" . fileName) files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([NuspecProject file], WalkContinue)

newtype NuspecProject = NuspecProject
  { nuspecFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NuspecProject

mkProject :: NuspecProject -> DiscoveredProject NuspecProject
mkProject project =
  DiscoveredProject
    { projectType = NuspecProjectType
    , projectBuildTargets = mempty
    , projectData = project
    , projectPath = parent $ nuspecFile project
    }

instance AnalyzeProject NuspecProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps

instance LicenseAnalyzeProject NuspecProject where
  licenseAnalyzeProject = analyzeLicenses . nuspecFile

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => NuspecProject -> m DependencyResults
getDeps = context "Nuspec" . context "Static analysis" . analyze' . nuspecFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  nuspec <- readContentsXML @Nuspec file
  graph <- context "Building dependency graph" $ pure (buildGraph nuspec)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [file]
      }

analyzeLicenses :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m [LicenseResult]
analyzeLicenses file = do
  nuspec <- readContentsXML @Nuspec file
  pure [LicenseResult (toFilePath file) (nuspecLicenses nuspec)]

nuspecLicenses :: Nuspec -> [License]
nuspecLicenses nuspec = url ++ licenseField
  where
    url = case licenseUrl nuspec of
      Just location -> [License LicenseURL location]
      Nothing -> []
    licenseField = foldr (\a b -> b ++ [License (parseLicenseType $ nuspecLicenseType a) (nuspecLicenseValue a)]) [] (license nuspec)

parseLicenseType :: Text -> LicenseType
parseLicenseType rawType = case toString rawType of
  "expression" -> LicenseSPDX
  "file" -> LicenseFile
  _ -> UnknownType

data Nuspec = Nuspec
  { groups :: [Group]
  , license :: [NuspecLicense]
  , licenseUrl :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data NuspecLicense = NuspecLicense
  { nuspecLicenseType :: Text
  , nuspecLicenseValue :: Text
  }
  deriving (Eq, Ord, Show)

newtype Group = Group
  { dependencies :: [NuGetDependency]
  }
  deriving (Eq, Ord, Show)

data NuGetDependency = NuGetDependency
  { depID :: Text
  , depVersion :: Text
  }
  deriving (Eq, Ord, Show)

instance FromXML Nuspec where
  parseElement el = do
    metadata <- child "metadata" el
    Nuspec
      <$> optional (child "dependencies" metadata >>= children "group")
      `defaultsTo` []
      <*> children "license" metadata
      <*> optional (child "licenseUrl" metadata)

instance FromXML NuspecLicense where
  parseElement el =
    NuspecLicense
      <$> optional (attr "type" el)
      `defaultsTo` ""
      <*> content el

instance FromXML Group where
  parseElement el = Group <$> children "dependency" el

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency
      <$> attr "id" el
      <*> attr "version" el

buildGraph :: Nuspec -> Graphing Dependency
buildGraph project = Graphing.fromList (map toDependency direct)
  where
    direct = concatMap dependencies (groups project)
    toDependency NuGetDependency{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = depID
        , dependencyVersion = Just (CEq depVersion)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
