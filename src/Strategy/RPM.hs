{-# LANGUAGE RecordWildCards #-}

module Strategy.RPM (
  buildGraph,
  discover,
  getSpecDeps,
  getTypeFromLine,
  toDependency,
  RPMDependency (..),
  RequiresType (..),
  Dependencies (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Extra (splitOnceOn)
import DepTypes (
  DepEnvironment,
  DepType (RPMType),
  Dependency (..),
  VerConstraint (CEq, CGreater, CGreaterOrEq, CLess, CLessOrEq),
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  fileName,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS, readContentsText)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Graphing qualified as G
import Path (Abs, Dir, File, Path)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (RpmProjectType),
  GraphBreadth (Partial),
 )

newtype SpecFileLabel
  = RequiresType DepEnvironment
  deriving (Eq, Ord, Show)

data RPMDependency = RPMDependency
  { rpmDepName :: Text
  , rpmConstraint :: Maybe VerConstraint
  }
  deriving (Eq, Ord, Show)

data RequiresType
  = BuildRequires RPMDependency
  | RuntimeRequires RPMDependency
  deriving (Eq, Ord, Show)

data Dependencies = Dependencies
  { depBuildRequires :: [RPMDependency]
  , depRuntimeRequires :: [RPMDependency]
  }
  deriving (Eq, Ord, Show)

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject RpmProject]
discover = simpleDiscover findProjects mkProject RpmProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [RpmProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let specs = filter (\f -> ".spec" `isSuffixOf` fileName f) files

  case specs of
    [] -> pure ([], WalkContinue)
    _ -> pure (RpmProject dir <$> specs, WalkContinue)

data RpmProject = RpmProject
  { rpmDir :: Path Abs Dir
  , rpmFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON RpmProject

instance AnalyzeProject RpmProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

mkProject :: RpmProject -> DiscoveredProject RpmProject
mkProject project =
  DiscoveredProject
    { projectType = RpmProjectType
    , projectBuildTargets = mempty
    , projectPath = rpmDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => RpmProject -> m DependencyResults
getDeps = context "RPM" . context "Static analysis" . analyze . rpmFile

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze specFile = do
  graph <- analyzeSingle specFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [specFile]
      }

analyzeSingle :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyzeSingle file = do
  specFileText <- readContentsText file
  pure . buildGraph $ getSpecDeps specFileText

toDependency :: RPMDependency -> Dependency
toDependency pkg =
  Dependency
    { dependencyType = RPMType
    , dependencyName = rpmDepName pkg
    , dependencyVersion = rpmConstraint pkg
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

buildGraph :: Dependencies -> Graphing Dependency
buildGraph Dependencies{..} = G.gmap toDependency $ G.fromList depRuntimeRequires

buildConstraint :: Text -> Maybe VerConstraint
buildConstraint raw = constraint
  where
    (comparatorStr, rawVersion) = splitOnceOn " " $ Text.strip raw
    version = Text.strip rawVersion
    constraint = case Text.strip comparatorStr of
      "<=" -> Just $ CLessOrEq version
      "<" -> Just $ CLess version
      ">=" -> Just $ CGreaterOrEq version
      ">" -> Just $ CGreater version
      "=" -> Just $ CEq version
      "==" -> Just $ CEq version
      _ -> Nothing

getTypeFromLine :: Text -> Maybe RequiresType
getTypeFromLine line = safeReq
  where
    (header, value) = splitOnceOn ": " line
    (pkgName, rawConstraint) = splitOnceOn " " $ Text.strip value
    --
    isSafeName :: Text -> Bool
    isSafeName name = not $ "%{" `Text.isInfixOf` name
    -- TODO: temporarily ignore names with macros, until we support expansion
    safeReq :: Maybe RequiresType
    safeReq = if isSafeName pkgName then req else Nothing
    --
    req :: Maybe RequiresType
    req = case header of
      "BuildRequires" -> Just . BuildRequires . RPMDependency pkgName $ buildConstraint rawConstraint
      "Requires" -> Just . RuntimeRequires . RPMDependency pkgName $ buildConstraint rawConstraint
      _ -> Nothing

buildDependencies :: [RequiresType] -> Dependencies
buildDependencies = foldr addDep blankDeps
  where
    addDep :: RequiresType -> Dependencies -> Dependencies
    addDep req deps = case req of
      BuildRequires dep -> deps{depBuildRequires = dep : depBuildRequires deps}
      RuntimeRequires dep -> deps{depRuntimeRequires = dep : depRuntimeRequires deps}
    blankDeps = Dependencies [] []

getSpecDeps :: Text -> Dependencies
getSpecDeps = buildDependencies . mapMaybe getTypeFromLine . Text.lines
