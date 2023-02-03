module Strategy.Composer (
  discover,
  buildGraph,
  ComposerLock (..),
  CompDep (..),
  ComposerProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  run,
 )
import Control.Effect.Reader (Reader)
import Data.Aeson.Types (
  FromJSON (parseJSON),
  ToJSON,
  Value (Array, String),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (ComposerType),
  Dependency (..),
  VerConstraint (CEq),
  insertEnvironment,
 )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Grapher (
  LabeledGrapher,
  direct,
  edge,
  label,
  withLabeling,
 )
import Effect.ReadFS (ReadFS, readContentsJson)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, toFilePath)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (..),
  GraphBreadth (Complete),
  License (License),
  LicenseResult (LicenseResult),
  LicenseType (LicenseSPDX),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject ComposerProject]
discover = simpleDiscover findProjects mkProject ComposerProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [ComposerProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "composer.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just lock -> do
      let project =
            ComposerProject
              { composerDir = dir
              , composerLock = lock
              , composerJson = findFileNamed "composer.json" files
              }

      pure ([project], WalkContinue)

mkProject :: ComposerProject -> DiscoveredProject ComposerProject
mkProject project =
  DiscoveredProject
    { projectType = ComposerProjectType
    , projectBuildTargets = mempty
    , projectPath = composerDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => ComposerProject -> m DependencyResults
getDeps project = context "Composer" $ do
  lock <- readContentsJson @ComposerLock (composerLock project)
  graph <- context "Building dependency graph" $ pure (buildGraph lock)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [composerLock project]
      }

data ComposerProject = ComposerProject
  { composerDir :: Path Abs Dir
  , composerLock :: Path Abs File
  , composerJson :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ComposerProject

instance AnalyzeProject ComposerProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps

instance LicenseAnalyzeProject ComposerProject where
  licenseAnalyzeProject = analyzeLicenses . composerJson

analyzeLicenses :: (Has ReadFS sig m, Has Diagnostics sig m) => Maybe (Path Abs File) -> m [LicenseResult]
analyzeLicenses Nothing = pure []
analyzeLicenses (Just licenseFileName) = do
  let mkLicenseResults l = [LicenseResult (toFilePath licenseFileName) l]
      textToLicense = License LicenseSPDX
  contents <- readContentsJson licenseFileName
  pure . mkLicenseResults . map textToLicense . filter (not . Text.null) . unLicense . license $ contents

data ComposerLock = ComposerLock
  { lockPackages :: [CompDep]
  , lockPackagesDev :: [CompDep]
  }
  deriving (Eq, Ord, Show)

data CompDep = CompDep
  { depName :: Text
  , depVersion :: Text
  , -- | name to version spec
    depRequire :: Maybe (Map Text Text)
  , depRequireDev :: Maybe (Map Text Text)
  }
  deriving (Eq, Ord, Show)

instance FromJSON ComposerLock where
  parseJSON = withObject "ComposerLock" $ \obj ->
    ComposerLock
      <$> obj .: "packages"
      <*> obj .: "packages-dev"

instance FromJSON CompDep where
  parseJSON = withObject "CompDep" $ \obj ->
    CompDep
      <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .:? "require"
      <*> obj .:? "require-dev"

newtype ComposerLicenses = ComposerLicenses {unLicense :: [Text]}
  deriving (Eq, Ord, Show)

instance FromJSON ComposerLicenses where
  parseJSON (Array t) = ComposerLicenses <$> parseJSON (Array t)
  parseJSON (String t) = pure $ ComposerLicenses [t]
  parseJSON _ = fail "Invalid schema for key 'license' in composer.json"

newtype ComposerJson = ComposerJson
  { license :: ComposerLicenses
  }
  deriving (Eq, Ord, Show)

-- | composer.json and its license key is documented
-- [here](https://getcomposer.org/doc/04-schema.md#license)
instance FromJSON ComposerJson where
  parseJSON = withObject "ComposerJson" $ \obj ->
    ComposerJson <$> obj .:? "license" .!= ComposerLicenses []

newtype CompPkg = CompPkg {pkgName :: Text}
  deriving (Eq, Ord, Show)

type CompGrapher = LabeledGrapher CompPkg CompLabel

data CompLabel
  = DepVersion Text
  | CompEnv DepEnvironment
  deriving (Eq, Ord, Show)

buildGraph :: ComposerLock -> Graphing Dependency
buildGraph lock = run . withLabeling toDependency $ do
  traverse_ (addDeps EnvProduction) $ lockPackages lock
  traverse_ (addDeps EnvDevelopment) $ lockPackagesDev lock
  where
    addDeps :: Has CompGrapher sig m => DepEnvironment -> CompDep -> m ()
    addDeps env dep = do
      let pkg = CompPkg (depName dep)
      _ <- Map.traverseWithKey (addEdge pkg) (fromMaybe Map.empty $ depRequire dep)
      label pkg (DepVersion $ depVersion dep)
      label pkg (CompEnv env)
      direct pkg

    addEdge :: Has CompGrapher sig m => CompPkg -> Text -> Text -> m ()
    addEdge pkg name _ = edge pkg (CompPkg name)

    toDependency :: CompPkg -> Set CompLabel -> Dependency
    toDependency pkg =
      foldr addLabel $
        Dependency
          { dependencyType = ComposerType
          , dependencyName = pkgName pkg
          , dependencyVersion = Nothing
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }

    addLabel :: CompLabel -> Dependency -> Dependency
    addLabel (DepVersion ver) dep = dep{dependencyVersion = Just (CEq ver)}
    addLabel (CompEnv env) dep = insertEnvironment env dep
