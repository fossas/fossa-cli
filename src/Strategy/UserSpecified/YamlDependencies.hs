{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.UserSpecified.YamlDependencies
  ( analyze,
    discover,
    buildGraph,
    UserDependencies (..),
  )
where

import Control.Effect.Diagnostics
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as M
import Data.Text (Text, unpack)
import DepTypes
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Path
import Types

discover :: (Has ReadFS sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = map mkProject <$> findProjects dir

-- Only search for fossa-deps.yaml in the root directory. We can extend this to subdirectories in the future.
findProjects :: (Has ReadFS sig m) => Path Abs Dir -> m [UserDependenciesYamlProject]
findProjects dir = do
  let file = dir </> $(mkRelFile "fossa-deps.yml")
  exists <- doesFileExist file
  if exists
    then pure [UserDependenciesYamlProject file dir]
    else pure []

data UserDependenciesYamlProject = UserDependenciesYamlProject
  { dependenciesFile :: Path Abs File,
    dependenciesDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => UserDependenciesYamlProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "user-specified-yaml",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = dependenciesDir project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => UserDependenciesYamlProject -> m (Graphing Dependency)
getDeps = analyze . dependenciesFile

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze file = buildGraph <$> readContentsYaml @UserDependencies file

buildGraph :: UserDependencies -> Graphing Dependency
buildGraph lockfile = Graphing.fromList (map toDependency direct)
  where
    direct = dependencies lockfile
    toDependency UserDependency {..} =
      Dependency
        { dependencyType = depType,
          dependencyName = depPackage,
          dependencyVersion = CEq <$> depVersion,
          dependencyLocations = [],
          dependencyEnvironments = [],
          dependencyTags = M.empty
        }

data UserDependencies = UserDependencies
  { dependencies :: [UserDependency]
  }
  deriving (Eq, Ord, Show)

data UserDependency = UserDependency
  { depPackage :: Text,
    depType :: DepType,
    depVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON UserDependencies where
  parseJSON = withObject "Dependencies" $ \obj ->
    UserDependencies <$> obj .:? "dependencies" .!= []

depTypeParser :: Text -> Parser DepType
depTypeParser text = case depTypeFromText text of
  Just t -> pure t
  Nothing -> fail $ "dep type: " <> unpack text <> " not supported"

instance FromJSON UserDependency where
  parseJSON = withObject "UserDependency" $ \obj ->
    UserDependency <$> obj .: "package"
      <*> (obj .: "type" >>= depTypeParser)
      <*> obj .:? "version"

-- Parse supported dependency types into their respective type or return Nothing.
depTypeFromText :: Text -> Maybe DepType
depTypeFromText text = case text of
  "cargo" -> Just CargoType
  "carthage" -> Just CarthageType
  "composer" -> Just ComposerType
  "gem" -> Just GemType
  "git" -> Just GitType
  "go" -> Just GoType
  "hackage" -> Just HackageType
  "hex" -> Just HexType
  "maven" -> Just MavenType
  "npm" -> Just NodeJSType
  "nuget" -> Just NuGetType
  "python" -> Just PipType
  "cocoapods" -> Just PodType
  "url" -> Just URLType
  _ -> Nothing -- unsupported dep, need to respond with an error and skip this dependency
  -- rpm is an unsupported type. This is because we currently have 2 RPM fetchers
  -- and we should wait for a need to determine which one to use for manually
  -- specified dependencies.
