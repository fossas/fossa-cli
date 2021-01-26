{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.PackagesConfig
  ( discover
  , findProjects
  , getDeps
  , mkProject
  , buildGraph

  , PackagesConfig(..)
  , NuGetDependency(..)
  ) where

import Control.Effect.Diagnostics
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (find)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Parse.XML
import Path
import Types

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [PackagesConfigProject]
findProjects = walk' $ \_ _ files -> do
  case find (\f -> fileName f == "packages.config") files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([PackagesConfigProject file], WalkContinue)

newtype PackagesConfigProject = PackagesConfigProject
  { packagesConfigFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

mkProject :: PackagesConfigProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "packagesconfig",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO $ getDeps project,
      projectPath = parent $ packagesConfigFile project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PackagesConfigProject -> m (Graphing Dependency)
getDeps = analyze' . packagesConfigFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = buildGraph <$> readContentsXML file

instance FromXML PackagesConfig where
  parseElement el = PackagesConfig <$> children "package" el

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency <$> attr "id" el
                    <*> attr "version" el

newtype PackagesConfig = PackagesConfig
  { deps :: [NuGetDependency]
  } deriving (Eq, Ord, Show)

data NuGetDependency = NuGetDependency
  { depID      :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show)

buildGraph :: PackagesConfig -> Graphing Dependency
buildGraph = Graphing.fromList . map toDependency . deps
    where
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depID
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }
