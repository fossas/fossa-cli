module Strategy.Pixi (
  discover,
  findProjects,
  mkProject,
  getDeps,
  PixiProject (..),
  PixiManifest (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, Has, context, recover, warnOnErr)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.Foldable (fold)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipSome),
  findFileNamed,
  walkWithFilters',
 )
import Effect.ReadFS (ReadFS, readContentsToml)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Pixi.Manifest (PixiManifestDeps, PyprojectPixiDeps (PyprojectPixiDeps))
import Strategy.Pixi.PixiLock qualified as PixiLock
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (PixiProjectType),
  GraphBreadth (Complete),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject PixiProject]
discover = simpleDiscover findProjects mkProject PixiProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [PixiProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "pixi.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just lockFile -> do
      let project =
            PixiProject
              { pixiDir = dir
              , pixiLock = lockFile
              , pixiManifest = (PixiToml <$> findFileNamed "pixi.toml" files) <|> (PyprojectToml <$> findFileNamed "pyproject.toml" files)
              }
      -- `.pixi` holds installed environments. Workspaces nested below have their own lock.
      pure ([project], WalkSkipSome [".pixi"])

data PixiProject = PixiProject
  { pixiDir :: Path Abs Dir
  , pixiLock :: Path Abs File
  , pixiManifest :: Maybe PixiManifest
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PixiProject

-- | pixi prefers @pixi.toml@ to @pyproject.toml@ in the same directory.
data PixiManifest
  = PixiToml (Path Abs File)
  | PyprojectToml (Path Abs File)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PixiManifest

instance AnalyzeProject PixiProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = getDeps

mkProject :: PixiProject -> DiscoveredProject PixiProject
mkProject project =
  DiscoveredProject
    { projectType = PixiProjectType
    , projectBuildTargets = mempty
    , projectPath = pixiDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PixiProject -> m DependencyResults
getDeps project = context "Pixi" $ do
  declared <- maybe (pure mempty) manifestDeps (pixiManifest project)
  graph <- PixiLock.analyze declared (pixiLock project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [pixiLock project]
      }

-- | With no readable manifest, nothing is declared, and direct dependencies are inferred from the lockfile.
manifestDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PixiManifest -> m PixiManifestDeps
manifestDeps manifest = fmap fold . recover . warnOnErr unreadable $ case manifest of
  PixiToml path -> readContentsToml path
  PyprojectToml path -> (\(PyprojectPixiDeps deps) -> fold deps) <$> readContentsToml path
  where
    unreadable :: Text
    unreadable = "pixi: could not read the manifest, so direct dependencies are inferred from pixi.lock"
