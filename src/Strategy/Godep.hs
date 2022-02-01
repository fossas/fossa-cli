module Strategy.Godep (
  discover,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Control.Effect.Diagnostics qualified as Diag
import Data.Aeson (ToJSON)
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import GHC.Generics (Generic)
import Path
import Strategy.Go.GopkgLock qualified as GopkgLock
import Strategy.Go.GopkgToml qualified as GopkgToml
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject GodepProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [GodepProject]
findProjects = walk' $ \dir _ files -> do
  let gopkgToml = findFileNamed "Gopkg.toml" files
  let gopkgLock = findFileNamed "Gopkg.lock" files

  let project =
        GodepProject
          { godepToml = gopkgToml
          , godepLock = gopkgLock
          , godepDir = dir
          }

  case gopkgToml <|> gopkgLock of
    Nothing -> pure ([], WalkSkipSome ["vendor"])
    Just _ -> pure ([project], WalkSkipSome ["vendor"])

data GodepProject = GodepProject
  { godepDir :: Path Abs Dir
  , godepToml :: Maybe (Path Abs File)
  , godepLock :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GodepProject

instance AnalyzeProject GodepProject where
  analyzeProject _ = getDeps

mkProject :: GodepProject -> DiscoveredProject GodepProject
mkProject project =
  DiscoveredProject
    { projectType = GodepProjectType
    , projectBuildTargets = mempty
    , projectPath = godepDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => GodepProject -> m DependencyResults
getDeps project =
  context "Godep" $
    context "Gopkg.lock analysis" (analyzeGopkgLock project)
      <||> context "Gopkg.toml analysis" (analyzeGopkgToml project)

analyzeGopkgLock :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => GodepProject -> m DependencyResults
analyzeGopkgLock project = do
  lockFile <- Diag.fromMaybeText "No Gopkg.lock present in the project" (godepLock project)
  graph <- GopkgLock.analyze' lockFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [lockFile]
      }

analyzeGopkgToml :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => GodepProject -> m DependencyResults
analyzeGopkgToml project = do
  tomlFile <- Diag.fromMaybeText "No Gopkg.toml present in the project" (godepToml project)
  graph <- GopkgToml.analyze' tomlFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [tomlFile]
      }
