module Strategy.Godep (
  discover,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Control.Effect.Diagnostics qualified as Diag
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Path
import Strategy.Go.GopkgLock qualified as GopkgLock
import Strategy.Go.GopkgToml qualified as GopkgToml
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
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

mkProject :: (Has ReadFS sig n, Has Exec sig n, Has Diagnostics sig n) => GodepProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "godep"
    , projectBuildTargets = mempty
    , projectDependencyResults = const $ getDeps project
    , projectPath = godepDir project
    , projectLicenses = pure []
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
