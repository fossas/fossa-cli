module Strategy.Rebar3
  ( discover,
  )
where

import Control.Effect.Diagnostics (Diagnostics)
import Control.Monad.IO.Class
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing
import Path
import qualified Strategy.Erlang.Rebar3Tree as Rebar3Tree
import Types

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [RebarProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "rebar.config" files of
    Nothing -> pure ([], WalkContinue)
    Just _ -> pure ([RebarProject dir], WalkSkipAll)

data RebarProject = RebarProject
  { rebarDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: RebarProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "rebar3",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO . runExecIO $ getDeps project,
      projectPath = rebarDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => RebarProject -> m (Graphing Dependency)
getDeps project = Rebar3Tree.analyze' (rebarDir project)
