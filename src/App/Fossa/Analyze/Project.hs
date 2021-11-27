module App.Fossa.Analyze.Project (
  ProjectResult (..),
  mkResult,
) where

import Data.Text (Text)
import DepTypes
import Graphing (Graphing)
import Graphing qualified
import Path
import Path.Extra (tryMakeRelative)
import Types

mkResult :: Path Abs Dir -> DiscoveredProject n -> (DependencyResults) -> ProjectResult
mkResult basedir project dependencyResults =
  ProjectResult
    { projectResultType = projectType project
    , projectResultPath = projectPath project
    , projectResultGraph = graph
    , projectResultGraphBreadth = dependencyGraphBreadth dependencyResults
    , projectResultManifestFiles = relativeManifestFiles
    }
  where
    graph = dependencyGraph dependencyResults
    relativeManifestFiles = map (tryMakeRelative basedir) $ dependencyManifestFiles dependencyResults

data ProjectResult = ProjectResult
  { projectResultType :: Text
  , projectResultPath :: Path Abs Dir
  , projectResultGraph :: Graphing Dependency
  , projectResultGraphBreadth :: GraphBreadth
  , projectResultManifestFiles :: [SomeBase File]
  }
