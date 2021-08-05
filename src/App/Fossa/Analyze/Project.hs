module App.Fossa.Analyze.Project (
  ProjectResult (..),
  mkResult,
) where

import Data.Text (Text)
import DepTypes
import Graphing (Graphing)
import Graphing qualified
import Path
import Types

mkResult :: DiscoveredProject n -> (Graphing Dependency, GraphBreadth) -> ProjectResult
mkResult project graphResults =
  ProjectResult
    { projectResultType = projectType project
    , projectResultPath = projectPath project
    , projectResultGraph =
        -- FIXME: this is a hack to work around analyzers that aren't able to
        -- determine which dependencies are direct. Without this hack, all of
        -- their dependencies would be filtered out. The real fix to this is to
        -- have a separate designation for "reachable" vs "direct" on nodes in a
        -- Graphing, where direct deps are inherently reachable.
        if null (Graphing.directList graph)
          then graph
          else Graphing.pruneUnreachable graph
    , projectResultGraphBreadth = graphBreadth
    }
  where
    (graph, graphBreadth) = graphResults

data ProjectResult = ProjectResult
  { projectResultType :: Text
  , projectResultPath :: Path Abs Dir
  , projectResultGraph :: Graphing Dependency
  , projectResultGraphBreadth :: GraphBreadth
  }
