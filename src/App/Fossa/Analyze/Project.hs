module App.Fossa.Analyze.Project (
  ProjectResult (..),
  mkResult,
) where

import DepTypes
import Graphing (Graphing)
import Graphing qualified
import Path
import Path.Extra (tryMakeRelative)
import Types

mkResult :: Path Abs Dir -> DiscoveredProject n -> Maybe (Path Rel Dir) -> (DependencyResults) -> ProjectResult
mkResult basedir project pathPrefix dependencyResults =
  ProjectResult
    { projectResultType = projectType project
    , projectResultPath = projectPath project
    , projectResultGraph =
        -- FIXME: this is a hack to work around analyzers that aren't able to
        -- determine which dependencies are direct. Without this hack, all of
        -- their dependencies would be filtered out. The real fix to this is to
        -- have a separate designation for "reachable" vs "direct" on nodes in a
        -- Graphing, where direct deps are inherently reachable.
        if null (Graphing.directList graph) || shouldKeepUnreachableDeps (projectType project)
          then graph
          else Graphing.pruneUnreachable graph
    , projectResultGraphBreadth = dependencyGraphBreadth dependencyResults
    , projectResultManifestFiles = prefixedManifestFiles
    }
  where
    graph = dependencyGraph dependencyResults
    relativeManifestFiles = map (tryMakeRelative basedir) $ dependencyManifestFiles dependencyResults
    prefixedManifestFiles = map (addPrefix pathPrefix) relativeManifestFiles
    addPrefix :: Maybe (Path Rel Dir) -> SomeBase File -> SomeBase File
    addPrefix maybePrefix relativeFile  =
      case (maybePrefix, relativeFile) of
        (Nothing, relFile) -> relFile
        (Just prefix, Rel relFile) -> Rel $ prefix </> relFile
        (Just _, Abs absFile) -> Abs absFile

data ProjectResult = ProjectResult
  { projectResultType :: DiscoveredProjectType
  , projectResultPath :: Path Abs Dir
  , projectResultGraph :: Graphing Dependency
  , projectResultGraphBreadth :: GraphBreadth
  , projectResultManifestFiles :: [SomeBase File]
  }
  deriving (Show)

shouldKeepUnreachableDeps :: DiscoveredProjectType -> Bool
shouldKeepUnreachableDeps SwiftProjectType = True
shouldKeepUnreachableDeps GomodProjectType = True
shouldKeepUnreachableDeps _ = False
