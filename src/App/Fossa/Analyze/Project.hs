{-# LANGUAGE DerivingStrategies #-}

module App.Fossa.Analyze.Project (
  ProjectResult (..),
  mkResult,
  shouldKeepUnreachableDeps,
  RawGraph (RawGraph, unRawGraph),
  PublishedGraph (PublishedGraph, unPublishedGraph),
) where

import App.Util (FileAncestry (..))
import DepTypes
import Graphing (Graphing)
import Graphing qualified
import Path
import Path.Extra (SomePath (SomeFile), tryMakeRelative)
import Types

-- | The dependency resolution as discovered, before publication filtering.
-- Findings render from this view; it is never pruned or enriched.
newtype RawGraph = RawGraph {unRawGraph :: Graphing Dependency}
  deriving stock (Eq, Ord, Show)

-- | The published dependency graph: pruned by 'mkResult', then enriched.
-- What is uploaded as the project.
newtype PublishedGraph = PublishedGraph {unPublishedGraph :: Graphing Dependency}
  deriving stock (Eq, Ord, Show)

mkResult :: Path Abs Dir -> DiscoveredProject n -> Maybe FileAncestry -> (DependencyResults) -> ProjectResult
mkResult basedir project pathPrefix dependencyResults =
  ProjectResult
    { projectResultType = projectType project
    , projectResultPath = projectPath project
    , -- Resolution as discovered; the published view below is pruned.
      projectResultUnprunedGraph = RawGraph graph
    , projectResultGraph =
        PublishedGraph
          ( -- FIXME (pre-existing): this is a hack to work around analyzers that aren't able to
            -- determine which dependencies are direct or have no edges. Without this hack, all of
            -- their dependencies would be filtered out. The real fix to this is to
            -- have a separate designation for "reachable" vs "direct" on nodes in a
            -- Graphing, where direct deps are inherently reachable.
            if null (Graphing.directList graph)
              || null (Graphing.edgesList graph)
              || shouldKeepUnreachableDeps (projectType project)
              then graph
              else Graphing.pruneUnreachable graph
          )
    , projectResultGraphBreadth = dependencyGraphBreadth dependencyResults
    , projectResultManifestFiles = prefixedManifestFiles
    }
  where
    graph = dependencyGraph dependencyResults
    relativeManifestFiles = map (tryMakeRelative basedir) $ dependencyManifestFiles dependencyResults
    prefixedManifestFiles = map (SomeFile . addPrefix (fileAncestryPath <$> pathPrefix)) relativeManifestFiles
    addPrefix :: Maybe (Path Rel Dir) -> SomeBase File -> SomeBase File
    addPrefix maybePrefix relativeFile =
      case (maybePrefix, relativeFile) of
        (Nothing, relFile) -> relFile
        (Just prefix, Rel relFile) -> Rel $ prefix </> relFile
        (Just _, Abs absFile) -> Abs absFile

data ProjectResult = ProjectResult
  { projectResultType :: DiscoveredProjectType
  , projectResultPath :: Path Abs Dir
  , projectResultUnprunedGraph :: RawGraph
  -- ^ The dependency resolution as discovered, before publication filtering;
  -- 'projectResultGraph' is the published view.
  , projectResultGraph :: PublishedGraph
  , projectResultGraphBreadth :: GraphBreadth
  , projectResultManifestFiles :: [SomePath]
  }
  deriving (Show, Eq)

shouldKeepUnreachableDeps :: DiscoveredProjectType -> Bool
shouldKeepUnreachableDeps SwiftProjectType = True
shouldKeepUnreachableDeps GomodProjectType = True
shouldKeepUnreachableDeps _ = False
