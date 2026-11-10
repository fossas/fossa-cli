module App.Fossa.Analyze.ProjectFindings (renderProjectFindings) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes (DepEnvironment (..), Dependency (..))
import Graphing (Graphing)
import Graphing qualified
import Path (toFilePath)
import Srclib.Converter (toLocator)
import Srclib.Types (renderLocator)
import Types (GraphBreadth (Complete, Partial))

-- | Render the unfiltered dependency graph of each project as a findings
-- document. Nodes are identified by index into @nodes@ (two nodes may share a
-- locator); ordering is fully deterministic: nodes sort by rendered locator,
-- ties broken by environments; edges and direct-node indices are sorted.
renderProjectFindings :: [ProjectResult] -> Aeson.Value
renderProjectFindings projects = Aeson.object ["projects" .= map renderProject projects]

renderProject :: ProjectResult -> Aeson.Value
renderProject project =
  Aeson.object
    [ "projectType" .= toText (projectResultType project)
    , "path" .= toText (toFilePath (projectResultPath project))
    , "breadth" .= renderBreadth (projectResultGraphBreadth project)
    , "nodes" .= map renderNode nodes
    , "edges" .= map renderEdge edges
    , "directNodes" .= directIndices
    ]
  where
    graph = projectResultGraph project
    -- Sort by rendered locator, ties broken by environments.
    nodes :: [Dependency]
    nodes = sortBy (compare `on` nodeKey) $ Graphing.toList graph

    nodeIndex :: Map.Map Dependency Int
    nodeIndex = Map.fromList $ zip nodes [0 ..]

    -- Every dependency in 'edgesList' and 'directList' is a vertex of the
    -- same graph that 'nodes' lists, so a missing entry is unreachable; the
    -- fallback index keeps the renderer total.
    lookupIndex :: Dependency -> Int
    lookupIndex dep = fromMaybe 0 $ Map.lookup dep nodeIndex

    -- 'edgesList' already returns distinct edges; sort the rendered index
    -- pairs for a deterministic document.
    edges :: [(Int, Int)]
    edges = sort $ map (bimap lookupIndex lookupIndex) $ Graphing.edgesList graph

    directIndices :: [Int]
    directIndices = sort $ map lookupIndex $ Graphing.directList graph

nodeKey :: Dependency -> (Text, [Text])
nodeKey dep = (renderLocator (toLocator dep), renderEnvironments dep)

-- | Sorted rendered environments; deterministic on purpose.
renderEnvironments :: Dependency -> [Text]
renderEnvironments Dependency{dependencyEnvironments} = sort $ map renderEnvironment $ Set.toList dependencyEnvironments

renderEnvironment :: DepEnvironment -> Text
renderEnvironment env = case env of
  EnvProduction -> "production"
  EnvDevelopment -> "development"
  EnvTesting -> "testing"
  EnvOther other -> "other:" <> other

renderNode :: Dependency -> Aeson.Value
renderNode dep =
  Aeson.object
    [ "locator" .= renderLocator (toLocator dep)
    , "environments" .= renderEnvironments dep
    ]

renderEdge :: (Int, Int) -> Aeson.Value
renderEdge (fromIndex, toIndex) =
  Aeson.object
    [ "fromIndex" .= fromIndex
    , "toIndex" .= toIndex
    ]

renderBreadth :: GraphBreadth -> Text
renderBreadth breadth = case breadth of
  Complete -> "GRAPH_BREADTH_COMPLETE"
  Partial -> "GRAPH_BREADTH_PARTIAL"
