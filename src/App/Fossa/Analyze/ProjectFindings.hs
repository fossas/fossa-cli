module App.Fossa.Analyze.ProjectFindings (renderProjectFindings) where

import App.Fossa.Analyze.ForkAlias (translateLocatorWithForkAliases)
import App.Fossa.Analyze.Project (ProjectResult (..), unRawGraph)
import App.Fossa.ManualDeps (ForkAlias)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (fromText)
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
import Srclib.Types (Locator, renderLocator)
import Types (GraphBreadth (Complete, Partial))

-- | Render the pre-prune dependency resolution of each project as a findings
-- document matching the @fossa/workflows/v1beta1@ @ProjectFindings@ contract:
--
-- > {"projects": [{
-- >   "projectType": "gradle", "path": "packages/cli",
-- >   "breadth": "GRAPH_BREADTH_COMPLETE",
-- >   "dependencies": {"mvn+org.example:lib$1.0.0": {"occurrences": [
-- >     {"module": "packages/cli", "isDirect": true, "scope": "production",
-- >      "aliasedFrom": "github+myorg/lib$1.0.0"}]}},
-- >   "graph": {"nodes": [{"locator": "mvn+org.example:lib$1.0.0",
-- >                         "environments": ["production"]}],
-- >             "edges": [{"fromIndex": 1, "toIndex": 0}], "directNodes": [1]}}]}
--
-- @aliasedFrom@ is present only when a fork alias changed the locator. Nodes
-- are identified by index, not locator. Determination fields this producer
-- cannot adjudicate (@rawScope@, @isOptional@, @linking@, @distribution@,
-- @provenance@) are omitted: consumers read absence as "no determination was
-- made".
renderProjectFindings :: Map.Map Locator ForkAlias -> [ProjectResult] -> Aeson.Value
renderProjectFindings forkAliasMap projects =
  Aeson.object ["projects" .= map (renderProject forkAliasMap) projects]

renderProject :: Map.Map Locator ForkAlias -> ProjectResult -> Aeson.Value
renderProject forkAliasMap project =
  Aeson.object
    [ "projectType" .= toText (projectResultType project)
    , "path" .= pathText
    , "breadth" .= renderBreadth (projectResultGraphBreadth project)
    , "dependencies" .= dependenciesObject
    , "graph" .= graphObject
    ]
  where
    -- Findings capture the resolution before 'pruneUnreachable';
    -- 'projectResultGraph' is the pruned, published view.
    graph = unRawGraph (projectResultUnprunedGraph project)
    pathText = toText (toFilePath (projectResultPath project))
    directSet :: Set.Set Dependency
    directSet = Set.fromList (Graphing.directList graph)

    rawLocatorText :: Dependency -> Text
    rawLocatorText dep = renderLocator (toLocator dep)

    aliasedLocatorText :: Dependency -> Text
    aliasedLocatorText dep = renderLocator (translateLocatorWithForkAliases forkAliasMap (toLocator dep))

    -- Present only when a fork alias actually changed this dependency's locator.
    aliasedFrom :: Dependency -> Maybe Text
    aliasedFrom dep
      | translated /= raw = Just raw
      | otherwise = Nothing
      where
        raw = rawLocatorText dep
        translated = aliasedLocatorText dep

    -- The raw-locator tiebreak keeps indices stable between nodes whose (aliased
    -- locator, environments) pairs are identical.
    nodeKey :: Dependency -> (Text, [Text], Text)
    nodeKey dep = (aliasedLocatorText dep, renderEnvironments dep, rawLocatorText dep)

    nodes :: [Dependency]
    nodes = sortBy (compare `on` nodeKey) (Graphing.toList graph)

    nodeIndex :: Map.Map Dependency Int
    nodeIndex = Map.fromList (zip nodes [0 ..])

    -- Every edge endpoint is a vertex of this same graph, so the missing case
    -- is unreachable; the fallback index keeps the renderer total.
    lookupIndex :: Dependency -> Int
    lookupIndex dep = fromMaybe 0 (Map.lookup dep nodeIndex)

    -- Set dedups and sorts in one pass, so the ordering guarantee is local
    -- rather than resting on 'edgesList'.
    edges :: [(Int, Int)]
    edges =
      Set.toAscList
        . Set.map (bimap lookupIndex lookupIndex)
        . Set.fromList
        $ Graphing.edgesList graph

    directIndices :: [Int]
    directIndices = sort (map lookupIndex (Graphing.directList graph))

    renderNode :: Dependency -> Aeson.Value
    renderNode dep =
      Aeson.object
        [ "locator" .= aliasedLocatorText dep
        , "environments" .= renderEnvironments dep
        ]

    graphObject :: Aeson.Value
    graphObject =
      Aeson.object
        [ "nodes" .= map renderNode nodes
        , "edges" .= map renderEdge edges
        , "directNodes" .= directIndices
        ]

    -- \| Per translated locator, per scope: (isDirect, aliasedFrom). Rows are
    -- unique on scope by contract; when two nodes share a translated locator,
    -- their rows merge on scope with isDirect ORed and the first-encountered
    -- aliasedFrom kept (nodes are processed in sorted order).
    occurrences :: Map.Map Text (Map.Map Text (Bool, Maybe Text))
    occurrences = foldr addNodeOccurrence Map.empty nodes

    addNodeOccurrence dep = Map.insertWith mergeOccurrences (aliasedLocatorText dep) rows
      where
        isDirect = Set.member dep directSet
        rows = Map.fromList [(renderEnvironment env, (isDirect, aliasedFrom dep)) | env <- Set.toList (dependencyEnvironments dep)]

    mergeOccurrences :: Map.Map Text (Bool, Maybe Text) -> Map.Map Text (Bool, Maybe Text) -> Map.Map Text (Bool, Maybe Text)
    mergeOccurrences = Map.unionWith combineRow
      where
        combineRow (isDirect1, aliased1) (isDirect2, aliased2) =
          ( isDirect1 || isDirect2
          , case (aliased1, aliased2) of
              (Just t, _) -> Just t
              (_, other) -> other
          )

    dependenciesObject :: Aeson.Value
    dependenciesObject =
      Aeson.object $
        [ (fromText locatorText, Aeson.object ["occurrences" .= renderOccurrences scopeMap])
        | (locatorText, scopeMap) <- Map.toList occurrences
        , -- 'DependencyOccurrences.occurrences' must never be empty when the key
        -- is present; env-less nodes stay visible through the graph sibling.
        not (Map.null scopeMap)
        ]

    renderOccurrence :: (Text, (Bool, Maybe Text)) -> Aeson.Value
    renderOccurrence (scope, (isDirect, aliasedFrom')) =
      Aeson.object $
        [ "module" .= pathText
        , "isDirect" .= isDirect
        , "scope" .= scope
        ]
          <> case aliasedFrom' of
            Nothing -> []
            Just raw -> [(fromText "aliasedFrom", Aeson.String raw)]

    renderOccurrences :: Map.Map Text (Bool, Maybe Text) -> [Aeson.Value]
    renderOccurrences = map renderOccurrence . Map.toList

renderEdge :: (Int, Int) -> Aeson.Value
renderEdge (fromIndex, toIndex) =
  Aeson.object
    [ "fromIndex" .= fromIndex
    , "toIndex" .= toIndex
    ]

-- | Sorted rendered environments; deterministic on purpose.
renderEnvironments :: Dependency -> [Text]
renderEnvironments Dependency{dependencyEnvironments} = sort $ map renderEnvironment $ Set.toList dependencyEnvironments

renderEnvironment :: DepEnvironment -> Text
renderEnvironment env = case env of
  EnvProduction -> "production"
  EnvDevelopment -> "development"
  EnvTesting -> "testing"
  EnvOther other -> "other:" <> other

renderBreadth :: GraphBreadth -> Text
renderBreadth breadth = case breadth of
  Complete -> "GRAPH_BREADTH_COMPLETE"
  Partial -> "GRAPH_BREADTH_PARTIAL"
