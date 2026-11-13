{-# LANGUAGE DerivingStrategies #-}

module App.Fossa.Analyze.ProjectFindings (renderProjectFindings) where

import App.Fossa.Analyze.ForkAlias (translateLocatorWithForkAliases)
import App.Fossa.Analyze.Project (ProjectResult (..), unRawGraph)
import App.Fossa.ManualDeps (ForkAlias)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (fromText)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (sort, sortBy, sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (DepEnvironment (..), Dependency (..), renderEnvironment)
import Graphing qualified
import Path (toFilePath)
import Srclib.Converter (toLocator)
import Srclib.Types (Locator, renderLocator)
import Types (DiscoveredProjectType, GraphBreadth, renderGraphType)

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
  toJSON (FindingsDocument (map (renderProject forkAliasMap) projects))

newtype FindingsDocument = FindingsDocument {findingsProjects :: [AnalyzedProjectFindings]}
  deriving stock (Eq, Show)

instance ToJSON FindingsDocument where
  toJSON (FindingsDocument projects) = object ["projects" .= projects]

data AnalyzedProjectFindings = AnalyzedProjectFindings
  { findingsProjectType :: DiscoveredProjectType
  , findingsPath :: Text
  , findingsBreadth :: GraphBreadth
  , findingsDependencies :: Map.Map Text FindingsOccurrences
  , findingsGraph :: IndexedGraph
  }
  deriving stock (Eq, Show)

instance ToJSON AnalyzedProjectFindings where
  toJSON p =
    object
      [ "projectType" .= toText (findingsProjectType p)
      , "path" .= findingsPath p
      , "breadth" .= renderWireBreadth (findingsBreadth p)
      , "dependencies" .= findingsDependencies p
      , "graph" .= findingsGraph p
      ]

-- | Wire spelling derived from the internal renderer: @GRAPH_BREADTH_@ plus
-- its upper-cased spelling (@complete@ -> @GRAPH_BREADTH_COMPLETE@).
renderWireBreadth :: GraphBreadth -> Text
renderWireBreadth breadth = "GRAPH_BREADTH_" <> Text.toUpper (renderGraphType breadth)

newtype FindingsOccurrences = FindingsOccurrences {findingsOccurrenceRows :: [FindingsOccurrence]}
  deriving stock (Eq, Show)

instance ToJSON FindingsOccurrences where
  toJSON (FindingsOccurrences rows) = object ["occurrences" .= rows]

data FindingsOccurrence = FindingsOccurrence
  { findingsOccurrenceModule :: Text
  , findingsOccurrenceIsDirect :: Bool
  , findingsOccurrenceScope :: DepEnvironment
  , findingsOccurrenceAliasedFrom :: Maybe Text
  -- ^ Present only when a fork alias actually changed the dependency's locator.
  }
  deriving stock (Eq, Show)

instance ToJSON FindingsOccurrence where
  toJSON o =
    object $
      [ "module" .= findingsOccurrenceModule o
      , "isDirect" .= findingsOccurrenceIsDirect o
      , "scope" .= findingsOccurrenceScope o
      ]
        <> maybe [] (\raw -> [fromText "aliasedFrom" .= raw]) (findingsOccurrenceAliasedFrom o)

data IndexedGraph = IndexedGraph
  { indexedGraphNodes :: [FindingsNode]
  , indexedGraphEdges :: [IndexedEdge]
  , indexedGraphDirectNodes :: [Int]
  }
  deriving stock (Eq, Show)

instance ToJSON IndexedGraph where
  toJSON g =
    object
      [ "nodes" .= indexedGraphNodes g
      , "edges" .= indexedGraphEdges g
      , "directNodes" .= indexedGraphDirectNodes g
      ]

data FindingsNode = FindingsNode
  { findingsNodeLocator :: Text
  , findingsNodeEnvironments :: [DepEnvironment]
  }
  deriving stock (Eq, Show)

instance ToJSON FindingsNode where
  toJSON n =
    object
      [ "locator" .= findingsNodeLocator n
      , "environments" .= findingsNodeEnvironments n
      ]

data IndexedEdge = IndexedEdge {indexedEdgeFrom :: Int, indexedEdgeTo :: Int}
  deriving stock (Eq, Show)

instance ToJSON IndexedEdge where
  toJSON e =
    object
      [ "fromIndex" .= indexedEdgeFrom e
      , "toIndex" .= indexedEdgeTo e
      ]

renderProject :: Map.Map Locator ForkAlias -> ProjectResult -> AnalyzedProjectFindings
renderProject forkAliasMap project =
  AnalyzedProjectFindings
    { findingsProjectType = projectResultType project
    , findingsPath = pathText
    , findingsBreadth = projectResultGraphBreadth project
    , findingsDependencies = dependencies
    , findingsGraph =
        IndexedGraph
          { indexedGraphNodes = map renderNode nodes
          , indexedGraphEdges = map (uncurry IndexedEdge) edges
          , indexedGraphDirectNodes = directIndices
          }
    }
  where
    -- Findings render the resolution as discovered, before publication
    -- filtering; 'projectResultGraph' is the published, pruned view.
    graph = unRawGraph (projectResultUnprunedGraph project)
    pathText = toText (toFilePath (projectResultPath project))
    directSet :: Set.Set Dependency
    directSet = Set.fromList (Graphing.directList graph)

    rawLocatorText :: Dependency -> Text
    rawLocatorText dep = renderLocator (toLocator dep)

    aliasedLocatorText :: Dependency -> Text
    aliasedLocatorText dep = renderLocator (translateLocatorWithForkAliases forkAliasMap (toLocator dep))

    -- The raw-locator tiebreak keeps indices stable between nodes whose (aliased
    -- locator, environments) pairs are identical.
    nodeKey :: Dependency -> (Text, [Text], Text)
    nodeKey dep = (aliasedLocatorText dep, map renderEnvironment (sortedEnvironments dep), rawLocatorText dep)

    -- Sorted by wire spelling, so output is deterministic regardless of graph
    -- construction order.
    sortedEnvironments :: Dependency -> [DepEnvironment]
    sortedEnvironments = sortOn renderEnvironment . Set.toList . dependencyEnvironments

    nodes :: [Dependency]
    nodes = sortBy (compare `on` nodeKey) (Graphing.toList graph)

    nodeIndex :: Map.Map Dependency Int
    nodeIndex = Map.fromList (zip nodes [0 ..])

    -- Every edge endpoint is a vertex of this same graph, so the missing case
    -- is unreachable; the fallback index keeps the renderer total.
    lookupIndex :: Dependency -> Int
    lookupIndex dep = fromMaybe 0 (Map.lookup dep nodeIndex)

    -- Set dedups and sorts in one pass, so the ordering guarantee is local
    -- rather than resting on the graph's edge order.
    edges :: [(Int, Int)]
    edges =
      Set.toAscList
        . Set.map (bimap lookupIndex lookupIndex)
        . Set.fromList
        $ Graphing.edgesList graph

    directIndices :: [Int]
    directIndices = sort (map lookupIndex (Graphing.directList graph))

    renderNode :: Dependency -> FindingsNode
    renderNode dep = FindingsNode (aliasedLocatorText dep) (sortedEnvironments dep)

    -- Per translated locator, per scope: (isDirect, aliasedFrom). Rows are
    -- unique on scope by contract; when two nodes share a translated locator,
    -- their rows merge on scope with isDirect ORed and the first-encountered
    -- aliasedFrom kept (nodes are processed in sorted order).
    occurrences :: Map.Map Text (Map.Map DepEnvironment (Bool, Maybe Text))
    occurrences = foldr addNodeOccurrence Map.empty nodes

    addNodeOccurrence dep = Map.insertWith mergeOccurrences (aliasedLocatorText dep) rows
      where
        isDirect = Set.member dep directSet
        rows = Map.fromList [(env, (isDirect, aliasedFrom dep)) | env <- Set.toList (dependencyEnvironments dep)]

    mergeOccurrences :: Map.Map DepEnvironment (Bool, Maybe Text) -> Map.Map DepEnvironment (Bool, Maybe Text) -> Map.Map DepEnvironment (Bool, Maybe Text)
    mergeOccurrences = Map.unionWith combineRow
      where
        combineRow (isDirect1, aliased1) (isDirect2, aliased2) =
          ( isDirect1 || isDirect2
          , case (aliased1, aliased2) of
              (Just t, _) -> Just t
              (_, other) -> other
          )

    -- Occurrence rows are emitted in wire-spelling order of their scope.
    occurrenceRows :: Map.Map DepEnvironment (Bool, Maybe Text) -> [FindingsOccurrence]
    occurrenceRows scopeMap =
      [FindingsOccurrence pathText isDirect env aliased | (env, (isDirect, aliased)) <- sortOn (renderEnvironment . fst) (Map.toList scopeMap)]

    -- A locator key is emitted only when it has at least one row; env-less
    -- nodes stay visible through the graph sibling.
    dependencies :: Map.Map Text FindingsOccurrences
    dependencies =
      Map.fromList
        [ (locatorText, FindingsOccurrences (occurrenceRows scopeMap))
        | (locatorText, scopeMap) <- Map.toList occurrences
        , not (Map.null scopeMap)
        ]

    aliasedFrom :: Dependency -> Maybe Text
    aliasedFrom dep
      | translated /= raw = Just raw
      | otherwise = Nothing
      where
        raw = rawLocatorText dep
        translated = aliasedLocatorText dep
