{-# LANGUAGE DerivingStrategies #-}

module App.Fossa.Analyze.ProjectFindings (
  FindingsLinking (..),
  defaultLinking,
  renderProjectFindings,
) where

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
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), renderEnvironment)
import Graphing qualified
import Path (Abs, Dir, Path, SomeBase (..), toFilePath)
import Path.Extra (tryMakeRelative)
import Srclib.Converter (toLocator)
import Srclib.Types (Locator, renderLocator)
import System.FilePath (dropTrailingPathSeparator)
import Types (DiscoveredProjectType, GraphBreadth, renderGraphType)

-- | Render the pre-prune dependency resolution of each project as a findings
-- document matching the @fossa/workflows/v1beta1@ @ProjectFindings@ contract:
--
-- > {"projects": [{
-- >   "projectType": "gradle", "path": "packages/cli",
-- >   "breadth": "GRAPH_BREADTH_COMPLETE",
-- >   "dependencies": {"mvn+org.example:lib$1.0.0": {"occurrences": [
-- >     {"module": "packages/cli", "isDirect": true, "scope": "production",
-- >      "linking": {"type": "LINKING_TYPE_DYNAMIC", "tier": "DETERMINATION_TIER_RULE",
-- >                  "evidence": {"summary": "Hard-coded per-ecosystem default",
-- >                                "references": []}},
-- >      "aliasedFrom": "github+myorg/lib$1.0.0"}]}},
-- >   "graph": {"nodes": [{"locator": "mvn+org.example:lib$1.0.0",
-- >                         "environments": ["production"]}],
-- >             "edges": [{"fromIndex": 1, "toIndex": 0}], "directNodes": [1]}}]}
--
-- @aliasedFrom@ is present only when a fork alias changed the locator. Nodes
-- are identified by index, not locator. Project paths are rendered relative to
-- the scan target (falling back to the absolute path for projects outside it).
-- Determination fields this producer cannot adjudicate (@rawScope@, @isOptional@,
-- @distribution@ and @provenance@) are omitted; @linking@ is always a rule-tier
-- ecosystem default, which evidence-backed determinations out-rank.
renderProjectFindings :: Path Abs Dir -> Map.Map Locator ForkAlias -> [ProjectResult] -> Aeson.Value
renderProjectFindings basedir forkAliasMap projects =
  toJSON (FindingsDocument (map (renderProject basedir forkAliasMap) projects))

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
  , findingsOccurrenceLinking :: FindingsLinking
  -- ^ Always emitted: the rule-tier per-ecosystem default, never an adjudication.
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
      , "linking" .= findingsOccurrenceLinking o
      ]
        <> maybe [] (\raw -> [fromText "aliasedFrom" .= raw]) (findingsOccurrenceAliasedFrom o)

-- | The rule-tier linking determination for an occurrence. Mirrors
-- @AnalyzedDependency.linking@ in fossa/workflows/v1beta1/workflow_result.proto:
-- @type@ plus @tier@ plus the
-- evidence that produced it, and @unknownReason@ exactly when the type is
-- @LINKING_TYPE_UNKNOWN@.
data FindingsLinking = LinkingStatic | LinkingDynamic | LinkingUnknown
  deriving stock (Eq, Ord, Show)

instance ToJSON FindingsLinking where
  toJSON = \case
    LinkingStatic -> linkingObject "LINKING_TYPE_STATIC" []
    LinkingDynamic -> linkingObject "LINKING_TYPE_DYNAMIC" []
    LinkingUnknown ->
      linkingObject "LINKING_TYPE_UNKNOWN" ["unknownReason" .= ("UNKNOWN_REASON_NEEDS_ADJUDICATION" :: Text)]
    where
      linkingObject ty extra =
        object $
          [ "type" .= (ty :: Text)
          , "tier" .= ("DETERMINATION_TIER_RULE" :: Text)
          , "evidence" .= object ["summary" .= ("Hard-coded per-ecosystem default" :: Text), "references" .= ([] :: [Text])]
          ]
            <> extra

-- | Hard-coded per-ecosystem linking default. STATIC: the dependency's code is
-- compiled or copied into the built artifact. DYNAMIC: resolved at load or run
-- time from elsewhere (classpath jars, node_modules, shared libs). Coarse by
-- design -- within-ecosystem exceptions (shaded jars, bundled JS, dynamic
-- framework builds) are the workflow adjudicator's job; this is the RULE-tier
-- prior, not a determination.
defaultLinking :: DepType -> FindingsLinking
defaultLinking = \case
  -- statically compiled / copied in
  GoType -> LinkingStatic
  CargoType -> LinkingStatic
  SwiftType -> LinkingStatic
  HackageType -> LinkingStatic
  HexType -> LinkingStatic
  ConanType -> LinkingStatic
  PodType -> LinkingStatic
  PathType -> LinkingStatic
  UnresolvedPathType -> LinkingStatic
  PubType -> LinkingStatic
  SubprojectType -> LinkingStatic
  -- resolved at load / run time
  MavenType -> LinkingDynamic
  NodeJSType -> LinkingDynamic
  PipType -> LinkingDynamic
  GemType -> LinkingDynamic
  NuGetType -> LinkingDynamic
  ComposerType -> LinkingDynamic
  CpanType -> LinkingDynamic
  CranType -> LinkingDynamic
  CondaType -> LinkingDynamic
  CarthageType -> LinkingDynamic
  LinuxAPK -> LinkingDynamic
  LinuxDEB -> LinkingDynamic
  LinuxRPM -> LinkingDynamic
  RPMType -> LinkingDynamic
  BowerType -> LinkingDynamic
  -- delivery-model dependent: no per-ecosystem answer exists
  GitType -> LinkingUnknown
  GooglesourceType -> LinkingUnknown
  URLType -> LinkingUnknown
  ArchiveType -> LinkingUnknown
  UserType -> LinkingUnknown
  CustomType -> LinkingUnknown

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

renderProject :: Path Abs Dir -> Map.Map Locator ForkAlias -> ProjectResult -> AnalyzedProjectFindings
renderProject basedir forkAliasMap project =
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
    graph = unRawGraph (projectResultUnprunedGraph project)
    -- Relative to the scan target per the proto contract; absolute fallback
    -- for projects outside it.
    -- The wire contract spells paths with forward slashes on every platform;
    -- toFilePath is native and a component cannot contain a raw backslash.
    wirePath = Text.replace "\\" "/" . toText . dropTrailingPathSeparator . toFilePath
    -- Contract: empty when the project is the scan target itself;
    -- tryMakeRelative only strips proper prefixes, so the equal case is open-coded.
    pathText
      | projectResultPath project == basedir = ""
      | otherwise = case tryMakeRelative basedir (projectResultPath project) of
          Rel rel -> wirePath rel
          Abs absolute -> wirePath absolute
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

    -- Per translated locator, per scope: (isDirect, aliasedFrom, linking).
    -- Rows are unique on scope by contract; same-locator nodes merge, so row
    -- order decides the survivor.
    occurrences :: Map.Map Text (Map.Map DepEnvironment (Bool, Maybe Text, FindingsLinking))
    occurrences = foldr addNodeOccurrence Map.empty nodes

    addNodeOccurrence dep = Map.insertWith mergeOccurrences (aliasedLocatorText dep) rows
      where
        isDirect = Set.member dep directSet
        rows = Map.fromList [(env, (isDirect, aliasedFrom dep, defaultLinking (dependencyType dep))) | env <- Set.toList (dependencyEnvironments dep)]

    mergeOccurrences :: Map.Map DepEnvironment (Bool, Maybe Text, FindingsLinking) -> Map.Map DepEnvironment (Bool, Maybe Text, FindingsLinking) -> Map.Map DepEnvironment (Bool, Maybe Text, FindingsLinking)
    mergeOccurrences = Map.unionWith combineRow
      where
        combineRow (isDirect1, aliased1, linking1) (isDirect2, aliased2, _) =
          ( isDirect1 || isDirect2
          , case (aliased1, aliased2) of
              (Just t, _) -> Just t
              (_, other) -> other
          , linking1
          )

    occurrenceRows :: Map.Map DepEnvironment (Bool, Maybe Text, FindingsLinking) -> [FindingsOccurrence]
    occurrenceRows scopeMap =
      [FindingsOccurrence pathText isDirect env linking aliased | (env, (isDirect, aliased, linking)) <- sortOn (renderEnvironment . fst) (Map.toList scopeMap)]

    -- Env-less deps have no rows; they stay visible as graph nodes.
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
