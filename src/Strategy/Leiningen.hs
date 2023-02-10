-- | The clojure strategy parses EDN from @lein deps :tree-data@
--
-- The output looks something like:
--
-- > {[clojure-complete "0.2.5" :exclusions [[org.clojure/clojure]]] nil,
-- >  [koan-engine "0.2.5"] {[fresh "1.0.2"] nil},
-- >  [lein-koan "0.1.5" :scope "test"] nil,
-- >  [nrepl "0.6.0" :exclusions [[org.clojure/clojure]]] nil,
-- >  [org.clojure/clojure "1.10.0"]
-- >  {[org.clojure/core.specs.alpha "0.2.44"] nil,
-- >   [org.clojure/spec.alpha "0.2.176"] nil}}
module Strategy.Leiningen (
  discover,
  buildGraph,
  findProjects,
  getDeps,
  mkProject,
  Deps (..),
  ClojureDep (..),
  LeiningenProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Applicative (optional)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic (..),
  context,
  errCtx,
  fatal,
  fatalText,
  run,
 )
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Data.EDN qualified as EDN
import Data.EDN.Class.Parser (Parser)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (
  AllowErr (Always, Never),
  Command (..),
  Exec,
  ExecErr (CommandParseError),
  exec,
  execThrow,
 )
import Effect.Grapher (
  LabeledGrapher,
  direct,
  edge,
  label,
  withLabeling,
 )
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, parent)
import Types (
  DepEnvironment (EnvOther, EnvTesting),
  DepType (MavenType),
  Dependency (..),
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (LeiningenProjectType),
  GraphBreadth (Complete),
  VerConstraint (CEq),
  insertEnvironment,
 )

leinDepsCmd :: Command
leinDepsCmd =
  Command
    { cmdName = "lein"
    , cmdArgs = ["deps", ":tree-data"]
    , cmdAllowErr = Never
    }

leinVersionCmd :: Command
leinVersionCmd =
  Command
    { cmdName = "lein"
    , cmdArgs = ["--version"]
    , cmdAllowErr = Always
    }

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject LeiningenProject]
discover = simpleDiscover findProjects mkProject LeiningenProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [LeiningenProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "project.clj" files of
    Nothing -> pure ([], WalkContinue)
    Just projectClj -> do
      let project =
            LeiningenProject
              { leinDir = dir
              , leinProjectClj = projectClj
              }

      pure ([project], WalkContinue)

mkProject :: LeiningenProject -> DiscoveredProject LeiningenProject
mkProject project =
  DiscoveredProject
    { projectType = LeiningenProjectType
    , projectBuildTargets = mempty
    , projectPath = leinDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has Diagnostics sig m) => LeiningenProject -> m DependencyResults
getDeps = context "Leiningen" . context "Dynamic analysis" . analyze . leinProjectClj

data LeiningenProject = LeiningenProject
  { leinDir :: Path Abs Dir
  , leinProjectClj :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON LeiningenProject

instance AnalyzeProject LeiningenProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = const $ fatalText "Cannot analyze Leiningen project statically"

analyze :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze file = do
  -- Lein executable performs configuration task and prints its progress on the first invocation.
  -- Intentionally invoke --version command to ensure subsequent analyses command are not lein's first invocation.
  -- This ensures, subsequent analyzes commands' outputs are not contaminated with lein's configuration task's output.
  _ <- exec (parent file) leinVersionCmd

  stdoutBL <- errCtx FailedToRetrieveLeinDependencies $ execThrow (parent file) leinDepsCmd
  let stdoutTL = decodeUtf8 stdoutBL
      stdout = TL.toStrict stdoutTL

  case EDN.decodeText "lein deps :tree-data" stdout of
    Left err -> fatal (CommandParseError leinDepsCmd (toText err))
    Right deps -> do
      graph <- context "Building dependency graph" $ pure (buildGraph deps)
      pure $
        DependencyResults
          { dependencyGraph = graph
          , dependencyGraphBreadth = Complete
          , dependencyManifestFiles = [file]
          }

data FailedToRetrieveLeinDependencies = FailedToRetrieveLeinDependencies
instance ToDiagnostic FailedToRetrieveLeinDependencies where
  renderDiagnostic _ = "We could not successfully retrieve dependencies information using lein deps subcommand."

-- node type for our LabeledGrapher
data ClojureNode = ClojureNode
  { nodeName :: Text
  , nodeVersion :: Text
  }
  deriving (Eq, Ord, Show)

-- label type for our LabeledGrapher
newtype ClojureLabel = ScopeLabel Text
  deriving (Eq, Ord, Show)

buildGraph :: Deps -> Graphing Dependency
buildGraph deps = run . withLabeling toDependency $ do
  traverse_ direct (topLevelNodes deps)
  buildEdges deps

-- extract the top-level clojure deps from a Deps as ClojureNodes
topLevelNodes :: Deps -> [ClojureNode]
topLevelNodes = map toClojureNode . Map.keys . depsTree

-- recursively build edges and add labels to dependencies we encounter
buildEdges :: Has (LabeledGrapher ClojureNode ClojureLabel) sig m => Deps -> m ()
buildEdges deps = Map.traverseWithKey single (depsTree deps) $> ()
  where
    single :: Has (LabeledGrapher ClojureNode ClojureLabel) sig m => ClojureDep -> Maybe Deps -> m ()
    single dep maybeDeeper = do
      let node = toClojureNode dep
      traverse_ (label node . ScopeLabel) (depScope dep)
      case maybeDeeper of
        Nothing -> pure ()
        Just deeper -> do
          traverse_ (edge node) (topLevelNodes deeper)
          buildEdges deeper

toClojureNode :: ClojureDep -> ClojureNode
toClojureNode dep = ClojureNode (reverseDepNameMerge $ Text.replace "/" ":" (depName dep)) (depVersion dep)

-- When a dependencies groupID and artifactID are identical, leiningen merges them.
-- In order to satisfy the Maven dependency format, we reverse the merge of these names.
reverseDepNameMerge :: Text -> Text
reverseDepNameMerge dep = if Text.any (== ':') dep then dep else dep <> ":" <> dep

toDependency :: ClojureNode -> Set ClojureLabel -> Dependency
toDependency node = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = MavenType
        , dependencyName = nodeName node
        , dependencyVersion = Just (CEq (nodeVersion node))
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
    applyLabel (ScopeLabel "test") dep = insertEnvironment EnvTesting dep
    applyLabel (ScopeLabel other) dep = insertEnvironment (EnvOther other) dep

instance EDN.FromEDN Deps where
  parseEDNv = fmap Deps . EDN.parseEDNv

instance EDN.FromEDN ClojureDep where
  parseEDNv = EDN.withVec $ \vec -> do
    name <- EDN.vecGet 0 vec
    version <- EDN.vecGet 1 vec
    attributes <- ednVecToMap (V.drop 2 vec)

    scope <- optional (EDN.mapGetKeyword "scope" attributes)
    pure (ClojureDep name version scope)

-- in the input EDN, a dependency is a vector of values, like:
--
-- > [lein-koan "0.1.5" :scope "test"]
--
-- after the name/version are a bunch of key/value pairs, so we can turn them
-- into a Map to make them easier to work with. in the future, this can be
-- turned into an Map TaggedValue [TaggedValue] -- but for now, we only care
-- about scopes which can only appear once
ednVecToMap :: EDN.EDNVec -> Parser EDN.EDNMap
ednVecToMap = go Map.empty
  where
    go :: EDN.EDNMap -> EDN.EDNVec -> Parser EDN.EDNMap
    -- TODO: refactor this to not use match guards
    go m vec
      | V.null vec = pure m
      | otherwise = do
        key <- EDN.vecGet 0 vec
        value <- EDN.vecGet 1 vec
        go (Map.insert key value m) (V.drop 2 vec)

-- | The FromEDN type for lein deps output
newtype Deps = Deps
  { depsTree :: Map ClojureDep (Maybe Deps)
  }
  deriving (Eq, Ord, Show)

-- | A single dependency in the lein deps output
data ClojureDep = ClojureDep
  { depName :: Text
  , depVersion :: Text
  , depScope :: Maybe Text
  }
  deriving (Eq, Ord, Show)
