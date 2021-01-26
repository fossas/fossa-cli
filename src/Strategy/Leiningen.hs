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
module Strategy.Leiningen
  ( discover,
    buildGraph,
    findProjects,
    getDeps,
    mkProject,
    Deps (..),
    ClojureDep (..),
    LeiningenProject (..),
  )
where

import Control.Applicative (optional)
import Control.Effect.Diagnostics
import Control.Monad.IO.Class (MonadIO)
import qualified Data.EDN as EDN
import Data.EDN.Class.Parser (Parser)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Vector as V
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Path
import Types

leinDepsCmd :: Command
leinDepsCmd =
  Command
    { cmdName = "lein",
      cmdArgs = ["deps", ":tree-data"],
      cmdAllowErr = Never
    }

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [LeiningenProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "project.clj" files of
    Nothing -> pure ([], WalkContinue)
    Just projectClj -> do
      let project =
            LeiningenProject
              { leinDir = dir,
                leinProjectClj = projectClj
              }

      pure ([project], WalkContinue)

mkProject :: LeiningenProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "leiningen",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runExecIO $ getDeps project,
      projectPath = leinDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has Diagnostics sig m) => LeiningenProject -> m (Graphing Dependency)
getDeps = analyze . leinProjectClj

data LeiningenProject = LeiningenProject
  { leinDir :: Path Abs Dir
  , leinProjectClj :: Path Abs File
  } deriving (Eq, Ord, Show)

analyze :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze file = do
  stdoutBL <- execThrow (parent file) leinDepsCmd
  let stdoutTL = decodeUtf8 stdoutBL
      stdout = TL.toStrict stdoutTL

  case EDN.decodeText "lein deps :tree-data" stdout of
    Left err -> fatal (CommandParseError leinDepsCmd (T.pack err))
    Right deps -> pure (buildGraph deps)

-- node type for our LabeledGrapher
data ClojureNode = ClojureNode
  { nodeName :: Text,
    nodeVersion :: Text
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
topLevelNodes = map toClojureNode . M.keys . depsTree

-- recursively build edges and add labels to dependencies we encounter
buildEdges :: Has (LabeledGrapher ClojureNode ClojureLabel) sig m => Deps -> m ()
buildEdges deps = M.traverseWithKey single (depsTree deps) $> ()
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
toClojureNode dep = ClojureNode (T.replace "/" ":" (depName dep)) (depVersion dep)

toDependency :: ClojureNode -> Set ClojureLabel -> Dependency
toDependency node = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = MavenType,
          dependencyName = nodeName node,
          dependencyVersion = Just (CEq (nodeVersion node)),
          dependencyLocations = [],
          dependencyEnvironments = [],
          dependencyTags = M.empty
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
ednVecToMap = go M.empty
  where
    go :: EDN.EDNMap -> EDN.EDNVec -> Parser EDN.EDNMap
    go m vec
      | V.null vec = pure m
      | otherwise = do
        key <- EDN.vecGet 0 vec
        value <- EDN.vecGet 1 vec
        go (M.insert key value m) (V.drop 2 vec)

-- | The FromEDN type for lein deps output
newtype Deps = Deps
  { depsTree :: Map ClojureDep (Maybe Deps)
  }
  deriving (Eq, Ord, Show)

-- | A single dependency in the lein deps output
data ClojureDep = ClojureDep
  { depName :: Text,
    depVersion :: Text,
    depScope :: Maybe Text
  }
  deriving (Eq, Ord, Show)
