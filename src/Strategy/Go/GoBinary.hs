-- | Discover compiled Go binaries on the filesystem and read the module list
-- from the buildinfo the Go toolchain embeds in them (the data
-- @go version -m@ prints).
--
-- This covers Go code shipped as a binary with no manifest alongside it:
-- a gomobile @.so@ inside an AAR, a vendored CLI, a binary nested in a JAR.
-- Manifest-based analysis ('Strategy.Gomodules') cannot see those, because
-- there is no @go.mod@ to find.
--
-- Opt-in via @--enable-go-binary-analysis@: @fossa analyze@ otherwise reports
-- only what package managers declare, and reading binaries would add
-- dependencies to existing projects without the user asking for them.
--
-- Because this is a normal discovery strategy, it inherits path filters and,
-- under @--unpack-archives@, runs again over extracted archive contents - which
-- is what reaches binaries nested inside AARs and JARs. The two flags are
-- independent: archives need both.
--
-- The buildinfo parsing itself lives in millhone (Rust), shared with the
-- container analysis path; this module shells out to it and converts the
-- result into a dependency graph.
module Strategy.Go.GoBinary (
  discoverVisitor,
  finalizeProjects,
  toProjects,
  mkProject,
  getDeps,
  GoBinaryProject (..),
  GoModule (..),
  DiscoveredGoBinary (..),
  goBinaryDependencies,
  goModuleToDependency,
  normalizeVersion,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject, analyzeProjectStaticOnly))
import App.Fossa.Config.Analyze (StrategyConfig (enableGoBinaryAnalysis))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withMillhoneBinary)
import Control.Effect.Diagnostics (Diagnostics, context, warnThenRecover)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Control.Monad (filterM)
import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:), (.:?))
import Data.List (nub, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.SemVer qualified as SemVer
import Data.SemVer.Internal (Version (..))
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepType (GoType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Discovery.Walk (WalkStep (WalkContinue))
import Effect.Exec (AllowErr (Never), Command (..), Exec, Has, execJson')
import Effect.ReadFS (ReadFS, contentIsBinary)
import GHC.Generics (Generic)
import Graphing qualified
import Path (Abs, Dir, File, Path, parent)
import Strategy.Go.Gomod (PackageVersion (..), parsePackageVersion)
import Text.Megaparsec (parseMaybe)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (GoBinaryProjectType),
  GraphBreadth (Complete),
 )

-- | A Go module (path + version) parsed from a binary's embedded buildinfo.
data GoModule = GoModule
  { goModulePath :: Text
  , goModuleVersion :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON GoModule where
  parseJSON = withObject "GoModule" $ \o ->
    GoModule
      <$> o .: "path"
      <*> o .: "version"

-- | A Go binary millhone found, with the module list parsed from its embedded
-- buildinfo. Shared with the container analysis path, which discovers the same
-- shape inside image layers.
data DiscoveredGoBinary = DiscoveredGoBinary
  { goBinaryPath :: Text
  , goBinaryGoVersion :: Text
  , goBinaryMainModule :: Maybe GoModule
  , goBinaryModules :: [GoModule]
  }
  deriving (Eq, Ord, Show)

instance FromJSON DiscoveredGoBinary where
  parseJSON = withObject "DiscoveredGoBinary" $ \o ->
    DiscoveredGoBinary
      <$> o .: "path"
      <*> o .: "go_version"
      <*> o .:? "main_module"
      <*> o .: "modules"

-- | The Go binaries found in one directory, with their dependencies already
-- read at discovery time. Analysis is therefore pure: millhone is invoked once
-- per scanned directory tree, not once per binary.
--
-- A project is a directory rather than a single binary because a source unit
-- is named after its directory ('Srclib.Converter.toSourceUnit'); one project
-- per binary would emit units that collide whenever a directory holds more
-- than one Go binary. Each contributing binary stays visible as an origin path.
data GoBinaryProject = GoBinaryProject
  { goBinaryProjectDir :: Path Abs Dir
  , goBinaryProjectFiles :: [Path Abs File]
  , goBinaryProjectDeps :: [Dependency]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GoBinaryProject

instance AnalyzeProject GoBinaryProject where
  analyzeProject _ = getDeps

  -- Reading bytes already on disk; no build tool is invoked.
  analyzeProjectStaticOnly _ = getDeps

-- | Per-directory collector for the shared discovery walk: candidate binaries
-- are pre-filtered by 'contentIsBinary' (a cheap prefix read); millhone applies
-- the precise magic/size checks and the buildinfo parse.
discoverVisitor :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m ([Path Abs File], WalkStep)
discoverVisitor _ _ files = do
  binaries <- filterM contentIsBinary files
  pure (binaries, WalkContinue)

-- | Batch phase for the shared discovery walk: hand the whole collected
-- candidate batch to millhone in a single invocation and match its results
-- back to the candidate paths.
finalizeProjects ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has (Reader StrategyConfig) sig m
  ) =>
  Path Abs Dir ->
  -- | candidate binaries collected by 'discoverVisitor'
  m [Path Abs File] ->
  m [DiscoveredProject GoBinaryProject]
finalizeProjects dir collect = do
  enabled <- enableGoBinaryAnalysis <$> ask
  if not enabled
    then pure []
    else do
      candidates <- collect
      if null candidates
        then pure []
        else do
          -- A failure here (millhone missing, unparseable output) should not
          -- sink the whole scan, but it must not pass silently either: the
          -- user asked for this analysis and would otherwise get zero Go
          -- dependencies with no explanation.
          discovered <-
            warnThenRecover @Text "Error reading Go buildinfo (millhone)" $
              context "Reading Go buildinfo" $
                analyzeGoBinaries dir candidates
          pure . map mkProject . toProjects candidates $ concat discovered

-- | Match millhone's results back to the paths we handed it and group them by
-- containing directory, dropping any binary whose buildinfo yields no usable
-- dependency (for example a binary whose only module is the unversioned
-- @(devel)@ main module).
toProjects :: [Path Abs File] -> [DiscoveredGoBinary] -> [GoBinaryProject]
toProjects candidates discovered = map toProject . Map.toAscList $ Map.fromListWith merge byDir
  where
    byPath = Map.fromList $ map (\path -> (toText path, path)) candidates

    byDir = mapMaybe toDir discovered

    toDir binary = do
      path <- Map.lookup (goBinaryPath binary) byPath
      let deps = goBinaryDependencies binary
      if null deps
        then Nothing
        else Just (parent path, (NE.singleton path, deps))

    -- 'Map.fromListWith' applies the later entry first; flip so paths and
    -- dependencies stay in the order the binaries were discovered.
    merge newer older = older <> newer

    toProject (dir, (paths, deps)) =
      GoBinaryProject
        { goBinaryProjectDir = dir
        , goBinaryProjectFiles = sortOn toText $ NE.toList paths
        , goBinaryProjectDeps = nub deps
        }

mkProject :: GoBinaryProject -> DiscoveredProject GoBinaryProject
mkProject project =
  DiscoveredProject
    { projectType = GoBinaryProjectType
    , projectBuildTargets = mempty
    , projectPath = goBinaryProjectDir project
    , projectData = project
    }

getDeps :: (Applicative m) => GoBinaryProject -> m DependencyResults
getDeps project =
  pure
    DependencyResults
      { dependencyGraph = Graphing.directs $ goBinaryProjectDeps project
      , -- Buildinfo records every module linked into the binary, but carries no
        -- edges between them, so the graph is a flat complete set.
        dependencyGraphBreadth = Complete
      , dependencyManifestFiles = goBinaryProjectFiles project
      }

-- | Every usable dependency in a binary's buildinfo.
--
-- The main module is normally versioned @(devel)@ and dropped, but binaries
-- built via @go install module\@version@ carry a real version.
goBinaryDependencies :: DiscoveredGoBinary -> [Dependency]
goBinaryDependencies binary =
  nub . mapMaybe goModuleToDependency $
    goBinaryModules binary <> maybeToList (goBinaryMainModule binary)

goModuleToDependency :: GoModule -> Maybe Dependency
goModuleToDependency (GoModule path version) = do
  normalized <- normalizeVersion version
  Just
    Dependency
      { dependencyType = GoType
      , dependencyName = path
      , dependencyVersion = Just $ CEq normalized
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = mempty
      }

-- | Normalize a buildinfo version the same way go.mod analysis does
-- ('Strategy.Go.GoListPackages.toVerConstraint'): pseudo-versions become their
-- commit hash, semantic versions keep their "v" prefix. Unusable versions
-- (empty, @(devel)@) yield 'Nothing'.
normalizeVersion :: Text -> Maybe Text
normalizeVersion version =
  if Text.null version || version == "(devel)"
    then Nothing
    else case parseMaybe (parsePackageVersion id) version of
      Just (Pseudo commitHash) -> Just commitHash
      Just (Semantic semver) -> Just ("v" <> SemVer.toText semver{_versionMeta = []})
      Just (NonCanonical v) -> Just v
      Nothing -> Just version

-- | Millhone reads the candidate paths from stdin, one per line: a large
-- repository can have more candidates than the platform argument limit allows.
analyzeGoBinaries ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  [Path Abs File] ->
  m [DiscoveredGoBinary]
analyzeGoBinaries dir candidates = withMillhoneBinary $ \binaryPaths ->
  execJson' dir (millhoneGoBinaryCmd binaryPaths) stdin
  where
    stdin = Text.unlines $ map toText candidates

millhoneGoBinaryCmd :: BinaryPaths -> Command
millhoneGoBinaryCmd binaryPaths =
  Command
    { cmdName = toText . toPath $ binaryPaths
    , cmdArgs = ["--log-to", "stderr", "analyze-go-binaries"]
    , cmdAllowErr = Never
    , cmdEnvVars = Map.empty
    }
