-- | The scala strategy leverages the machinery from maven-pom.
--
-- Sbt has a command to export pom files, with one caveat -- in multi-project
-- setups, parent/child relationships are not present in the generated poms.
--
-- The only non-trivial logic that exists in this strategy is adding edges
-- between poms in the maven "global closure", before building the individual
-- multi-project closures.
module Strategy.Scala (
  discover,
  findProjects,
  ScalaProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import Control.Effect.Diagnostics (
  Diagnostics,
  errCtx,
  fatalText,
  fromMaybeText,
  recover,
  warnOnErr,
  (<||>),
 )
import Control.Effect.Reader (Reader, asks)
import Control.Effect.Stack (context)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.String.Conversion (ConvertUtf8 (decodeUtf8), toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Diag.Common (MissingDeepDeps (MissingDeepDeps))
import Discovery.Filters (AllFilters, FilterSet (scopes), MavenScopeFilters (excludeScope, includeScope))
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  Has,
  execThrow,
 )
import Effect.Logger (Logger, logDebug, viaShow)
import Effect.ReadFS (ReadFS, readContentsXML)
import GHC.Generics (Generic)
import Path (
  Abs,
  Dir,
  File,
  Path,
  parent,
  parseAbsFile,
  toFilePath,
 )
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure, buildProjectClosures, closurePath)
import Strategy.Maven.Pom.PomFile (RawPom (rawPomArtifact, rawPomGroup, rawPomVersion))
import Strategy.Maven.Pom.Resolver (buildGlobalClosure)
import Strategy.Scala.Errors (FailedToListProjects (FailedToListProjects), MaybeWithoutDependencyTreeTask (MaybeWithoutDependencyTreeTask), MissingFullDependencyPlugin (MissingFullDependencyPlugin))
import Strategy.Scala.Plugin (genTreeJson, hasDependencyPlugins)
import Strategy.Scala.SbtDependencyTree (SbtArtifact (SbtArtifact), analyze, sbtDepTreeCmd)
import Strategy.Scala.SbtDependencyTreeJson qualified as TreeJson
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (ScalaProjectType),
  GraphBreadth (Complete, Partial),
 )

discover ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject ScalaProject]
discover = simpleDiscover findProjects' mkProject ScalaProjectType
  where
    findProjects' dir = concatMap toScalaProjects <$> (findProjects dir)

    toScalaProjects :: SbtTargets -> [ScalaProject]
    toScalaProjects (SbtTargets maybeSbtDepTree treeJsonPaths closure) =
      map
        (mkScalaProject (SbtTargets maybeSbtDepTree treeJsonPaths closure))
        closure

    mkScalaProject :: SbtTargets -> MavenProjectClosure -> ScalaProject
    mkScalaProject (SbtTargets maybeSbtDepTree treeJsonPaths _) cls =
      ScalaProject maybeSbtDepTree (findRelevantDependencyTreeJson cls treeJsonPaths) cls

    findRelevantDependencyTreeJson :: MavenProjectClosure -> [Path Abs File] -> Maybe (Path Abs File)
    findRelevantDependencyTreeJson closure paths = do
      let clsPath = parent $ closurePath closure
      -- treeJson are written in /module/target/
      -- where as makePom may write in /module/target/scala-version/ or /module/target/
      --
      -- match closure to treeJson based common parent path /module/target/
      -- module can only have one pom closure, and one or none tree json file.
      let matchingPaths = filter (\p -> parent p == clsPath || parent p == parent clsPath) paths
      listToMaybe matchingPaths

data ScalaProject = ScalaProject
  { rawSbtDepTree :: Maybe ByteString
  , unScalaProjectDepTreeJson :: Maybe (Path Abs File)
  , unScalaProject :: MavenProjectClosure
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ScalaProject where
  toJSON scalaProject =
    object
      [ "unScalaProject" .= unScalaProject scalaProject
      ]

instance AnalyzeProject ScalaProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = const $ fatalText "Cannot analyze scala project statically"

mkProject :: ScalaProject -> DiscoveredProject ScalaProject
mkProject (ScalaProject sbtBuildDir sbtTreeJson closure) =
  DiscoveredProject
    { projectType = ScalaProjectType
    , projectPath = parent $ closurePath closure
    , projectBuildTargets = mempty
    , projectData = ScalaProject sbtBuildDir sbtTreeJson closure
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m, Has (Reader MavenScopeFilters) sig m) => ScalaProject -> m DependencyResults
getDeps project =
  warnOnErr MissingDeepDeps (analyzeWithDepTreeJson project <||> analyzeWithSbtDepTree project)
    <||> analyzeWithPoms project

pathToText :: Path ar fd -> Text
pathToText = toText . toFilePath

data SbtTargets = SbtTargets (Maybe ByteString) [Path Abs File] [MavenProjectClosure]

findProjects ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [SbtTargets]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "build.sbt" files of
    Nothing -> pure ([], WalkContinue)
    Just _ -> do
      projectsRes <-
        recover
          . warnOnErr (FailedToListProjects dir)
          . context ("Listing sbt projects at " <> pathToText dir)
          $ genPoms dir

      (miniDepPlugin, depPlugin) <- hasDependencyPlugins dir
      case (projectsRes, miniDepPlugin, depPlugin) of
        (Nothing, _, _) -> pure ([], WalkSkipAll)
        (Just projects, False, False) -> pure ([SbtTargets Nothing [] projects], WalkSkipAll)
        (Just projects, _, True) -> do
          -- project is explicitly configured to use dependency-tree-plugin
          treeJSONs <- recover $ genTreeJson dir
          pure ([SbtTargets Nothing (fromMaybe [] treeJSONs) projects], WalkSkipAll)
        (Just projects, True, _) -> do
          -- project is using miniature dependency tree plugin,
          -- which is included by default with sbt 1.4+
          depTreeStdOut <-
            recover $
              context ("inferring dependencies") $
                errCtx (MaybeWithoutDependencyTreeTask) $
                  execThrow dir sbtDepTreeCmd

          case (length projects > 1, depTreeStdOut) of
            -- not emitting warning or error, to avoid duplication from
            -- those in `analyze` step. further analysis warn/errors are
            -- included in analysis scan summary.
            (True, _) -> pure ([SbtTargets Nothing [] projects], WalkSkipAll)
            (_, Just _) -> pure ([SbtTargets depTreeStdOut [] projects], WalkSkipAll)
            (_, _) -> pure ([], WalkSkipAll)

analyzeWithPoms :: (Has Diagnostics sig m, Has (Reader MavenScopeFilters) sig m) => ScalaProject -> m DependencyResults
analyzeWithPoms (ScalaProject _ _ closure) = context "Analyzing sbt dependencies with generated pom" $ do
  includeScopeFilters <- asks includeScope
  excludeScopeFilters <- asks excludeScope
  let includeScopeFilterSet = scopes includeScopeFilters
      excludeScopeFilterSet = scopes excludeScopeFilters
  pure $
    DependencyResults
      { dependencyGraph = Pom.analyze' includeScopeFilterSet excludeScopeFilterSet closure
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [closurePath closure]
      }

analyzeWithDepTreeJson :: (Has ReadFS sig m, Has Diagnostics sig m) => ScalaProject -> m DependencyResults
analyzeWithDepTreeJson (ScalaProject _ treeJson closure) = context "Analyzing sbt dependencies using dependencyBrowseTreeHTML" $ do
  treeJson' <- errCtx MissingFullDependencyPlugin $ fromMaybeText "Could not retrieve output from sbt dependencyBrowseTreeHTML" treeJson
  projectGraph <- TreeJson.analyze treeJson'
  pure $
    DependencyResults
      { dependencyGraph = projectGraph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [closurePath closure]
      }

analyzeWithSbtDepTree :: (Has Exec sig m, Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m) => ScalaProject -> m DependencyResults
analyzeWithSbtDepTree (ScalaProject maybeDepTree _ closure) = context "Analyzing sbt dependencies using dependencyTree" $ do
  projectArtifact <- pomSbtArtifact
  logDebug $ "identified artifact whose descendent to include when graphing: " <> viaShow projectArtifact

  projectGraph <- analyze maybeDepTree projectArtifact
  pure $
    DependencyResults
      { dependencyGraph = projectGraph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [closurePath closure]
      }
  where
    pomSbtArtifact :: (Has ReadFS sig m, Has Diagnostics sig m) => m SbtArtifact
    pomSbtArtifact = do
      let pomPath = closurePath closure
      maybeRawPom <- recover (readContentsXML @RawPom pomPath)
      groupId <- fromMaybeText ("Could not retrieve project group from generated pom file:" <> toText pomPath) (rawPomGroup =<< maybeRawPom)
      artifactId <- fromMaybeText ("Could not retrieve project artifact from generated pom file:" <> toText pomPath) (rawPomArtifact <$> maybeRawPom)
      version <- fromMaybeText ("Could not retrieve project version from generated pom file:" <> toText pomPath) (rawPomVersion =<< maybeRawPom)
      pure $ SbtArtifact groupId artifactId version

makePomCmd :: Command
makePomCmd =
  Command
    { cmdName = "sbt"
    , -- --no-colors to disable ANSI escape codes
      -- --batch to disable interactivity. normally, if an `sbt` command fails, it'll drop into repl mode: --batch will disable the repl.
      cmdArgs = ["--no-colors", "--batch", "makePom"]
    , cmdAllowErr = Never
    }

genPoms :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [MavenProjectClosure]
genPoms projectDir = do
  stdoutBL <- context "Generating poms" $ execThrow projectDir makePomCmd

  -- stdout for "sbt makePom" looks something like:
  --
  -- > ...
  -- > [info] Wrote /absolute/path/to/pom.xml
  -- > [info] Wrote /absolute/path/to/other/pom.xml
  -- > ...
  let stdoutLText = decodeUtf8 stdoutBL
      stdout = TL.toStrict stdoutLText
      --
      stdoutLines :: [Text]
      stdoutLines = Text.lines stdout
      --
      pomLines :: [Text]
      pomLines = mapMaybe (Text.stripPrefix "[info] Wrote ") stdoutLines
      --
      pomLocations :: Maybe [Path Abs File]
      pomLocations = traverse (parseAbsFile . toString) pomLines

  case pomLocations of
    Nothing -> fatalText ("Could not parse pom paths from:\n" <> Text.unlines pomLines)
    Just [] -> fatalText "No sbt projects found"
    Just paths -> do
      globalClosure <- buildGlobalClosure paths

      pure $ buildProjectClosures projectDir globalClosure
