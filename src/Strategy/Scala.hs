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

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics (
  Diagnostics,
  errCtx,
  fatalText,
  fromMaybeText,
  recover,
  warnOnErr,
  (<||>),
 )
import Control.Effect.Reader (Reader)
import Control.Effect.Stack (context)
import Control.Monad (when)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ConvertUtf8 (decodeUtf8), toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Diag.Common (MissingDeepDeps (MissingDeepDeps))
import Discovery.Filters (AllFilters)
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
import Effect.Logger (Logger, logDebug, logInfo, viaShow)
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
import Strategy.Scala.Errors (FailedToListProjects (FailedToListProjects), MaybeWithoutDependencyTreeTask (MaybeWithoutDependencyTreeTask))
import Strategy.Scala.SbtDependencyTree (SbtArtifact (SbtArtifact), analyze, sbtDepTreeCmd)
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
  , Has Logger sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject ScalaProject]
discover = simpleDiscover findProjects' mkProject ScalaProjectType
  where
    findProjects' dir = concatMap toScalaProjects <$> (findProjects dir)

    toScalaProjects :: SbtTargets -> [ScalaProject]
    toScalaProjects (SbtTargets maybeSbtDepTree closure) = ScalaProject maybeSbtDepTree <$> closure

data ScalaProject = ScalaProject
  { rawSbtDepTree :: Maybe ByteString
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

mkProject :: ScalaProject -> DiscoveredProject ScalaProject
mkProject (ScalaProject sbtBuildDir closure) =
  DiscoveredProject
    { projectType = ScalaProjectType
    , projectPath = parent $ closurePath closure
    , projectBuildTargets = mempty
    , projectData = ScalaProject sbtBuildDir closure
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => ScalaProject -> m DependencyResults
getDeps project =
  case (rawSbtDepTree project) of
    Nothing -> analyzeWithPoms project
    _ -> warnOnErr MissingDeepDeps (analyzeWithSbtDepTree project) <||> analyzeWithPoms project

pathToText :: Path ar fd -> Text
pathToText = toText . toFilePath

data SbtTargets = SbtTargets (Maybe ByteString) [MavenProjectClosure]

findProjects ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
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

      rawSbtDepTreeStdout <-
        recover $
          context ("inferring dependencies") $
            errCtx (MaybeWithoutDependencyTreeTask) $
              execThrow dir sbtDepTreeCmd

      case projectsRes of
        Nothing -> pure ([], WalkSkipAll)
        Just projects -> do
          -- In Sbt there is an ongoing defect when sbt renders multi-project build
          -- Refer to: https://github.com/sbt/sbt/issues/6905
          --
          -- We intentionally avoid faulty tactic if multi-project build is identified!
          when (length projects > 1) $
            logInfo "Discovered multi-project sbt build, only direct dependencies will be reported!"
          let safeRawSbtDepTreeStdout = if length projects > 1 then Nothing else rawSbtDepTreeStdout

          pure ([SbtTargets safeRawSbtDepTreeStdout projects], WalkSkipAll)

analyzeWithPoms :: (Has Diagnostics sig m) => ScalaProject -> m DependencyResults
analyzeWithPoms (ScalaProject _ closure) = context "Analyzing sbt dependencies with generated pom" $ do
  pure $
    DependencyResults
      { dependencyGraph = Pom.analyze' closure
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [closurePath closure]
      }

analyzeWithSbtDepTree :: (Has Exec sig m, Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m) => ScalaProject -> m DependencyResults
analyzeWithSbtDepTree (ScalaProject maybeDepTree closure) = context "Analyzing sbt dependencies using dependencyTree" $ do
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
