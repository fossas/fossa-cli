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
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Carrier.Diagnostics
import Data.Aeson (ToJSON)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import GHC.Generics (Generic)
import Path
import Prettyprinter (viaShow)
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure, buildProjectClosures)
import Strategy.Maven.Pom.Closure qualified as PomClosure
import Strategy.Maven.Pom.Resolver (buildGlobalClosure)
import Types

discover ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject ScalaProject]
discover dir = context "Scala" $ do
  projects <- findProjects dir
  pure (map mkProject projects)

newtype ScalaProject = ScalaProject {unScalaProject :: PomClosure.MavenProjectClosure}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ScalaProject

instance AnalyzeProject ScalaProject where
  analyzeProject _ = pure . getDeps

mkProject :: MavenProjectClosure -> DiscoveredProject ScalaProject
mkProject closure =
  DiscoveredProject
    { projectType = ScalaProjectType
    , projectPath = parent $ PomClosure.closurePath closure
    , projectBuildTargets = mempty
    , projectData = ScalaProject closure
    }

-- only do static analysis of generated pom files
getDeps :: ScalaProject -> DependencyResults
getDeps (ScalaProject closure) =
  DependencyResults
    { dependencyGraph = Pom.analyze' closure
    , dependencyGraphBreadth = Complete
    , dependencyManifestFiles = [PomClosure.closurePath closure]
    }

pathToText :: Path ar fd -> Text
pathToText = toText . toFilePath

findProjects :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [MavenProjectClosure]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "build.sbt" files of
    Nothing -> pure ([], WalkContinue)
    Just _ -> do
      projectsRes <-
        recover
          . warnOnErr (FailedToListProjects dir)
          . context ("Listing sbt projects at " <> pathToText dir)
          $ genPoms dir

      case projectsRes of
        Nothing -> pure ([], WalkSkipAll)
        Just projects -> pure (projects, WalkSkipAll)

newtype FailedToListProjects = FailedToListProjects (Path Abs Dir)
  deriving (Eq, Ord, Show)

instance ToDiagnostic FailedToListProjects where
  renderDiagnostic (FailedToListProjects dir) = "Failed to discover and analyze sbt projects, for sbt build manifest at:" <> viaShow dir

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
