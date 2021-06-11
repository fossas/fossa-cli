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

import Control.Carrier.Diagnostics
import Data.Maybe (mapMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Discovery.Walk
import Effect.Exec
import Effect.Logger hiding (group)
import Effect.ReadFS
import Path
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure, buildProjectClosures)
import Strategy.Maven.Pom.Closure qualified as PomClosure
import Strategy.Maven.Pom.Resolver (buildGlobalClosure)
import Types

discover ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  , Applicative run
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject run]
discover dir = context "Scala" $ do
  projects <- findProjects dir
  pure (map (mkProject dir) projects)

mkProject ::
  Applicative n =>
  -- | basedir; required for licenses
  Path Abs Dir ->
  MavenProjectClosure ->
  DiscoveredProject n
mkProject basedir closure =
  DiscoveredProject
    { projectType = "scala"
    , projectPath = parent $ PomClosure.closurePath closure
    , projectBuildTargets = mempty
    , -- only do static analysis of generated pom files
      projectDependencyGraph = \_ -> pure (Pom.analyze' closure)
    , projectLicenses = pure $ Pom.getLicenses basedir closure
    }

pathToText :: Path ar fd -> Text
pathToText = T.pack . toFilePath

findProjects :: (Has Exec sig m, Has ReadFS sig m, Has Logger sig m, Has Diagnostics sig m) => Path Abs Dir -> m [MavenProjectClosure]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "build.sbt" files of
    Nothing -> pure ([], WalkContinue)
    Just _ -> do
      projectsRes <-
        errorBoundary
          . context ("Listing sbt projects at " <> pathToText dir)
          $ genPoms dir

      case projectsRes of
        Left err -> do
          logWarn $ renderFailureBundle err
          pure ([], WalkSkipAll)
        Right projects -> pure (projects, WalkSkipAll)

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
      stdoutLines = T.lines stdout
      --
      pomLines :: [Text]
      pomLines = mapMaybe (T.stripPrefix "[info] Wrote ") stdoutLines
      --
      pomLocations :: Maybe [Path Abs File]
      pomLocations = traverse (parseAbsFile . T.unpack) pomLines

  case pomLocations of
    Nothing -> fatalText ("Could not parse pom paths from:\n" <> T.unlines pomLines)
    Just [] -> fatalText "No sbt projects found"
    Just paths -> do
      globalClosure <- buildGlobalClosure paths

      pure $ buildProjectClosures projectDir globalClosure
