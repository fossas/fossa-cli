module App.Fossa.Reachability.Maven (
  mavenJarCallGraph,
  mavenJarCallGraph',
  getJarsByBuild,
) where

import App.Fossa.Reachability.Jar (callGraphFromJar, isValidJar)
import App.Fossa.Reachability.Types (CallGraphAnalysis (..))
import Control.Carrier.Lift (Lift)
import Control.Effect.Diagnostics (Diagnostics, context, fromEither, recover)
import Control.Monad (join)
import Control.Monad.List (filterM)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (ToText (toText))
import Data.Text (Text, replace)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug, pretty)
import Effect.ReadFS (Has, ReadFS, resolveDir', resolveFile)
import Path (Abs, Dir, File, Path, parent)
import Strategy.Maven.Pom.Closure (MavenProjectClosure (..), findProjects)
import Strategy.Maven.Pom.PomFile (
  MavenCoordinate (coordArtifact, coordGroup, coordVersion),
  Pom (pomBuilds, pomCoord),
  PomBuild (PomBuild),
 )
import Text.Pretty.Simple (pShow)

-- | Discovers the JAR files associated with the project at the provided path,
-- then returns the parsed results of analyzing these JARs.
mavenJarCallGraph ::
  ( Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  m CallGraphAnalysis
mavenJarCallGraph dir = context ("build call graph for " <> toText dir) $ do
  jars <- getJarsByBuild dir
  logDebug . pretty $ "found jars: " ++ show jars
  mavenJarCallGraph' jars

-- | Like @mavenJarCallGraph@, but used when the list of JARs to parse is already available.
mavenJarCallGraph' ::
  ( Has Logger sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) =>
  [Path Abs File] ->
  m CallGraphAnalysis
mavenJarCallGraph' jars = context ("build call graph from " <> toText (show jars)) $ do
  parsedJars <- traverse callGraphFromJar jars
  pure $ JarAnalysis (catMaybes parsedJars)

getJarsByBuild ::
  ( Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m [Path Abs File]
getJarsByBuild dir = do
  mvnProjectClosures <- findProjects dir
  let pomPathsAndPom = concatMap (Map.elems . closurePoms) mvnProjectClosures

  candidateJars <- catMaybes <$> traverse getJarPathFromPom pomPathsAndPom
  logDebug . pretty $ ("Found candidate jars: ") <> toText (pShow candidateJars)
  filterM isValidJar candidateJars

-- Gets the candidate Jar File from pom.xml's build configs, or using defaults
-- returns Nothing, if file cannot be found
getJarPathFromPom ::
  ( Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  (Path Abs File, Pom) ->
  m (Maybe (Path Abs File))
getJarPathFromPom (pomPath, pom) = do
  let pomDir = parent pomPath
  let group = coordGroup . pomCoord $ pom
  let artifact = coordArtifact . pomCoord $ pom

  let jarName = defaultJarName pom
  let pomBuild =
        fromMaybe
          (PomBuild Nothing Nothing)
          (join $ Map.lookup (group, artifact) $ pomBuilds pom)

  recover $ pomBuildToJar pomBuild jarName pomDir

-- Default directory to write jar is /target/ from
-- root of the the pom file.
-- ref: https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html
defaultJarDir :: Text
defaultJarDir = "target"

-- Default name of the jar file is artifact-version.jar
-- ref: https://maven.apache.org/pom.html#the-basebuild-element-set
defaultJarName :: Pom -> Text
defaultJarName pom =
  (coordArtifact . pomCoord $ pom)
    <> "-"
    <> (coordVersion . pomCoord $ pom)

-- Gets the build jar filepath by inferring to build configs.
-- in pom.xml if build.finalName and build.outputDirectory provided, use them as
-- as first choice, instead of default.
-- ref: https://maven.apache.org/pom.html#build
pomBuildToJar :: (Has ReadFS sig m, Has Diagnostics sig m) => PomBuild -> Text -> Path Abs Dir -> m (Path Abs File)
pomBuildToJar (PomBuild Nothing Nothing) jarName = resolveJar defaultJarDir jarName
pomBuildToJar (PomBuild (Just name) (Just dir)) _ = resolveJar dir name
pomBuildToJar (PomBuild (Just name) _) _ = resolveJar defaultJarDir name
pomBuildToJar (PomBuild _ (Just dir)) jarName = resolveJar dir jarName

-- Resolves jar path by inferring from directory, and filename of jar
resolveJar :: (Has ReadFS sig m, Has Diagnostics sig m) => Text -> Text -> Path Abs Dir -> m (Path Abs File)
resolveJar jarDir jarFile baseDir = do
  let jarDir' = replace "${project.basedir}" (toText baseDir) jarDir
  jarAbsDir <- fromEither =<< resolveDir' baseDir jarDir'
  resolveFile jarAbsDir (jarFile <> ".jar")
