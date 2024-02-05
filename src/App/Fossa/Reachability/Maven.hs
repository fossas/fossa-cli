module App.Fossa.Reachability.Maven (
  mavenJarCallGraph,
  isValidJar,
  getJarsByBuild,
) where

import App.Fossa.Reachability.Jar (callGraphFromJar)
import App.Fossa.Reachability.Types (CallGraphAnalysis (..))
import Control.Carrier.Lift (Lift)
import Control.Effect.Diagnostics (Diagnostics, fromEither, recover)
import Control.Monad (join)
import Control.Monad.List (filterM)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (ToText (toText))
import Data.Text (Text, isSuffixOf, replace)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug, pretty)
import Effect.ReadFS (Has, ReadFS, doesFileExist, resolveDir', resolveFile)
import Path (Abs, Dir, File, Path, fileExtension, parent, toFilePath)
import Strategy.Maven.Pom.Closure (MavenProjectClosure (..), findProjects)
import Strategy.Maven.Pom.PomFile (
  MavenCoordinate (coordArtifact, coordGroup, coordVersion),
  Pom (pomBuilds, pomCoord),
  PomBuild (PomBuild),
 )
import System.FilePath (takeFileName)
import Text.Pretty.Simple (pShow)

mavenJarCallGraph ::
  ( Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  m CallGraphAnalysis
mavenJarCallGraph dir = do
  jars <- getJarsByBuild dir
  logDebug . pretty $ "found jars: " ++ show jars

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

-- True if jar exist, and is not likely test jar, otherwise False
isValidJar :: (Has ReadFS sig m) => Path Abs File -> m Bool
isValidJar file = do
  exists <- doesFileExist file
  pure $
    exists
      && fileExtension file == Just ".jar"
      -- In maven builds, test jars have -test suffix by convention
      && not (isSuffixOf "-test.jar" $ toText . takeFileName . toFilePath $ file)

-- Resolves jar path by inferring from directory, and filename of jar
resolveJar :: (Has ReadFS sig m, Has Diagnostics sig m) => Text -> Text -> Path Abs Dir -> m (Path Abs File)
resolveJar jarDir jarFile baseDir = do
  let jarDir' = replace "${project.basedir}" (toText baseDir) jarDir
  jarAbsDir <- fromEither =<< resolveDir' baseDir jarDir'
  resolveFile jarAbsDir (jarFile <> ".jar")
