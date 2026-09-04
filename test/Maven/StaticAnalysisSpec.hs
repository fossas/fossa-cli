{-# LANGUAGE QuasiQuotes #-}

-- | Pins the graph shape of the Maven static ("pomxml") analysis path.
--
-- 'Pom.analyze'' marks the project's own coordinate as the sole direct node,
-- so without further processing every declared dependency ends up one level
-- down as transitive. Historically only the dynamic path applied 'shrinkRoots'
-- to fix this; the static path returned 'Pom.analyze'' as-is, reporting the
-- project artifact as the only Direct dependency. These specs pin both halves:
-- the raw 'Pom.analyze'' invariant, and the corrected end-to-end static output
-- (single-module and multi-module, matching the dynamic path: first-party
-- artifacts removed, declared dependencies promoted to Direct).
module Maven.StaticAnalysisSpec (spec) where

import Control.Carrier.Reader (runReader)
import Control.Effect.Lift (sendIO)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import DepTypes (DepType (MavenType), Dependency (..), VerConstraint (CEq))
import Discovery.Filters (MavenScopeFilters (..))
import GraphUtil (expectDeps', expectDirect')
import Path (Dir, Path, Rel, reldir, (</>))
import Path.IO qualified as PIO
import Strategy.Maven (MavenProject (..), getDepsStatically)
import Strategy.Maven.Common (MavenDependency (..), mavenDependencyToDependency)
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure, closureSubmodules, findProjects)
import Test.Effect (EffectStack, expectationFailure', it', shouldBe')
import Test.Hspec (Spec, describe)
import Types (BuildTarget (..), DependencyResults (..), FoundTargets (..), GraphBreadth (Partial))

spec :: Spec
spec = describe "Maven static analysis" $ do
  describe "Pom.analyze'" $
    it' "marks only the project's own coordinate as direct" $
      -- this is why the strategy layer must shrink roots before uploading
      withFixtureClosure singleModuleFixture $ \closure ->
        expectDirect' [rootPackage] (Pom.analyze' closure)

  describe "getDepsStatically" $ do
    it' "reports declared dependencies as direct and removes the project artifact" $
      withFixtureClosure singleModuleFixture $ \closure -> do
        results <- staticallyAnalyze closure
        dependencyGraphBreadth results `shouldBe'` Partial
        expectDirect' [junitDependency] (dependencyGraph results)
        expectDeps' [junitDependency] (dependencyGraph results)

    it' "promotes submodule-declared dependencies to direct and removes all first-party artifacts" $
      withFixtureClosure multiModuleFixture $ \closure -> do
        results <- staticallyAnalyze closure
        let expected = [commonsLang3Dependency, junitDependency]
        expectDirect' expected (dependencyGraph results)
        expectDeps' expected (dependencyGraph results)

-- | Load a checked-in fixture and pass its sole project closure to the test,
-- failing with a readable message otherwise.
withFixtureClosure :: Path Rel Dir -> (MavenProjectClosure -> EffectStack ()) -> EffectStack ()
withFixtureClosure fixture act = do
  cwd <- sendIO PIO.getCurrentDir
  closures <- findProjects (cwd </> fixture)
  case closures of
    [closure] -> act closure
    _ -> expectationFailure' $ "expected exactly one Maven project closure, got " <> show (length closures)

staticallyAnalyze :: MavenProjectClosure -> EffectStack DependencyResults
staticallyAnalyze closure =
  runReader (MavenScopeIncludeFilters mempty)
    . getDepsStatically (allTargets closure)
    $ MavenProject closure

-- | Mirror the target set mkProject derives from discovery, so submodule
-- filtering exercises the same code path as a production analysis.
allTargets :: MavenProjectClosure -> FoundTargets
allTargets closure =
  maybe ProjectWithoutTargets FoundTargets
    . NESet.nonEmpty
    . Set.map BuildTarget
    $ closureSubmodules closure

singleModuleFixture :: Path Rel Dir
singleModuleFixture = [reldir|test/Maven/testdata/static-root-repro|]

multiModuleFixture :: Path Rel Dir
multiModuleFixture = [reldir|test/Maven/testdata/static-multimodule-repro|]

rootPackage :: MavenDependency
rootPackage = MavenDependency projectArtifact mempty mempty

projectArtifact :: Dependency
projectArtifact = mkDependency "com.example:app" "1.0.0"

junitDependency :: Dependency
junitDependency = mavenDependencyToDependency $ MavenDependency junitArtifact mempty mempty

commonsLang3Dependency :: Dependency
commonsLang3Dependency = mavenDependencyToDependency $ MavenDependency commonsLang3Artifact mempty mempty

junitArtifact :: Dependency
junitArtifact = mkDependency "junit:junit" "4.13.2"

commonsLang3Artifact :: Dependency
commonsLang3Artifact = mkDependency "org.apache.commons:commons-lang3" "3.14.0"

mkDependency :: Text -> Text -> Dependency
mkDependency name version =
  Dependency
    { dependencyType = MavenType
    , dependencyName = name
    , dependencyVersion = Just (CEq version)
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }
