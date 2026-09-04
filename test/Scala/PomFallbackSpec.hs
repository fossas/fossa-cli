{-# LANGUAGE QuasiQuotes #-}

-- | Pins the graph shape of Scala's sbt-generated-pom fallback
-- (Strategy.Scala.analyzeWithPoms), the third resort in Scala's getDeps
-- chain after the dependency-tree JSON and dependencyTree tactics.
--
-- Without additional processing, 'Pom.analyze' marks the project's own
-- artifact as the sole direct node and demotes declared dependencies to
-- transitive. This spec feeds the fallback a checked-in copy of a real
-- `sbt makePom` artifact (the fixture under
-- test/Scala/testdata/pom-fallback-generated) and asserts that declared
-- dependencies appear as Direct and the project artifact is removed.
--
-- Note the input is the pom that sbt generates for the test project in
-- test/Scala/testdata/pom-fallback-repro; that project exercises the same
-- path end-to-end when fossa analyzes it (its MiniDependencyTreePlugin is
-- disabled on purpose).
module Scala.PomFallbackSpec (spec) where

import Control.Effect.Lift (sendIO)
import Data.Text (Text)
import DepTypes (DepType (MavenType), Dependency (..), VerConstraint (CEq))
import GraphUtil (expectDeps', expectDirect')
import Path (reldir, relfile, (</>))
import Path.IO qualified as PIO
import Strategy.Maven.Common (MavenDependency (..), mavenDependencyToDependency)
import Strategy.Maven.Pom.Closure (MavenProjectClosure, buildProjectClosures)
import Strategy.Maven.Pom.Resolver (buildGlobalClosure)
import Strategy.Scala (ScalaProject (..), analyzeWithPoms)
import Test.Effect (EffectStack, expectationFailure', it', shouldBe')
import Test.Hspec (Spec, describe)
import Types (DependencyResults (..), GraphBreadth (Partial))

spec :: Spec
spec = describe "Scala pom fallback analysis" $
  describe "analyzeWithPoms" $
    it' "reports declared dependencies as direct and removes the project artifact" $
      withGeneratedClosure $ \closure -> do
        results <- analyzeWithPoms (ScalaProject Nothing Nothing closure)
        dependencyGraphBreadth results `shouldBe'` Partial
        expectDirect' [scalaLibraryDependency, catsCoreDependency] (dependencyGraph results)
        expectDeps' [scalaLibraryDependency, catsCoreDependency] (dependencyGraph results)

-- | Rebuild the project closure that `genPoms` would produce for the checked-in
-- makePom artifact, and pass its sole closure to the test.
withGeneratedClosure :: (MavenProjectClosure -> EffectStack ()) -> EffectStack ()
withGeneratedClosure act = do
  cwd <- sendIO PIO.getCurrentDir
  let fixtureDir = cwd </> [reldir|test/Scala/testdata/pom-fallback-generated|]
      pomPath = fixtureDir </> [relfile|pom-fallback-repro_2.13-0.1.0.pom|]
  global <- buildGlobalClosure [pomPath]
  closures <- buildProjectClosures fixtureDir global
  case closures of
    [closure] -> act closure
    _ -> expectationFailure' $ "expected exactly one generated-pom closure, got " <> show (length closures)

scalaLibraryDependency :: Dependency
scalaLibraryDependency =
  mavenDependencyToDependency $ MavenDependency (mkDependency "org.scala-lang:scala-library" "2.13.14") mempty mempty

catsCoreDependency :: Dependency
catsCoreDependency =
  mavenDependencyToDependency $ MavenDependency (mkDependency "org.typelevel:cats-core_2.13" "2.10.0") mempty mempty

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
