{-# LANGUAGE QuasiQuotes #-}

module Analysis.PnpmSpec (spec) where

import Analysis.FixtureExpectationUtils (
  testSuiteHasSomeDepResults,
  withAnalysisOf,
 )
import Analysis.FixtureUtils (
  AnalysisTestFixture (AnalysisTestFixture),
  FixtureArtifact (FixtureArtifact),
  FixtureEnvironment (LocalEnvironment),
 )
import App.Types (Mode (NonStrict))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  Dependency (..),
  VerConstraint (CEq),
 )
import Graphing (vertexList)
import Path (reldir)
import Strategy.Node qualified as Node
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)
import Types (DependencyResults (..), DiscoveredProjectType (..))

elementPlus :: AnalysisTestFixture (Node.NodeProject)
elementPlus =
  AnalysisTestFixture
    "element-plus"
    Node.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/element-plus/element-plus/archive/refs/tags/2.0.0.tar.gz"
      [reldir|pnpm/element-plus/|]
      [reldir|element-plus-2.0.0/|]

-- | jotai-eager uses pnpm v9 catalogs (catalog: specifiers in package.json
-- resolved via the catalogs section in pnpm-lock.yaml).
jotaiEager :: AnalysisTestFixture (Node.NodeProject)
jotaiEager =
  AnalysisTestFixture
    "jotai-eager"
    Node.discover
    LocalEnvironment
    Nothing
    $ FixtureArtifact
      "https://github.com/jotaijs/jotai-eager/archive/refs/tags/v0.2.4.tar.gz"
      [reldir|pnpm/jotai-eager/|]
      [reldir|jotai-eager-0.2.4/|]

-- | Collect all dependency vertices from all discovered projects' graphs.
allDeps :: [(a, DependencyResults)] -> [Dependency]
allDeps = concatMap (vertexList . dependencyGraph . snd)

-- | Extract raw version strings from all dependencies.
allVersionStrings :: [Dependency] -> [Text]
allVersionStrings = mapMaybe (fmap verText . dependencyVersion)
  where
    verText (CEq t) = t
    verText _ = ""

testJotaiEagerCatalogs :: Spec
testJotaiEagerCatalogs =
  aroundAll (withAnalysisOf NonStrict jotaiEager) $ do
    describe "jotai-eager (pnpm catalogs)" $ do
      it "should find targets" $ \(result, _) ->
        length result `shouldSatisfy` (> 0)

      -- If catalog resolution fails, dependencies would appear with raw
      -- "catalog:" or "catalog:<name>" strings instead of actual versions.
      -- Assert on the absence of these rather than specific versions so the
      -- test stays stable as the upstream fixture's deps evolve.
      it "should not contain any unresolved catalog: version strings" $ \(result, _) ->
        filter (Text.isPrefixOf "catalog:") (allVersionStrings (allDeps result)) `shouldBe` []

      -- Guard against a regression where catalog-backed deps are silently
      -- dropped entirely — in that case the "no unresolved catalog:" check
      -- would pass trivially. A real pnpm v9 project has many transitive
      -- deps; a near-empty graph means resolution failed.
      it "should discover a non-trivial dependency graph" $ \(result, _) ->
        length (allDeps result) `shouldSatisfy` (> 10)

spec :: Spec
spec = do
  testSuiteHasSomeDepResults elementPlus PnpmProjectType
  testJotaiEagerCatalogs
