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

-- | Check that a dependency with the given name and exact version exists in the list.
hasDep :: Text -> Text -> [Dependency] -> Bool
hasDep name version =
  any (\d -> dependencyName d == name && dependencyVersion d == Just (CEq version))

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

      -- These four dependencies use catalog: specifiers in jotai-eager's
      -- workspace packages. If catalog resolution fails, they would appear
      -- with a raw "catalog:" version string or be missing entirely.
      it "should resolve catalog:default @types/node to 22.15.19" $ \(result, _) ->
        allDeps result `shouldSatisfy` hasDep "@types/node" "22.15.19"

      it "should resolve catalog:default typescript to 5.9.3" $ \(result, _) ->
        allDeps result `shouldSatisfy` hasDep "typescript" "5.9.3"

      it "should resolve catalog:default vite to 7.3.1" $ \(result, _) ->
        allDeps result `shouldSatisfy` hasDep "vite" "7.3.1"

      it "should resolve catalog:default vitest to 4.0.16" $ \(result, _) ->
        allDeps result `shouldSatisfy` hasDep "vitest" "4.0.16"

      it "should not contain any unresolved catalog: version strings" $ \(result, _) ->
        filter (Text.isPrefixOf "catalog:") (allVersionStrings (allDeps result)) `shouldBe` []

spec :: Spec
spec = do
  testSuiteHasSomeDepResults elementPlus PnpmProjectType
  testJotaiEagerCatalogs
