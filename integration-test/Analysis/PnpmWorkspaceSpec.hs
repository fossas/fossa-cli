{-# LANGUAGE TemplateHaskell #-}

-- | End-to-end coverage for target-level dependency scoping of pnpm workspaces:
-- discovery over a vendored workspace fixture, then analysis per selected build
-- target.
module Analysis.PnpmWorkspaceSpec (spec) where

import Analysis.FixtureUtils (FixtureEnvironment (LocalEnvironment), testRunner, withResult)
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject))
import App.Types (Mode (NonStrict))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Reader (runReader)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NonEmptySet
import Data.Text (Text)
import DepTypes (Dependency (dependencyName))
import Graphing (Graphing)
import Graphing qualified
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO qualified as PIO
import Test.Hspec (Spec, beforeAll, describe, it, shouldBe, shouldSatisfy)
import Types (
  BuildTarget (BuildTarget),
  DependencyResults (dependencyGraph),
  DiscoveredProject (projectBuildTargets, projectData, projectType),
  DiscoveredProjectType (PnpmProjectType),
  FoundTargets (FoundTargets, ProjectWithoutTargets),
 )

import Strategy.Node qualified as Node

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/Node/testdata/pnpm-workspaces/")

-- | The fixture's root package.json has no @name@ field, which is typical of a
-- pnpm workspace root since the workspace configuration lives in
-- pnpm-workspace.yaml. Its target name is therefore the root directory's own
-- basename.
allTargetNames :: [Text]
allTargetNames =
  [ "pnpm-workspaces"
  , "@fossa-test/browser"
  , "@fossa-test/server"
  , "@fossa-test/shared"
  ]

data FixtureGraphs = FixtureGraphs
  { discoveredTargets :: FoundTargets
  , wholeGraph :: Graphing Dependency
  , rootGraph :: Graphing Dependency
  , browserGraph :: Graphing Dependency
  , serverGraph :: Graphing Dependency
  , sharedGraph :: Graphing Dependency
  }

mkTargets :: [Text] -> FoundTargets
mkTargets = maybe ProjectWithoutTargets FoundTargets . NonEmptySet.nonEmpty . Set.fromList . map BuildTarget

depNames :: Graphing Dependency -> Set Text
depNames = Set.fromList . map dependencyName . Graphing.vertexList

analyzeFixture :: IO FixtureGraphs
analyzeFixture = do
  currentDir <- PIO.getCurrentDir
  let scanDir = currentDir </> fixtureDir
  discovered <- testRunner (Node.discover scanDir) LocalEnvironment
  withResult discovered $ \_ projects -> case projects of
    [project] -> do
      projectType project `shouldBe` PnpmProjectType
      let analyzeWith targets = do
            analyzed <- testRunner (ignoreDebug $ runReader NonStrict $ analyzeProject targets (projectData project)) LocalEnvironment
            withResult analyzed $ \_ depResults -> pure (dependencyGraph depResults)
      FixtureGraphs (projectBuildTargets project)
        <$> analyzeWith (projectBuildTargets project)
        <*> analyzeWith (mkTargets ["pnpm-workspaces"])
        <*> analyzeWith (mkTargets ["@fossa-test/browser"])
        <*> analyzeWith (mkTargets ["@fossa-test/server"])
        <*> analyzeWith (mkTargets ["@fossa-test/shared"])
    projects' -> fail ("expected exactly one discovered project, got " <> show (length projects'))

spec :: Spec
spec = beforeAll analyzeFixture $
  describe "pnpm workspace" $ do
    it "should expose the root and each workspace member as build targets" $ \fixture ->
      discoveredTargets fixture `shouldBe` mkTargets allTargetNames

    it "should report only the selected member's dependencies" $ \fixture -> do
      -- left-pad reaches browser through a catalog: specifier; is-odd belongs
      -- only to server, and colorjs only to the root.
      depNames (browserGraph fixture) `shouldSatisfy` Set.member "left-pad"
      depNames (browserGraph fixture) `shouldSatisfy` (\names -> not (any (`Set.member` names) ["is-odd", "is-number", "colorjs"]))

      depNames (serverGraph fixture) `shouldBe` Set.fromList ["is-odd", "is-number"]
      depNames (rootGraph fixture) `shouldBe` Set.fromList ["colorjs"]

    it "should follow a workspace link into the sibling it names" $ \fixture -> do
      -- browser depends on the shared member via `version: link:../shared`.
      -- Its dependencies, and their transitives, belong in browser's result;
      -- the workspace package itself is not a reportable dependency.
      depNames (browserGraph fixture) `shouldBe` Set.fromList ["left-pad", "uri-js", "punycode"]
      depNames (sharedGraph fixture) `shouldBe` Set.fromList ["uri-js", "punycode"]

    it "should analyze the whole workspace when all targets are selected" $ \fixture -> do
      depNames (wholeGraph fixture) `shouldBe` Set.fromList ["colorjs", "left-pad", "is-odd", "is-number", "uri-js", "punycode"]
      [rootGraph fixture, browserGraph fixture, serverGraph fixture, sharedGraph fixture]
        `shouldSatisfy` all ((`Set.isSubsetOf` depNames (wholeGraph fixture)) . depNames)
