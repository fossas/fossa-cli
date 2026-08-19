{-# LANGUAGE TemplateHaskell #-}

-- | End-to-end coverage for target-level dependency scoping of npm workspaces
-- monorepos using a v3 root lockfile: discovery over a vendored monorepo
-- fixture, then analysis per selected build target.
module Analysis.NpmLockV3WorkspaceSpec (spec) where

import Analysis.FixtureUtils (FixtureEnvironment (LocalEnvironment), testRunner, withResult)
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject))
import App.Types (Mode (NonStrict))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Reader (runReader)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NonEmptySet
import Data.Text (Text)
import DepTypes (Dependency (dependencyName, dependencyVersion))
import Graphing (Graphing)
import Graphing qualified
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO qualified as PIO
import Test.Hspec (Spec, beforeAll, describe, it, shouldBe, shouldSatisfy)
import Types (
  BuildTarget (BuildTarget),
  DependencyResults (dependencyGraph),
  DiscoveredProject (projectBuildTargets, projectData, projectType),
  DiscoveredProjectType (NpmProjectType),
  FoundTargets (FoundTargets, ProjectWithoutTargets),
 )

import Strategy.Node qualified as Node

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/Node/testdata/npm-lock-v3-workspaces/")

allTargetNames :: [Text]
allTargetNames =
  [ "npm-monorepo-fossa-test"
  , "@fossa-test/api-service"
  , "@fossa-test/cli-tool"
  , "@fossa-test/shared-utils"
  , "@fossa-test/web-client"
  ]

data FixtureGraphs = FixtureGraphs
  { discoveredTargets :: FoundTargets
  , wholeGraph :: Graphing Dependency
  , apiGraph :: Graphing Dependency
  , cliGraph :: Graphing Dependency
  , sharedGraph :: Graphing Dependency
  , webGraph :: Graphing Dependency
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
      projectType project `shouldBe` NpmProjectType
      let analyzeWith targets = do
            analyzed <- testRunner (ignoreDebug $ runReader NonStrict $ analyzeProject targets (projectData project)) LocalEnvironment
            withResult analyzed $ \_ depResults -> pure (dependencyGraph depResults)
      FixtureGraphs (projectBuildTargets project)
        <$> analyzeWith (projectBuildTargets project)
        <*> analyzeWith (mkTargets ["@fossa-test/api-service"])
        <*> analyzeWith (mkTargets ["@fossa-test/cli-tool"])
        <*> analyzeWith (mkTargets ["@fossa-test/shared-utils"])
        <*> analyzeWith (mkTargets ["@fossa-test/web-client"])
    projects' -> fail ("expected exactly one discovered project, got " <> show (length projects'))

spec :: Spec
spec = beforeAll analyzeFixture $
  describe "npm workspaces monorepo with v3 lockfile" $ do
    it "should expose the root and each workspace as build targets" $ \fixture ->
      discoveredTargets fixture `shouldBe` mkTargets allTargetNames

    it "should produce distinct dependency sets per selected workspace" $ \fixture -> do
      let workspaceGraphs = [apiGraph fixture, cliGraph fixture, sharedGraph fixture, webGraph fixture]
      length (Set.fromList (map depNames workspaceGraphs)) `shouldBe` length workspaceGraphs

      depNames (apiGraph fixture) `shouldSatisfy` Set.isSubsetOf (Set.fromList ["cors", "uuid"])
      depNames (apiGraph fixture) `shouldSatisfy` (\names -> not (any (`Set.member` names) ["lodash", "commander", "react", "typescript"]))

      depNames (cliGraph fixture) `shouldSatisfy` Set.isSubsetOf (Set.fromList ["chalk", "commander"])
      depNames (cliGraph fixture) `shouldSatisfy` (\names -> not (any (`Set.member` names) ["cors", "lodash", "react"]))

      depNames (sharedGraph fixture) `shouldBe` Set.fromList ["lodash", "uuid"]

    it "should attribute a linked sibling workspace's dependencies to the requesting workspace" $ \fixture -> do
      -- web-client depends on the shared-utils workspace via a lockfile link
      -- entry; its scoped result must include shared-utils' external deps
      -- with real versions, and no entry for the workspace itself.
      depNames (webGraph fixture) `shouldSatisfy` Set.isSubsetOf (Set.fromList ["react", "classnames", "lodash", "uuid"])
      Set.member "@fossa-test/shared-utils" (depNames (webGraph fixture)) `shouldBe` False
      filter (isNothing . dependencyVersion) (Graphing.vertexList (webGraph fixture)) `shouldBe` []

    it "should analyze the whole repo when all targets are selected" $ \fixture -> do
      let wholeNames = depNames (wholeGraph fixture)
      -- root dev deps and every workspace's deps are present
      ["prettier", "typescript", "cors", "chalk", "lodash", "react"] `shouldSatisfy` all (`Set.member` wholeNames)
      [apiGraph fixture, cliGraph fixture, sharedGraph fixture, webGraph fixture]
        `shouldSatisfy` all ((`Set.isSubsetOf` wholeNames) . depNames)
