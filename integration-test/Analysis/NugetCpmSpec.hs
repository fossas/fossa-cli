{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.NugetCpmSpec (spec) where

import Analysis.FixtureUtils (FixtureEnvironment (LocalEnvironment), testRunner, withResult)
import Data.Map.Strict qualified as Map
import DepTypes (DepType (NuGetType), Dependency (..), VerConstraint (CEq))
import Graphing (vertexList)
import Path (reldir, (</>))
import Path.IO qualified as PIO
import Strategy.NuGet qualified as NuGet
import Test.Hspec
import Types (DependencyResults (..), DiscoveredProject (..), GraphBreadth (Partial))

runCpmAnalysis :: IO [(DiscoveredProject NuGet.NuGetProject, DependencyResults)]
runCpmAnalysis = do
  cwd <- PIO.getCurrentDir
  let targetDir = cwd </> [reldir|integration-test/Analysis/testdata/nuget-cpm/|]
  result <- testRunner (doAnalysis targetDir) LocalEnvironment
  withResult result $ \_ res -> pure res
  where
    doAnalysis targetDir = do
      projects <- NuGet.discover targetDir
      mapM
        ( \dp -> do
            dr <- NuGet.getDeps (projectData dp)
            pure (dp, dr)
        )
        projects

spec :: Spec
spec = do
  describe "NuGet Central Package Management (CPM)" $ do
    results <- runIO runCpmAnalysis

    it "discovers the .csproj project" $ do
      length results `shouldBe` 1

    it "resolves versions from Directory.Packages.props" $ do
      let (_, dr) = head results
          deps = vertexList (dependencyGraph dr)
          depMap = Map.fromList [(dependencyName d, d) | d <- deps]

      -- Newtonsoft.Json: no inline version, resolved from Directory.Packages.props
      case Map.lookup "Newtonsoft.Json" depMap of
        Nothing -> expectationFailure "Newtonsoft.Json not found in dependencies"
        Just dep -> dependencyVersion dep `shouldBe` Just (CEq "13.0.1")

      -- Serilog: no inline version, resolved from Directory.Packages.props
      case Map.lookup "Serilog" depMap of
        Nothing -> expectationFailure "Serilog not found in dependencies"
        Just dep -> dependencyVersion dep `shouldBe` Just (CEq "3.1.1")

    it "prefers inline version over CPM version" $ do
      let (_, dr) = head results
          deps = vertexList (dependencyGraph dr)
          depMap = Map.fromList [(dependencyName d, d) | d <- deps]

      -- xunit: inline version 2.7.0 takes precedence over CPM's 2.6.1
      case Map.lookup "xunit" depMap of
        Nothing -> expectationFailure "xunit not found in dependencies"
        Just dep -> dependencyVersion dep `shouldBe` Just (CEq "2.7.0")

    it "reports Partial graph breadth with all NuGetType deps" $ do
      let (_, dr) = head results
          deps = vertexList (dependencyGraph dr)
      dependencyGraphBreadth dr `shouldBe` Partial
      all (\d -> dependencyType d == NuGetType) deps `shouldBe` True
