{-# LANGUAGE TemplateHaskell #-}

module NuGet.ProjectJsonSpec (
  spec,
) where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import DepTypes
import GraphUtil
import Path (mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Strategy.NuGet.ProjectJson
import Test.Effect (it', shouldBe')
import Test.Hspec
import Types (DependencyResults (dependencyGraph))

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "one"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "two"
    , dependencyVersion = Just (CCompatible "2.*")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "three"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.fromList [("type", ["sometype"])]
    }

dependencyOneOverride :: Dependency
dependencyOneOverride =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = "one"
    , dependencyVersion = Just (CEq "9.9.9")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  let jsoncDir = currentDir </> $(mkRelDir "test/NuGet/testdata/project-json-jsonc")
  testFile <- runIO (BS.readFile "test/NuGet/testdata/project.json")
  frameworkDepsFile <- runIO (BS.readFile "test/NuGet/testdata/project-framework-deps.json")
  noDepsFile <- runIO (BS.readFile "test/NuGet/testdata/project-no-deps.json")
  conflictingDepsFile <- runIO (BS.readFile "test/NuGet/testdata/project-conflicting-deps.json")

  describe "project.json analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case eitherDecodeStrict testFile of
        Right res -> do
          let graph = buildGraph res
          expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyThree] graph
          expectEdges [] graph
        Left _ -> expectationFailure "failed to parse"

    it "reads dependencies declared per-framework instead of top-level" $ do
      case eitherDecodeStrict frameworkDepsFile of
        Right res -> do
          let graph = buildGraph res
          expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyThree] graph
          expectEdges [] graph
        Left _ -> expectationFailure "failed to parse"

    it "keeps every distinct version of a package declared in several places" $ do
      case eitherDecodeStrict conflictingDepsFile of
        Right res -> do
          let graph = buildGraph res
          expectDeps [dependencyOne, dependencyOneOverride, dependencyTwo, dependencyThree] graph
          expectDirect [dependencyOne, dependencyOneOverride, dependencyTwo, dependencyThree] graph
          expectEdges [] graph
        Left _ -> expectationFailure "failed to parse"

    it "parses a project.json without any dependencies key" $ do
      case eitherDecodeStrict noDepsFile of
        Right res -> do
          let graph = buildGraph res
          expectDeps [] graph
          expectDirect [] graph
          expectEdges [] graph
        Left _ -> expectationFailure "failed to parse"

  describe "looksLikeNuGetManifest" $ do
    it "accepts manifests declaring top-level or per-framework dependencies" $ do
      case traverse eitherDecodeStrict [testFile, frameworkDepsFile, conflictingDepsFile] of
        Right objs -> objs `shouldSatisfy` all looksLikeNuGetManifest
        Left _ -> expectationFailure "failed to parse"

    it "rejects a project.json from another tool (e.g. Nx)" $ do
      case eitherDecodeStrict noDepsFile of
        Right obj -> obj `shouldNotSatisfy` looksLikeNuGetManifest
        Left _ -> expectationFailure "failed to parse"

  -- NuGet reads project.json with Json.NET, and Nx reads its own project.json
  -- with jsonc-parser: both accept comments and trailing commas.
  describe "project.json with comments and trailing commas" $ do
    it' "analyzes a NuGet manifest" $ do
      results <- traverse getDeps =<< findProjects (jsoncDir </> $(mkRelDir "nuget"))
      length results `shouldBe'` 1
      for_ results $ \result -> do
        expectDeps' [dependencyOne, dependencyTwo, dependencyThree] (dependencyGraph result)
        expectDirect' [dependencyOne, dependencyTwo, dependencyThree] (dependencyGraph result)

    it' "does not claim a project.json from another tool (e.g. Nx)" $ do
      projects <- findProjects (jsoncDir </> $(mkRelDir "nx"))
      projects `shouldBe'` []
