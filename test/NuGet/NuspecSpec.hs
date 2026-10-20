{-# LANGUAGE TemplateHaskell #-}

module NuGet.NuspecSpec (
  spec,
) where

import App.Fossa.Analyze.LicenseAnalyze (licenseAnalyzeProject)
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil
import Graphing qualified
import Parse.XML
import Path (mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.NuGet.Nuspec
import Strategy.NuGet.Nuspec qualified as Nuspec
import Test.Effect (it', shouldBe', shouldMatchList')
import Test.Hspec
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (NuspecProjectType),
  FoundTargets (ProjectWithoutTargets),
  GraphBreadth (Partial),
  License (License),
  LicenseResult (..),
  LicenseType (..),
 )

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
    , dependencyVersion = Just (CEq "2.0.0")
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
    , dependencyTags = Map.empty
    }

nugetDependency :: Text -> Text -> Dependency
nugetDependency name version =
  Dependency
    { dependencyType = NuGetType
    , dependencyName = name
    , dependencyVersion = Just (CEq version)
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

nuspec :: Nuspec
nuspec = Nuspec groupList licenses (Just "test.com")

licenses :: [NuspecLicense]
licenses = [NuspecLicense "expression" "", NuspecLicense "" "BSD-2-Clause", NuspecLicense "expression" "  ", NuspecLicense "file" "", NuspecLicense "file" "doesnt-exist.txt", NuspecLicense "expression" "Foo"]

groupList :: [Group]
groupList = [Group [depOne, depTwo], Group [depThree]]

depOne :: NuGetDependency
depOne = NuGetDependency "one" "1.0.0"

depTwo :: NuGetDependency
depTwo = NuGetDependency "two" "2.0.0"

depThree :: NuGetDependency
depThree = NuGetDependency "three" "3.0.0"

spec :: Spec
spec = do
  dependenciesAndLicense <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/test.nuspec")
  singleLicense <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/license.nuspec")
  multipleLicenses <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/multiple-licenses.nuspec")
  -- Trimmed from the flat-list example in docs/references/strategies/languages/dotnet/nuspec.md.
  flatDependencies <- runIO (TIO.readFile "test/NuGet/testdata/nuspec/flat.nuspec")
  currDir <- runIO getCurrentDir
  let multiDir = currDir </> $(mkRelDir "test/NuGet/testdata/multi-nuspec")
      projectDir = currDir </> $(mkRelDir "test/NuGet/testdata/nuspec-project")
      projectFile = projectDir </> $(mkRelFile "package.nuspec")

  describe "nuspec analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case parseXML dependenciesAndLicense of
        Right project -> do
          (groups project) `shouldContain` groupList
          (license project) `shouldMatchList` [NuspecLicense "file" "license-file"]
          (licenseUrl project) `shouldBe` (Just "https://licence.location.com/LICENSE.md")
        Left err -> expectationFailure (toString ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "reads a file and extracts the correct license" $ do
      case parseXML singleLicense of
        Right project -> do
          (groups project) `shouldBe` []
          (license project) `shouldMatchList` [NuspecLicense "file" "LICENSE.txt"]
          (licenseUrl project) `shouldBe` Nothing
        Left err -> expectationFailure (toString ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "reads a file with multiple licenses" $ do
      case parseXML multipleLicenses of
        Right project -> do
          (groups project) `shouldBe` []
          (license project) `shouldMatchList` licenses
          (licenseUrl project) `shouldBe` (Just "test.com")
        Left err -> expectationFailure (toString ("could not parse nuspec file: " <> xmlErrorPretty err))

    it "constructs an accurate graph" $ do
      let graph = buildGraph nuspec
      expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
      expectDirect [dependencyOne, dependencyTwo, dependencyThree] graph
      expectEdges [] graph

    -- Fails until src/Strategy/NuGet/Nuspec.hs:FromXML Nuspec is fixed: only `<group>` children of `<dependencies>` are read, so the documented flat `<dependencies><dependency/></dependencies>` form yields no dependencies and reports success. See the PR description.
    it "reads dependencies declared directly under <dependencies>" $ do
      case parseXML flatDependencies of
        Right project -> do
          let graph = buildGraph project
          expectDeps [nugetDependency "GenericLibsBase" "1.0.1", nugetDependency "EntityFramework" "6.1.3"] graph
          expectDirect [nugetDependency "GenericLibsBase" "1.0.1", nugetDependency "EntityFramework" "6.1.3"] graph
        Left err -> expectationFailure (toString ("could not parse nuspec file: " <> xmlErrorPretty err))

  describe "nuspec discovery" $ do
    it' "discovers a .nuspec file as a project in its directory" $ do
      projects <- Nuspec.discover projectDir
      map projectPath projects `shouldBe'` [projectDir]
      map projectType projects `shouldBe'` [NuspecProjectType]
      map projectBuildTargets projects `shouldBe'` [ProjectWithoutTargets]

    -- Fails until src/Strategy/NuGet/Nuspec.hs:findProjects is fixed: `find` keeps only the first .nuspec file listed in a directory, so every sibling .nuspec is silently dropped. See the PR description.
    it' "discovers every .nuspec sibling in a directory" $ do
      projects <- Nuspec.discover multiDir
      results <- traverse (Nuspec.getDeps . projectData) projects
      map dependencyManifestFiles results `shouldMatchList'` [[multiDir </> $(mkRelFile "first.nuspec")], [multiDir </> $(mkRelFile "second.nuspec")]]
      concatMap (Graphing.vertexList . dependencyGraph) results `shouldMatchList'` [nugetDependency "first-dep" "1.0.0", nugetDependency "second-dep" "2.0.0"]

  describe "nuspec project analysis" $ do
    it' "reports the manifest's dependencies as a partial graph of direct dependencies" $ do
      projects <- Nuspec.findProjects projectDir
      results <- traverse Nuspec.getDeps projects
      map dependencyGraphBreadth results `shouldBe'` [Partial]
      map dependencyManifestFiles results `shouldBe'` [[projectFile]]
      concatMap (Graphing.directList . dependencyGraph) results `shouldMatchList'` [nugetDependency "alpha" "1.0.0", nugetDependency "beta" "2.0.0"]
      concatMap (Graphing.edgesList . dependencyGraph) results `shouldBe'` []

    it' "reports the licenseUrl and every license element of the manifest" $ do
      projects <- Nuspec.findProjects projectDir
      results <- concat <$> traverse licenseAnalyzeProject projects
      map licenseFile results `shouldBe'` [toFilePath projectFile]
      concatMap licensesFound results
        `shouldMatchList'` [ License LicenseURL "https://example.com/LICENSE"
                           , License LicenseSPDX "MIT"
                           , License LicenseFile "LICENSE.txt"
                           , License UnknownType "BSD-2-Clause"
                           ]
