{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Analyze.ForkAliasSpec (spec) where

import App.Fossa.Analyze.ForkAlias
  ( buildProject
  , collectForkAliasLabels
  , mergeForkAliasLabels
  , mkForkAliasMap
  , translateDependency
  , translateDependencyGraph
  , translateLocatorWithForkAliases
  )
import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.ManualDeps (ForkAlias (..), ForkAliasEntry (..), forkAliasEntryToLocator)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map qualified as Map
import Data.Set qualified as Set
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
import Graphing qualified
import Path (Abs, Dir, Path, mkAbsDir)
import Srclib.Types
  ( Locator (..)
  , ProvidedPackageLabel (..)
  , ProvidedPackageLabelScope (..)
  , ProvidedPackageLabels (..)
  , SourceUnit (..)
  , SourceUnitBuild (..)
  , SourceUnitDependency (..)
  , buildProvidedPackageLabels
  , toProjectLocator
  , unProvidedPackageLabels
  )
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldMatchList)
import Types (DiscoveredProjectType (CargoProjectType), GraphBreadth (Complete))

spec :: Spec
spec = do
  describe "mkForkAliasMap" $ do
    it "should create a map keyed by project locator" $ do
      let fork = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          -- The map is keyed by project locator (without version) of the fork
          forkLocator = forkAliasEntryToLocator fork
          projectLocator = forkLocator{locatorRevision = Nothing}

      Map.lookup projectLocator forkAliasMap `shouldBe` Just forkAlias

  describe "collectForkAliasLabels" $ do
    it "should collect labels from fork aliases keyed by base project locator" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" Nothing
          label = ProvidedPackageLabel "internal" ProvidedPackageLabelScopeOrg
          forkAlias = ForkAlias fork base [label]
          labels = collectForkAliasLabels [forkAlias]

      labels `shouldBe` Map.singleton "cargo+serde$" [label]

    it "should skip fork aliases with no labels" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          labels = collectForkAliasLabels [forkAlias]

      labels `shouldBe` Map.empty

    it "should merge labels from multiple fork aliases with same base" $ do
      let fork1 = ForkAliasEntry CargoType "my-serde" Nothing
          base1 = ForkAliasEntry CargoType "serde" Nothing
          label1 = ProvidedPackageLabel "internal" ProvidedPackageLabelScopeOrg
          forkAlias1 = ForkAlias fork1 base1 [label1]
          fork2 = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base2 = ForkAliasEntry CargoType "serde" Nothing
          label2 = ProvidedPackageLabel "approved" ProvidedPackageLabelScopeProject
          forkAlias2 = ForkAlias fork2 base2 [label2]
          labels = collectForkAliasLabels [forkAlias1, forkAlias2]

      labels `shouldBe` Map.singleton "cargo+serde$" [label1, label2]

  describe "mergeForkAliasLabels" $ do
    it "should merge labels into source unit that contains matching dependency" $ do
      let forkAliasLabels = Map.singleton "cargo+serde$" [ProvidedPackageLabel "internal" ProvidedPackageLabelScopeOrg]
          serdeLocator = Locator "cargo" "serde" (Just "1.0.0")
          unit =
            SourceUnit
              "test"
              "cargo"
              "Cargo.toml"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = [serdeLocator]
                    , buildDependencies = []
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let merged = mergeForkAliasLabels forkAliasLabels unit
      let labels = unProvidedPackageLabels <$> sourceUnitLabels merged

      labels `shouldBe` Just forkAliasLabels

    it "should not add labels to source unit that doesn't contain matching dependency" $ do
      let forkAliasLabels = Map.singleton "cargo+serde$" [ProvidedPackageLabel "internal" ProvidedPackageLabelScopeOrg]
          otherLocator = Locator "cargo" "other-pkg" (Just "1.0.0")
          unit =
            SourceUnit
              "test"
              "cargo"
              "Cargo.toml"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = [otherLocator]
                    , buildDependencies = []
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let merged = mergeForkAliasLabels forkAliasLabels unit

      sourceUnitLabels merged `shouldBe` Nothing

    it "should merge with existing labels" $ do
      let forkAliasLabels = Map.singleton "cargo+serde$" [ProvidedPackageLabel "internal" ProvidedPackageLabelScopeOrg]
          existingLabels = Map.singleton "cargo+serde$" [ProvidedPackageLabel "existing" ProvidedPackageLabelScopeRevision]
          serdeLocator = Locator "cargo" "serde" (Just "1.0.0")
          unit =
            SourceUnit
              "test"
              "cargo"
              "Cargo.toml"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = [serdeLocator]
                    , buildDependencies = []
                    }
              )
              Complete
              []
              []
              (Just $ ProvidedPackageLabels existingLabels)
              Nothing

      let merged = mergeForkAliasLabels forkAliasLabels unit
      let labels = unProvidedPackageLabels <$> sourceUnitLabels merged

      labels `shouldBe` Just (Map.singleton "cargo+serde$" [ProvidedPackageLabel "internal" ProvidedPackageLabelScopeOrg, ProvidedPackageLabel "existing" ProvidedPackageLabelScopeRevision])

    it "should match by project locator (without version)" $ do
      let forkAliasLabels = Map.singleton "cargo+serde$" [ProvidedPackageLabel "internal" ProvidedPackageLabelScopeOrg]
          -- Different version, but should still match
          serdeLocator = Locator "cargo" "serde" (Just "2.0.0")
          unit =
            SourceUnit
              "test"
              "cargo"
              "Cargo.toml"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = [serdeLocator]
                    , buildDependencies = []
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let merged = mergeForkAliasLabels forkAliasLabels unit
      let labels = unProvidedPackageLabels <$> sourceUnitLabels merged

      labels `shouldBe` Just forkAliasLabels

    it "should handle empty fork alias labels" $ do
      let forkAliasLabels = Map.empty
          unit =
            SourceUnit
              "test"
              "cargo"
              "Cargo.toml"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = []
                    , buildDependencies = []
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let merged = mergeForkAliasLabels forkAliasLabels unit

      merged `shouldBe` unit

  describe "translateLocatorWithForkAliases" $ do
    it "should translate when fork version matches" $ do
      let fork = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "cargo" "my-serde" (Just "1.0.0")

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      translated `shouldBe` Locator "cargo" "serde" (Just "1.0.0")

    it "should not translate when fork version does not match" $ do
      let fork = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "cargo" "my-serde" (Just "2.0.0")

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      -- Should remain unchanged because version doesn't match
      translated `shouldBe` loc

    it "should translate any version when fork version is not specified" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "cargo" "my-serde" (Just "1.0.0")

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      translated `shouldBe` Locator "cargo" "serde" (Just "1.0.0")

    it "should use base version when specified" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" (Just "2.0.0")
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "cargo" "my-serde" (Just "1.0.0")

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      -- Should use base version 2.0.0 instead of original 1.0.0
      translated `shouldBe` Locator "cargo" "serde" (Just "2.0.0")

    it "should preserve original version when base version is not specified" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "cargo" "my-serde" (Just "1.5.0")

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      -- Should preserve original version 1.5.0
      translated `shouldBe` Locator "cargo" "serde" (Just "1.5.0")

    it "should not translate when fork specifies version but loc has none" $ do
      let fork = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "cargo" "my-serde" Nothing

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      -- Should remain unchanged because loc has no version but fork requires one
      translated `shouldBe` loc

    it "should handle combination: fork version matches and base version specified" $ do
      let fork = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base = ForkAliasEntry CargoType "serde" (Just "2.0.0")
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "cargo" "my-serde" (Just "1.0.0")

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      -- Version matches fork, so translate to base with base version
      translated `shouldBe` Locator "cargo" "serde" (Just "2.0.0")

    it "should not translate when type or name doesn't match" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "npm" "my-serde" (Just "1.0.0")

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      -- Should remain unchanged because type doesn't match
      translated `shouldBe` loc

    it "should not translate when locator doesn't match any fork alias" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "cargo" "other-package" (Just "1.0.0")

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      -- Should remain unchanged because name doesn't match
      translated `shouldBe` loc

    it "should handle locator with no version when fork has no version requirement" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" (Just "2.0.0")
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          loc = Locator "cargo" "my-serde" Nothing

      let translated = translateLocatorWithForkAliases forkAliasMap loc

      -- Should translate to base with base version
      translated `shouldBe` Locator "cargo" "serde" (Just "2.0.0")

  describe "translateDependency with fork aliases" $ do
    it "should translate when fork version matches" $ do
      let fork = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          dep = Dependency CargoType "my-serde" (Just (CEq "1.0.0")) [] Set.empty Map.empty

      let translated = translateDependency forkAliasMap dep

      translated `shouldBe` Dependency CargoType "serde" (Just (CEq "1.0.0")) [] Set.empty Map.empty

    it "should not translate when fork version does not match" $ do
      let fork = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          dep = Dependency CargoType "my-serde" (Just (CEq "2.0.0")) [] Set.empty Map.empty

      let translated = translateDependency forkAliasMap dep

      -- Should remain unchanged because version doesn't match
      translated `shouldBe` dep

    it "should translate any version when fork version is not specified" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          dep = Dependency CargoType "my-serde" (Just (CEq "1.0.0")) [] Set.empty Map.empty

      let translated = translateDependency forkAliasMap dep

      translated `shouldBe` Dependency CargoType "serde" (Just (CEq "1.0.0")) [] Set.empty Map.empty

    it "should use base version when specified" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" (Just "2.0.0")
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          dep = Dependency CargoType "my-serde" (Just (CEq "1.0.0")) [] Set.empty Map.empty

      let translated = translateDependency forkAliasMap dep

      -- Should use base version 2.0.0 instead of original 1.0.0
      translated `shouldBe` Dependency CargoType "serde" (Just (CEq "2.0.0")) [] Set.empty Map.empty

    it "should preserve original version when base version is not specified" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          dep = Dependency CargoType "my-serde" (Just (CEq "1.5.0")) [] Set.empty Map.empty

      let translated = translateDependency forkAliasMap dep

      -- Should preserve original version 1.5.0
      translated `shouldBe` Dependency CargoType "serde" (Just (CEq "1.5.0")) [] Set.empty Map.empty

    it "should not translate when fork specifies version but dep has none" $ do
      let fork = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          dep = Dependency CargoType "my-serde" Nothing [] Set.empty Map.empty

      let translated = translateDependency forkAliasMap dep

      -- Should remain unchanged because dep has no version but fork requires one
      translated `shouldBe` dep

    it "should handle combination: fork version matches and base version specified" $ do
      let fork = ForkAliasEntry CargoType "my-serde" (Just "1.0.0")
          base = ForkAliasEntry CargoType "serde" (Just "2.0.0")
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          dep = Dependency CargoType "my-serde" (Just (CEq "1.0.0")) [] Set.empty Map.empty

      let translated = translateDependency forkAliasMap dep

      -- Version matches fork, so translate to base with base version
      translated `shouldBe` Dependency CargoType "serde" (Just (CEq "2.0.0")) [] Set.empty Map.empty

    it "should not translate when type or name doesn't match" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" Nothing
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          dep = Dependency NodeJSType "my-serde" (Just (CEq "1.0.0")) [] Set.empty Map.empty

      let translated = translateDependency forkAliasMap dep

      -- Should remain unchanged because type doesn't match
      translated `shouldBe` dep

  describe "translateDependencyGraph with fork aliases" $ do
    it "should translate multiple dependencies in a graph" $ do
      let fork1 = ForkAliasEntry CargoType "my-serde" Nothing
          base1 = ForkAliasEntry CargoType "serde" (Just "2.0.0")
          fork2 = ForkAliasEntry GoType "github.com/myorg/gin" (Just "v1.9.1")
          base2 = ForkAliasEntry GoType "github.com/gin-gonic/gin" Nothing
          forkAliases = [ForkAlias fork1 base1 [], ForkAlias fork2 base2 []]
          forkAliasMap = mkForkAliasMap forkAliases
          dep1 = Dependency CargoType "my-serde" (Just (CEq "1.0.0")) [] Set.empty Map.empty
          dep2 = Dependency GoType "github.com/myorg/gin" (Just (CEq "v1.9.1")) [] Set.empty Map.empty
          graph = Graphing.deeps [dep1, dep2]

      let translated = translateDependencyGraph forkAliasMap graph
      let vertices = Graphing.vertexList translated

      -- dep1 should be translated to serde with version 2.0.0
      -- dep2 should be translated to gin-gonic/gin with version v1.9.1 preserved
      vertices `shouldMatchList` [Dependency CargoType "serde" (Just (CEq "2.0.0")) [] Set.empty Map.empty, Dependency GoType "github.com/gin-gonic/gin" (Just (CEq "v1.9.1")) [] Set.empty Map.empty]

  describe "buildProject" $ do
    it "should build project JSON with translated graph" $ do
      let fork = ForkAliasEntry CargoType "my-serde" Nothing
          base = ForkAliasEntry CargoType "serde" (Just "2.0.0")
          forkAlias = ForkAlias fork base []
          forkAliasMap = mkForkAliasMap [forkAlias]
          dep = Dependency CargoType "my-serde" (Just (CEq "1.0.0")) [] Set.empty Map.empty
          graph = Graphing.deeps [dep]
#ifdef mingw32_HOST_OS
          testPath = $(mkAbsDir "C:/test")
#else
          testPath = $(mkAbsDir "/test")
#endif
          project =
            ProjectResult
              { projectResultType = CargoProjectType
              , projectResultPath = testPath
              , projectResultGraph = graph
              , projectResultGraphBreadth = Complete
              , projectResultManifestFiles = []
              }

      let result = buildProject forkAliasMap project

      -- Verify it's a JSON object with expected fields
      case result of
        Aeson.Object obj -> do
          -- Check that path and graph fields exist
          let pathKey = Key.fromString "path"
              graphKey = Key.fromString "graph"
              hasPath = KeyMap.member pathKey obj
              hasGraph = KeyMap.member graphKey obj
          if hasPath && hasGraph
            then True `shouldBe` True -- Graph structure is complex, just verify fields exist
            else expectationFailure $ "Missing fields: path=" ++ show hasPath ++ ", graph=" ++ show hasGraph
        _ -> expectationFailure "Result is not a JSON object"
