module Srclib.TypesSpec (spec) where

import App.Fossa.Analyze (mkForkAliasMap, translateDependency, translateDependencyGraph)
import App.Fossa.ManualDeps (ForkAlias (..), ForkAliasEntry (..), forkAliasEntryToLocator)
import Data.Map qualified as Map
import Data.Set qualified as Set
import DepTypes (CargoType, Dependency (..), DepType (..), GitType, GoType, NodeJSType, PipType, VerConstraint (CEq))
import Graphing (Graphing)
import Graphing qualified
import Srclib.Converter (toLocator)
import Srclib.Types (
  Locator (..),
  SourceUnit (..),
  SourceUnitBuild (..),
  SourceUnitDependency (..),
  toProjectLocator,
  translateSourceUnitLocators,
 )
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldMatchList)
import Types (GraphBreadth (Complete))

spec :: Spec
spec = do
  describe "translateSourceUnitLocators" $ do
    it "should translate locators in buildImports" $ do
      let myForkLocator = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          sourceUnit =
            SourceUnit
              "test"
              "go"
              "go.mod"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = [myForkLocator]
                    , buildDependencies = []
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let translated = translateSourceUnitLocators translationMap sourceUnit
      let translatedImports = buildImports <$> sourceUnitBuild translated

      translatedImports `shouldBe` Just [Locator "go" "github.com/gin-gonic/gin" (Just "v1.9.1")]

    it "should translate locators in sourceDepLocator" $ do
      let myForkLocator = Locator "go" "github.com/myorg/testify" (Just "v1.8.4")
          baseLocator = Locator "go" "github.com/stretchr/testify" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          dep = SourceUnitDependency myForkLocator []
          sourceUnit =
            SourceUnit
              "test"
              "go"
              "go.mod"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = []
                    , buildDependencies = [dep]
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let translated = translateSourceUnitLocators translationMap sourceUnit
      let translatedDeps = buildDependencies <$> sourceUnitBuild translated

      case translatedDeps of
        Just [translatedDep] ->
          sourceDepLocator translatedDep `shouldBe` Locator "go" "github.com/stretchr/testify" (Just "v1.8.4")
        _ -> expectationFailure "Expected exactly one dependency"

    it "should translate locators in sourceDepImports" $ do
      let myForkLocator = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          dep = SourceUnitDependency (Locator "go" "other" Nothing) [myForkLocator]
          sourceUnit =
            SourceUnit
              "test"
              "go"
              "go.mod"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = []
                    , buildDependencies = [dep]
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let translated = translateSourceUnitLocators translationMap sourceUnit
      let translatedDeps = buildDependencies <$> sourceUnitBuild translated

      case translatedDeps of
        Just [translatedDep] ->
          sourceDepImports translatedDep `shouldBe` [Locator "go" "github.com/gin-gonic/gin" (Just "v1.9.1")]
        _ -> expectationFailure "Expected exactly one dependency"

    it "should preserve revision when translating" $ do
      let myForkLocator = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          sourceUnit =
            SourceUnit
              "test"
              "go"
              "go.mod"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = [myForkLocator]
                    , buildDependencies = []
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let translated = translateSourceUnitLocators translationMap sourceUnit
      let translatedImports = buildImports <$> sourceUnitBuild translated

      -- The revision from the original locator should be preserved
      translatedImports `shouldBe` Just [Locator "go" "github.com/gin-gonic/gin" (Just "v1.9.1")]

    it "should not translate locators that don't match the map" $ do
      let myForkLocator = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          otherLocator = Locator "go" "github.com/other/pkg" (Just "v1.0.0")
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          sourceUnit =
            SourceUnit
              "test"
              "go"
              "go.mod"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = [myForkLocator, otherLocator]
                    , buildDependencies = []
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let translated = translateSourceUnitLocators translationMap sourceUnit
      let translatedImports = buildImports <$> sourceUnitBuild translated

      -- myForkLocator should be translated to baseLocator, otherLocator should remain unchanged
      translatedImports `shouldBe` Just [Locator "go" "github.com/gin-gonic/gin" (Just "v1.9.1"), otherLocator]

    it "should match locators ignoring version" $ do
      let myForkLocatorV1 = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          myForkLocatorV2 = Locator "go" "github.com/myorg/gin" (Just "v2.0.0")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocatorV1) baseLocator
          sourceUnit =
            SourceUnit
              "test"
              "go"
              "go.mod"
              ( Just
                  SourceUnitBuild
                    { buildArtifact = "default"
                    , buildSucceeded = True
                    , buildImports = [myForkLocatorV2]
                    , buildDependencies = []
                    }
              )
              Complete
              []
              []
              Nothing
              Nothing

      let translated = translateSourceUnitLocators translationMap sourceUnit
      let translatedImports = buildImports <$> sourceUnitBuild translated

      -- Should match myForkLocatorV2 even though it has a different version, because we match by fetcher+project only
      translatedImports `shouldBe` Just [Locator "go" "github.com/gin-gonic/gin" (Just "v2.0.0")]

    it "should handle SourceUnit without build" $ do
      let translationMap = Map.empty
          sourceUnit =
            SourceUnit
              "test"
              "go"
              "go.mod"
              Nothing
              Complete
              []
              []
              Nothing
              Nothing

      let translated = translateSourceUnitLocators translationMap sourceUnit

      -- Should remain unchanged
      translated `shouldBe` sourceUnit

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
          dep = Dependency CargoType "my-serde" (Just (CEq "2.0.0")) [] mempty Map.empty

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
          dep = Dependency CargoType "my-serde" (Just (CEq "1.5.0")) [] mempty Map.empty

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
          graph = Graphing.overlay (Graphing.vertex dep1) (Graphing.vertex dep2)

      let translated = translateDependencyGraph forkAliasMap graph
      let vertices = Graphing.vertexList translated

      -- dep1 should be translated to serde with version 2.0.0
      -- dep2 should be translated to gin-gonic/gin with version v1.9.1 preserved
      vertices `shouldMatchList` [Dependency CargoType "serde" (Just (CEq "2.0.0")) [] Set.empty Map.empty, Dependency GoType "github.com/gin-gonic/gin" (Just (CEq "v1.9.1")) [] Set.empty Map.empty]
