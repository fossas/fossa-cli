module Srclib.TypesSpec (spec) where

import App.Fossa.Analyze (mkForkAliasMap, translateDependency, translateDependencyGraph, translateLocatorWithForkAliases)
import App.Fossa.ManualDeps (ForkAlias (..), ForkAliasEntry (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import DepTypes (Dependency (..), DepType (..), VerConstraint (CEq))
import Graphing qualified
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
    -- Helper to create a simple translation function from a map (for backward compatibility in tests)
    let simpleTranslate translationMap loc =
          case Map.lookup (toProjectLocator loc) translationMap of
            Nothing -> loc
            Just replacement -> replacement{locatorRevision = locatorRevision loc}

    it "should translate locators in buildImports" $ do
      let myForkLocator = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          translateLocator = simpleTranslate translationMap
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

      let translated = translateSourceUnitLocators translateLocator sourceUnit
      let translatedImports = buildImports <$> sourceUnitBuild translated

      translatedImports `shouldBe` Just [Locator "go" "github.com/gin-gonic/gin" (Just "v1.9.1")]

    it "should translate locators in sourceDepLocator" $ do
      let myForkLocator = Locator "go" "github.com/myorg/testify" (Just "v1.8.4")
          baseLocator = Locator "go" "github.com/stretchr/testify" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          translateLocator = simpleTranslate translationMap
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

      let translated = translateSourceUnitLocators translateLocator sourceUnit
      let translatedDeps = buildDependencies <$> sourceUnitBuild translated

      case translatedDeps of
        Just [translatedDep] ->
          sourceDepLocator translatedDep `shouldBe` Locator "go" "github.com/stretchr/testify" (Just "v1.8.4")
        _ -> expectationFailure "Expected exactly one dependency"

    it "should translate locators in sourceDepImports" $ do
      let myForkLocator = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          translateLocator = simpleTranslate translationMap
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

      let translated = translateSourceUnitLocators translateLocator sourceUnit
      let translatedDeps = buildDependencies <$> sourceUnitBuild translated

      case translatedDeps of
        Just [translatedDep] ->
          sourceDepImports translatedDep `shouldBe` [Locator "go" "github.com/gin-gonic/gin" (Just "v1.9.1")]
        _ -> expectationFailure "Expected exactly one dependency"

    it "should preserve revision when translating" $ do
      let myForkLocator = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          translateLocator = simpleTranslate translationMap
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

      let translated = translateSourceUnitLocators translateLocator sourceUnit
      let translatedImports = buildImports <$> sourceUnitBuild translated

      -- The revision from the original locator should be preserved
      translatedImports `shouldBe` Just [Locator "go" "github.com/gin-gonic/gin" (Just "v1.9.1")]

    it "should not translate locators that don't match the map" $ do
      let myForkLocator = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          otherLocator = Locator "go" "github.com/other/pkg" (Just "v1.0.0")
          translationMap = Map.singleton (toProjectLocator myForkLocator) baseLocator
          translateLocator = simpleTranslate translationMap
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

      let translated = translateSourceUnitLocators translateLocator sourceUnit
      let translatedImports = buildImports <$> sourceUnitBuild translated

      -- myForkLocator should be translated to baseLocator, otherLocator should remain unchanged
      translatedImports `shouldBe` Just [Locator "go" "github.com/gin-gonic/gin" (Just "v1.9.1"), otherLocator]

    it "should match locators ignoring version" $ do
      let myForkLocatorV1 = Locator "go" "github.com/myorg/gin" (Just "v1.9.1")
          myForkLocatorV2 = Locator "go" "github.com/myorg/gin" (Just "v2.0.0")
          baseLocator = Locator "go" "github.com/gin-gonic/gin" Nothing
          translationMap = Map.singleton (toProjectLocator myForkLocatorV1) baseLocator
          translateLocator = simpleTranslate translationMap
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

      let translated = translateSourceUnitLocators translateLocator sourceUnit
      let translatedImports = buildImports <$> sourceUnitBuild translated

      -- Should match myForkLocatorV2 even though it has a different version, because we match by fetcher+project only
      translatedImports `shouldBe` Just [Locator "go" "github.com/gin-gonic/gin" (Just "v2.0.0")]

    it "should handle SourceUnit without build" $ do
      let translateLocator = id  -- No translation
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

      let translated = translateSourceUnitLocators translateLocator sourceUnit

      -- Should remain unchanged
      translated `shouldBe` sourceUnit

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
