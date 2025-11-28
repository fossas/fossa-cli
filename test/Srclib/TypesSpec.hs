module Srclib.TypesSpec (spec) where

import Data.Map qualified as Map
import Srclib.Types (
  Locator (..),
  SourceUnit (..),
  SourceUnitBuild (..),
  SourceUnitDependency (..),
  toProjectLocator,
  translateSourceUnitLocators,
 )
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
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
