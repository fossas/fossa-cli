{-# LANGUAGE TemplateHaskell #-}

module Discovery.FiltersSpec (
  spec,
) where

import Data.Foldable (traverse_)
import Data.Set qualified as Set
import Data.Set.NonEmpty
import Data.Text qualified as Text
import Discovery.Filters
import Path
import Test.Hspec
import Types (BuildTarget (..), FoundTargets (..), TargetFilter (..))

spec :: Spec
spec = do
  {-
    Directory Structure
    /foo
     mvn:[]
     gradle:[foo, bar]
     /bar
      mvn:[]
      gradle:[foo, bar]
      /baz
       mvn:[]
    /quux
      mvn:[]

  -}
  describe "FilterCombination filters" $ do
    let mvnFoo = ("mvn", $(mkRelDir "foo"))
        gradleFoo = ("gradle", $(mkRelDir "foo"))
        mvnFooBar = ("mvn", $(mkRelDir "foo/bar"))
        gradleFooBar = ("gradle", $(mkRelDir "foo/bar"))
        mvnFooBarBaz = ("mvn", $(mkRelDir "foo/bar/baz"))
        mvnQuux = ("mvn", $(mkRelDir "quux"))
        gradleTargets = maybe ProjectWithoutTargets FoundTargets (nonEmpty $ Set.fromList [BuildTarget "foo", BuildTarget "bar"])
        fooTargetAssertion = (Just . FoundTargets) =<< nonEmpty (Set.fromList [BuildTarget "foo"])
        barTargetAssertion = (Just . FoundTargets) =<< nonEmpty (Set.fromList [BuildTarget "bar"])
        fooBarTargetAssertion = (Just . FoundTargets) =<< nonEmpty (Set.fromList [BuildTarget "bar", BuildTarget "foo"])

    it "includes an entire directory" $ do
      let include = comboInclude [] [$(mkRelDir "quux")]
          exclude = mempty

      testHarness
        include
        exclude
        [ (mvnFoo, ProjectWithoutTargets, Nothing)
        , (gradleFoo, gradleTargets, Nothing)
        , (mvnFooBar, ProjectWithoutTargets, Nothing)
        , (gradleFooBar, gradleTargets, Nothing)
        , (mvnFooBarBaz, ProjectWithoutTargets, Nothing)
        , (mvnQuux, ProjectWithoutTargets, Just ProjectWithoutTargets)
        ]

    it "includes a subdirectory" $ do
      let include = comboInclude [] [$(mkRelDir "foo/bar")]
          exclude = mempty

      testHarness
        include
        exclude
        [ (mvnFoo, ProjectWithoutTargets, Nothing)
        , (gradleFoo, gradleTargets, Nothing)
        , (mvnFooBar, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (gradleFooBar, gradleTargets, fooBarTargetAssertion)
        , (mvnFooBarBaz, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (mvnQuux, ProjectWithoutTargets, Nothing)
        ]

    it "excludes a directory" $ do
      let include = mempty
          exclude = comboExclude [] [$(mkRelDir "foo")]

      testHarness
        include
        exclude
        [ (mvnFoo, ProjectWithoutTargets, Nothing)
        , (gradleFoo, gradleTargets, Nothing)
        , (mvnFooBar, ProjectWithoutTargets, Nothing)
        , (gradleFooBar, gradleTargets, Nothing)
        , (mvnFooBarBaz, ProjectWithoutTargets, Nothing)
        , (mvnQuux, ProjectWithoutTargets, Just ProjectWithoutTargets)
        ]

    it "excludes a subdirectory" $ do
      let include = mempty
          exclude = comboExclude [] [$(mkRelDir "foo/bar")]

      testHarness
        include
        exclude
        [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (gradleFoo, gradleTargets, fooBarTargetAssertion)
        , (mvnFooBar, ProjectWithoutTargets, Nothing)
        , (gradleFooBar, gradleTargets, Nothing)
        , (mvnFooBarBaz, ProjectWithoutTargets, Nothing)
        , (mvnQuux, ProjectWithoutTargets, Just ProjectWithoutTargets)
        ]

    it "excludes a target in a subdirectory" $ do
      let include = mempty
          exclude = comboExclude [TypeDirTargetTarget "gradle" $(mkRelDir "foo/bar") (BuildTarget "foo")] []

      testHarness
        include
        exclude
        [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (gradleFoo, gradleTargets, fooBarTargetAssertion)
        , (mvnFooBar, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (gradleFooBar, gradleTargets, barTargetAssertion)
        , (mvnFooBarBaz, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (mvnQuux, ProjectWithoutTargets, Just ProjectWithoutTargets)
        ]

    it "excludes a subdirectory of an included directory" $ do
      let include = comboInclude [] [$(mkRelDir "foo")]
          exclude = comboExclude [] [$(mkRelDir "foo/bar")]

      testHarness
        include
        exclude
        [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (gradleFoo, gradleTargets, fooBarTargetAssertion)
        , (mvnFooBar, ProjectWithoutTargets, Nothing)
        , (gradleFooBar, gradleTargets, Nothing)
        , (mvnFooBarBaz, ProjectWithoutTargets, Nothing)
        , (mvnQuux, ProjectWithoutTargets, Nothing)
        ]

    it "excludes a build-target in an included directory" $ do
      let include = comboInclude [] [$(mkRelDir "foo")]
          exclude = comboExclude [TypeDirTargetTarget "gradle" $(mkRelDir "foo") (BuildTarget "foo")] []

      testHarness
        include
        exclude
        [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (gradleFoo, gradleTargets, barTargetAssertion)
        , (mvnFooBar, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (gradleFooBar, gradleTargets, fooBarTargetAssertion)
        , (mvnFooBarBaz, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (mvnQuux, ProjectWithoutTargets, Nothing)
        ]

    it "does the thing" $ do
      let include = comboInclude [TypeDirTargetTarget "gradle" $(mkRelDir "foo") (BuildTarget "foo")] [$(mkRelDir "foo")]
          exclude = mempty

      testHarness
        include
        exclude
        [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (gradleFoo, gradleTargets, fooTargetAssertion)
        , (mvnFooBar, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (gradleFooBar, gradleTargets, fooBarTargetAssertion)
        , (mvnFooBarBaz, ProjectWithoutTargets, Just ProjectWithoutTargets)
        , (mvnQuux, ProjectWithoutTargets, Nothing)
        ]

testHarness :: FilterCombination Include -> FilterCombination Exclude -> [((Text.Text, Path Rel Dir), FoundTargets, Maybe FoundTargets)] -> Expectation
testHarness include exclude = traverse_ testSingle
  where
    testSingle ((buildtool, dir), targets, expected) = applyFilters (AllFilters include exclude) buildtool dir targets `shouldBe` expected
