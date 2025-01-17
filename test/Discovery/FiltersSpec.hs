{-# LANGUAGE TemplateHaskell #-}

module Discovery.FiltersSpec (
  spec,
) where

import Control.Carrier.Reader (run, runReader)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty)
import Data.String.Conversion (ToText (toText))
import Data.Text qualified as Text
import Discovery.Filters (
  AllFilters (AllFilters),
  Exclude,
  FilterCombination,
  Include,
  applyFilters,
  comboExclude,
  comboInclude,
  pathAllowed,
  toolAllowed,
  withToolFilter,
 )
import Path (Dir, Path, Rel, mkRelDir)
import Test.Fixtures (excludePath)
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  expectationFailure,
  it,
  shouldBe,
 )
import Types (
  BuildTarget (..),
  DiscoveredProjectType (..),
  FoundTargets (..),
  TargetFilter (..),
 )

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

  describe "Matching primitives" $ do
    describe "Tool-based matching" $ do
      it "should exclude tools correctly" $ do
        toolAllowed (excludeTool CargoProjectType) CargoProjectType `shouldBe` False
        toolAllowed (excludeTool GomodProjectType) CargoProjectType `shouldBe` True

      it "should include tools correctly" $ do
        toolAllowed (includeTool CargoProjectType) CargoProjectType `shouldBe` True
        toolAllowed (includeTool GomodProjectType) CargoProjectType `shouldBe` False

      -- Multiple filters
      it "should include multiple tools correctly" $ do
        let filts = includeTool CargoProjectType <> includeTool GomodProjectType
        toolAllowed filts CargoProjectType `shouldBe` True
        toolAllowed filts GomodProjectType `shouldBe` True
        toolAllowed filts SetuptoolsProjectType `shouldBe` False

      it "should exclude multiple tools correctly" $ do
        let filts = excludeTool CargoProjectType <> excludeTool GomodProjectType
        toolAllowed filts CargoProjectType `shouldBe` False
        toolAllowed filts GomodProjectType `shouldBe` False
        toolAllowed filts SetuptoolsProjectType `shouldBe` True

      it "should reject conflicted tools" $ do
        -- Conflicting filters, members of exclude are NEVER allowed
        toolAllowed (includeTool CargoProjectType <> excludeTool CargoProjectType) CargoProjectType `shouldBe` False

      it "should not exclude tools present only in project filters" $ do
        let filters = excludeProject CargoProjectType $(mkRelDir "no-go") <> excludeTool GomodProjectType
        toolAllowed filters CargoProjectType `shouldBe` True
        toolAllowed filters GomodProjectType `shouldBe` False

    describe "Path-based matching" $ do
      it "should include paths correctly" $ do
        pathAllowed (includePath $(mkRelDir "hello")) $(mkRelDir "hello") `shouldBe` True
        pathAllowed (includePath $(mkRelDir "NOPE")) $(mkRelDir "Yeah") `shouldBe` False

      it "should include all parents" $ do
        let child = includePath $(mkRelDir "a/b/c")
        pathAllowed child $(mkRelDir "a") `shouldBe` True
        pathAllowed child $(mkRelDir "a/b") `shouldBe` True
        pathAllowed child $(mkRelDir "a/b/c") `shouldBe` True
        pathAllowed child $(mkRelDir "a/d/c") `shouldBe` False
        pathAllowed child $(mkRelDir "a/c") `shouldBe` False

      it "should include all children" $ do
        let parent = includePath $(mkRelDir "a/b")
        pathAllowed parent $(mkRelDir "a/b") `shouldBe` True
        pathAllowed parent $(mkRelDir "a/b/c") `shouldBe` True
        pathAllowed parent $(mkRelDir "a/b/r") `shouldBe` True
        pathAllowed parent $(mkRelDir "a/b/c/d/e/f/g/h") `shouldBe` True
        pathAllowed parent $(mkRelDir "a/c") `shouldBe` False

      it "should exclude paths correctly" $ do
        pathAllowed (excludePath $(mkRelDir "Nope")) $(mkRelDir "No") `shouldBe` True
        pathAllowed (excludePath $(mkRelDir "Bad")) $(mkRelDir "Bad") `shouldBe` False

      it "should match sub-dir paths correctly" $ do
        -- Subdir matching
        pathAllowed (excludePath $(mkRelDir "hello")) $(mkRelDir "hello/world") `shouldBe` False

      it "should reject conflicted paths" $ do
        -- Conflicting filters
        let conflict = $(mkRelDir "conflict")
        pathAllowed (excludePath conflict <> includePath conflict) conflict `shouldBe` False

      -- Big tests: multiple filters, subdirectory matching
      it "should should handle multi-matching with subdirs" $ do
        let bigFilters = includePath $(mkRelDir "a") <> excludePath $(mkRelDir "a/b/c")
        pathAllowed bigFilters $(mkRelDir "a/b/c") `shouldBe` False
        pathAllowed bigFilters $(mkRelDir "a/b/d") `shouldBe` True

  describe "tool filtering helpers" $ do
    it "should return an empty list when the tool is not allowed" $ do
      let filters = excludeTool CargoProjectType
          result = run . runReader filters $ withToolFilter CargoProjectType $ pure [True]
      if null result
        then pure ()
        else expectationFailure "withToolFilter returned non-empty"

    it "should return the list continuation when the tool is allowed" $ do
      let filters = excludeTool GomodProjectType
          result = run . runReader filters $ withToolFilter CargoProjectType $ pure [True]
      when (null result) $
        expectationFailure "withToolFilter returned non-empty"

    it "should return the continuation when the filters are empty" $ do
      let filters = mempty :: AllFilters
          result = run . runReader filters $ withToolFilter CargoProjectType $ pure [True]
      when (null result) $
        expectationFailure "withToolFilter returned non-empty"

testHarness :: FilterCombination Include -> FilterCombination Exclude -> [((Text.Text, Path Rel Dir), FoundTargets, Maybe FoundTargets)] -> Expectation
testHarness include exclude = traverse_ testSingle
  where
    testSingle ((buildtool, dir), targets, expected) = applyFilters (AllFilters include exclude) buildtool dir targets `shouldBe` expected

excludeTool :: DiscoveredProjectType -> AllFilters
excludeTool tool = AllFilters mempty $ comboExclude [TypeTarget $ toText tool] mempty

excludeProject :: DiscoveredProjectType -> Path Rel Dir -> AllFilters
excludeProject ty path = AllFilters mempty $ comboExclude [TypeDirTarget (toText ty) path] mempty

includePath :: Path Rel Dir -> AllFilters
includePath path = AllFilters include mempty
  where
    include = comboInclude mempty [path]

includeTool :: DiscoveredProjectType -> AllFilters
includeTool tool = AllFilters include mempty
  where
    include = comboInclude [TypeTarget $ toText tool] mempty
