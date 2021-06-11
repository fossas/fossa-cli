{-# LANGUAGE TemplateHaskell #-}

module Discovery.FiltersSpec (
  spec,
) where

import Data.Set qualified as S
import Discovery.Filters
import Path
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Types (BuildTarget (..))

spec :: Spec
spec = do
  describe "filterParser" $ do
    it "should parse both types of projects" $ do
      runParser filterParser "" "foo@bar" `shouldParse` (ProjectFilter "foo" $(mkRelDir "bar"))
      runParser filterParser "" "foo@bar/baz" `shouldParse` (ProjectFilter "foo" $(mkRelDir "bar/baz"))
      runParser filterParser "" "foo@bar/baz:quux" `shouldParse` (TargetFilter "foo" $(mkRelDir "bar/baz") (BuildTarget "quux"))
    it "should fail on malformed input" $ do
      runParser filterParser "" `shouldFailOn` "foo@bar:"
      runParser filterParser "" `shouldFailOn` "foo@"
    it "should work for a subset of weird parse cases" $ do
      runParser filterParser "" "foo@bar@baz" `shouldParse` ProjectFilter "foo" $(mkRelDir "bar@baz")
      runParser filterParser "" "foo@\127" `shouldParse` ProjectFilter "foo" $(mkRelDir "\127")

  describe "applyFilter" $ do
    it "should allow all BuildTargets when a project filter succeeds" $ do
      let filterFooAtBar = ProjectFilter "foo" $(mkRelDir "bar")
      applyFilter filterFooAtBar "foo" $(mkRelDir "bar") S.empty `shouldBe` Just S.empty
      applyFilter filterFooAtBar "foo" $(mkRelDir "bar") (S.singleton (BuildTarget "baz")) `shouldBe` (Just (S.singleton (BuildTarget "baz")))

    it "should fail with Nothing" $ do
      let filterFooAtBar = ProjectFilter "foo" $(mkRelDir "bar")
      applyFilter filterFooAtBar "not" $(mkRelDir "ok") S.empty `shouldBe` Nothing
      applyFilter filterFooAtBar "not" $(mkRelDir "ok") (S.singleton (BuildTarget "baz")) `shouldBe` Nothing

    it "should only allow a matching BuildTarget" $ do
      let filterFooAtBarBaz = TargetFilter "foo" $(mkRelDir "bar") (BuildTarget "baz")
      applyFilter filterFooAtBarBaz "foo" $(mkRelDir "bar") S.empty `shouldBe` Nothing
      applyFilter filterFooAtBarBaz "foo" $(mkRelDir "bar") (S.singleton (BuildTarget "baz")) `shouldBe` (Just (S.singleton (BuildTarget "baz")))
      applyFilter filterFooAtBarBaz "foo" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "quux"]) `shouldBe` (Just (S.singleton (BuildTarget "baz")))

  describe "applyFilters" $ do
    it "should succeed when any filters succeed" $ do
      let filterFooAtBar = ProjectFilter "foo" $(mkRelDir "bar")
      let filterBazAtQuux = ProjectFilter "baz" $(mkRelDir "quux")
      applyFilters [filterFooAtBar, filterBazAtQuux] "foo" $(mkRelDir "bar") (S.singleton (BuildTarget "ok"))
        `shouldBe` Just (S.singleton (BuildTarget "ok"))
      applyFilters [filterFooAtBar, filterBazAtQuux] "foo" $(mkRelDir "bar") S.empty
        `shouldBe` Just S.empty

    it "should fail when all filters fail" $ do
      let filterFooAtBar = ProjectFilter "foo" $(mkRelDir "bar")
      let filterBazAtQuux = ProjectFilter "baz" $(mkRelDir "quux")
      applyFilters [filterFooAtBar, filterBazAtQuux] "not" $(mkRelDir "ok") S.empty
        `shouldBe` Nothing

    it "should union successful filter results" $ do
      let filterFooAtBarBaz = TargetFilter "foo" $(mkRelDir "bar") (BuildTarget "baz")
      let filterFooAtBarOk = TargetFilter "foo" $(mkRelDir "bar") (BuildTarget "ok")
      applyFilters [filterFooAtBarBaz, filterFooAtBarOk] "foo" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "ok", BuildTarget "notok"])
        `shouldBe` Just (S.fromList [BuildTarget "baz", BuildTarget "ok"])
