module Gradle.CommonSpec (
  spec,
) where

import Strategy.Gradle.Common (PackageName (PackageName), getWarningMessages, packagePathsWithJson)
import Test.Hspec

spec :: Spec
spec = do
  describe "packagePathsWithJson" $ do
    it "should break package and jsonText correctly for project with _" $ do
      packagePathsWithJson ["sub-project_{}"] `shouldBe` [(PackageName "sub-project", "{}")]
      packagePathsWithJson ["sub_project_{}"] `shouldBe` [(PackageName "sub_project", "{}")]
      packagePathsWithJson ["sub-project__{}"] `shouldBe` [(PackageName "sub-project_", "{}")]
      packagePathsWithJson ["subProject_{}"] `shouldBe` [(PackageName "subProject", "{}")]

  describe "getWarningMessages" $ do
    it "should get only warning messages" $ do
      getWarningMessages "FOSSA-WARNING (someScope): some warning message" `shouldBe` ["some warning message"]
      getWarningMessages "DEBUG (someScope): some debug message" `shouldBe` []
      getWarningMessages "ERROR (someScope): some error message" `shouldBe` []
      getWarningMessages "  some exception warning message" `shouldBe` []