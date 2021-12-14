module Gradle.CommonSpec (
  spec,
) where

import Strategy.Gradle.Common (PackageName (PackageName), getLogWithPrefix, packagePathsWithJson)
import Test.Hspec

spec :: Spec
spec = do
  describe "packagePathsWithJson" $ do
    it "should break package and jsonText correctly for project with _" $ do
      packagePathsWithJson ["sub-project_{}"] `shouldBe` [(PackageName "sub-project", "{}")]
      packagePathsWithJson ["sub_project_{}"] `shouldBe` [(PackageName "sub_project", "{}")]
      packagePathsWithJson ["sub-project__{}"] `shouldBe` [(PackageName "sub-project_", "{}")]
      packagePathsWithJson ["subProject_{}"] `shouldBe` [(PackageName "subProject", "{}")]

  describe "getLogWithPrefix" $ do
    it "should get only messages matching prefix" $ do
      getLogWithPrefix "FOSSA-WARNING (someScope): some warning message" "FOSSA-WARNING" `shouldBe` ["some warning message"]
      getLogWithPrefix "ERROR (someScope): some error message" "ERROR" `shouldBe` ["some error message"]
      getLogWithPrefix "DEBUG (someScope): some debug message" "FOSSA-WARNING" `shouldBe` []
