module Maven.PomStrategySpec (
  spec,
) where

import Data.Map.Strict qualified as M
import Strategy.Maven.Pom (interpolateProperties)
import Strategy.Maven.Pom.PomFile
import Test.Hspec

spec :: Spec
spec = do
  describe "interpolateProperties" $ do
    let pom = Pom (MavenCoordinate "MYGROUP" "MYARTIFACT" "MYVERSION") Nothing M.empty M.empty M.empty []
    it "should work for built-in properties" $ do
      interpolateProperties pom "${project.groupId}" `shouldBe` "MYGROUP"
      interpolateProperties pom "${project.artifactId}" `shouldBe` "MYARTIFACT"
      interpolateProperties pom "${project.version}" `shouldBe` "MYVERSION"

    it "should prefer user-specified properties over computed ones" $ do
      let pom' = pom{pomProperties = M.singleton "project.groupId" "OTHERGROUP"}
      interpolateProperties pom' "${project.groupId}" `shouldBe` "OTHERGROUP"

    it "should work in the middle of strings" $ do
      interpolateProperties pom "foo${project.groupId}bar" `shouldBe` "fooMYGROUPbar"

    it "should interpolate multiple properties" $ do
      interpolateProperties pom "${project.groupId}${project.artifactId}" `shouldBe` "MYGROUPMYARTIFACT"
