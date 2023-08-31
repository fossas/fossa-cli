module Maven.PomStrategySpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Strategy.Maven.Pom (MavenPackage (..), buildMavenPackage, interpolateProperties)
import Strategy.Maven.Pom.PomFile
import Test.Hspec

spec :: Spec
spec = do
  describe "interpolateProperties" $ do
    let pom = Pom (MavenCoordinate "MYGROUP" "MYARTIFACT" "MYVERSION") Nothing Map.empty Map.empty Map.empty []
    it "should work for built-in properties" $ do
      interpolateProperties pom "${project.groupId}" `shouldBe` "MYGROUP"
      interpolateProperties pom "${project.artifactId}" `shouldBe` "MYARTIFACT"
      interpolateProperties pom "${project.version}" `shouldBe` "MYVERSION"

    it "should prefer user-specified properties over computed ones" $ do
      let pom' = pom{pomProperties = Map.singleton "project.groupId" "OTHERGROUP"}
      interpolateProperties pom' "${project.groupId}" `shouldBe` "OTHERGROUP"

    it "should work in the middle of strings" $ do
      interpolateProperties pom "foo${project.groupId}bar" `shouldBe` "fooMYGROUPbar"

    it "should interpolate multiple properties" $ do
      interpolateProperties pom "${project.groupId}${project.artifactId}" `shouldBe` "MYGROUPMYARTIFACT"

    it "should not infinitely recurse when interpolating a property that is interpolated to itself" $ do
      let pom' = pom{pomProperties = Map.singleton "project.groupId" "${project.groupId"}
      interpolateProperties pom' "${project.groupId}" `shouldBe` "project.groupId"

  describe "buildMavenPackage" $ do
    let pom = Pom (MavenCoordinate "MYGROUP" "MYARTIFACT" "MYVERSION") Nothing Map.empty Map.empty Map.empty []
    it "should interpolate properties in groupId/artifactId/version" $ do
      let result =
            buildMavenPackage
              pom
              "${project.groupId}"
              "${project.artifactId}"
              ( MvnDepBody
                  { depVersion = Just "${project.version}"
                  , depClassifier = Nothing
                  , depScope = Nothing
                  , depOptional = Nothing
                  }
              )
      result `shouldBe` MavenPackage "MYGROUP" "MYARTIFACT" (Just "MYVERSION")
