module Maven.PomClosureSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Strategy.Maven.Pom.Closure (extractSubmoduleFromCoordinate, submodulesFromCoordinate)
import Strategy.Maven.Pom.PomFile (MavenCoordinate (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "extractSubmoduleFromCoordinate" $ do
    it "should extract group:artifact from a coordinate" $ do
      extractSubmoduleFromCoordinate (MavenCoordinate "org.apache.poi" "poi-ooxml" "5.2.5")
        `shouldBe` "org.apache.poi:poi-ooxml"

    it "should ignore the version component" $ do
      extractSubmoduleFromCoordinate (MavenCoordinate "g" "a" "9.9.9")
        `shouldBe` "g:a"

  describe "submodulesFromCoordinate" $ do
    it "should return an empty set for an empty map" $ do
      submodulesFromCoordinate (Map.empty :: Map.Map MavenCoordinate ()) `shouldBe` Set.empty

    it "should extract a single submodule from a one-entry map" $ do
      let m = Map.singleton (MavenCoordinate "g" "a" "v") ()
      submodulesFromCoordinate m `shouldBe` Set.fromList ["g:a"]

    it "should deduplicate coordinates with the same group and artifact" $ do
      let m =
            Map.fromList
              [ (MavenCoordinate "g" "a" "1.0", ())
              , (MavenCoordinate "g" "a" "2.0", ()) -- same group:artifact, different version
              ]
      submodulesFromCoordinate m `shouldBe` Set.fromList ["g:a"]

    it "should include multiple distinct coordinates" $ do
      let m =
            Map.fromList
              [ (MavenCoordinate "org.apache.poi" "poi" "5.2.5", ())
              , (MavenCoordinate "org.apache.logging.log4j" "log4j-core" "2.21.1", ())
              , (MavenCoordinate "com.google.guava" "guava" "31.1", ())
              ]
      submodulesFromCoordinate m
        `shouldBe` Set.fromList
          [ "org.apache.poi:poi"
          , "org.apache.logging.log4j:log4j-core"
          , "com.google.guava:guava"
          ]
