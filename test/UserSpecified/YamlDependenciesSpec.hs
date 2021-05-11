{-# LANGUAGE TypeApplications #-}

module UserSpecified.YamlDependenciesSpec
  ( spec,
  )
where

import Control.Algebra
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Yaml
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.UserSpecified.YamlDependencies
import Test.Hspec

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = GemType,
        dependencyName = "one",
        dependencyVersion = Nothing,
        dependencyLocations = [],
        dependencyEnvironments = [],
        dependencyTags = M.empty
      }
  direct $
    Dependency
      { dependencyType = URLType,
        dependencyName = "two",
        dependencyVersion = Just (CEq "1.0.0"),
        dependencyLocations = [],
        dependencyEnvironments = [],
        dependencyTags = M.empty
      }

spec :: Spec
spec = do
  testFile <- runIO (BS.readFile "test/UserSpecified/testdata/valid-deps.yaml")
  unsupportedTypeFile <- runIO (BS.readFile "test/UserSpecified/testdata/invalid-deps.yaml")

  describe "yaml user specified dependencies" $ do
    it "works end to end" $
      case decodeEither' testFile of
        Right res -> buildGraph res `shouldBe` expected
        Left err -> expectationFailure $ "failed to parse: " <> show err

    it "fails with unsupported deps" $
      case decodeEither' @UserDependencies unsupportedTypeFile of
        Right res -> expectationFailure $ "Expected a failure to parse due to unsupported dependency, but got: " <> show res
        Left _ -> pure ()
