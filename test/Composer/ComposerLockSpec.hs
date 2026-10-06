module Composer.ComposerLockSpec (
  spec,
) where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes
import GraphUtil
import Strategy.Composer
import Test.Hspec

dependencyOne :: Dependency
dependencyOne =
  Dependency
    { dependencyType = ComposerType
    , dependencyName = "one"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

dependencyTwo :: Dependency
dependencyTwo =
  Dependency
    { dependencyType = ComposerType
    , dependencyName = "two"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

dependencyThree :: Dependency
dependencyThree =
  Dependency
    { dependencyType = ComposerType
    , dependencyName = "three"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

dependencyFour :: Dependency
dependencyFour =
  Dependency
    { dependencyType = ComposerType
    , dependencyName = "four"
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

dependencyFive :: Dependency
dependencyFive =
  Dependency
    { dependencyType = ComposerType
    , dependencyName = "five"
    , dependencyVersion = Just (CEq "5.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

dependencySourceless :: Dependency
dependencySourceless =
  Dependency
    { dependencyType = ComposerType
    , dependencyName = "sourceless"
    , dependencyVersion = Just (CEq "5.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

monolog :: Dependency
monolog =
  Dependency
    { dependencyType = ComposerType
    , dependencyName = "monolog/monolog"
    , dependencyVersion = Just (CEq "3.8.1")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

psrLog :: Dependency
psrLog =
  Dependency
    { dependencyType = ComposerType
    , dependencyName = "psr/log"
    , dependencyVersion = Just (CEq "3.0.2")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

spec :: Spec
spec = do
  testFile <- runIO (BS.readFile "test/Composer/testdata/composer.lock")
  platformFile <- runIO (BS.readFile "test/Composer/testdata/platform_requirements.lock")
  describe "composer.lock analyzer" $
    it "reads a file and constructs an accurate graph" $
      case eitherDecodeStrict testFile of
        Right res -> do
          let graph = buildGraph res
          expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive, dependencySourceless] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive, dependencySourceless] graph
          expectEdges [(dependencyOne, dependencyTwo), (dependencyOne, dependencyTwo), (dependencyTwo, dependencyFour)] graph
        Left err -> expectationFailure $ show err

  describe "platform requirements" $ do
    it "are not reported as dependencies" $
      case eitherDecodeStrict platformFile of
        Right res -> do
          let graph = buildGraph res
          expectDeps [monolog, psrLog] graph
          expectDirect [monolog, psrLog] graph
          expectEdges [(monolog, psrLog)] graph
        Left err -> expectationFailure $ show err

    it "are recognized by name" $
      filter (not . isPlatformPackage) platformNames `shouldBe` []

    it "do not include registry packages with a platform-like prefix" $
      filter isPlatformPackage packageNames `shouldBe` []
  where
    platformNames =
      [ "php"
      , "php-64bit"
      , "hhvm"
      , "ext-json"
      , "lib-pcre"
      , "composer"
      , "composer-plugin-api"
      , "composer-runtime-api"
      , "PHP"
      , "ext-PDO"
      ]
    packageNames =
      [ "php-http/guzzle7-adapter"
      , "composer/semver"
      , "ext-foo/bar"
      , "psr/log"
      ]
