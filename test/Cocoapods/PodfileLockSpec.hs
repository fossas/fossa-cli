module Cocoapods.PodfileLockSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Yaml (decodeEither')
import DepTypes (
  DepType (GitType, PodType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Cocoapods.PodfileLock (
  Dep (Dep),
  ExternalGitSource (..),
  ExternalSource (..),
  Pod (Pod),
  PodLock (..),
  buildGraph,
 )
import Test.Hspec qualified as T

podDepOf :: Text -> Maybe Text -> Dependency
podDepOf name version =
  Dependency
    { dependencyType = PodType
    , dependencyName = name
    , dependencyVersion = CEq <$> version
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

gitDepOf :: Text -> Maybe Text -> Dependency
gitDepOf name version =
  Dependency
    { dependencyType = GitType
    , dependencyName = name
    , dependencyVersion = CEq <$> version
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

dependencyOne :: Dependency
dependencyOne = podDepOf "one" (Just "1.0.0")

dependencyTwo :: Dependency
dependencyTwo = podDepOf "two" (Just "2.0.0")

dependencyThree :: Dependency
dependencyThree = podDepOf "three" (Just "3.0.0")

dependencyFour :: Dependency
dependencyFour = podDepOf "four" (Just "4.0.0")

dependencyAbnormalName :: Dependency
dependencyAbnormalName = podDepOf "ab-normal/+name" (Just "2.0.0")

dependencyGitTagged :: Dependency
dependencyGitTagged = gitDepOf "git@github.example.com:ab/cap.git" (Just "v1.2.3")

dependencyTwoDepA :: Dependency
dependencyTwoDepA = podDepOf "two_dep_A" Nothing

dependencyTwoDepB :: Dependency
dependencyTwoDepB = podDepOf "two-dep-B" Nothing

pods :: [Pod]
pods =
  [ Pod "one" "1.0.0" [Dep "two", Dep "three", Dep "ab-normal/+name"]
  , Pod "two" "2.0.0" [Dep "two_dep_A", Dep "two-dep-B"]
  , Pod "three" "3.0.0" [Dep "four"]
  , Pod "ab-normal/+name" "2.0.0" []
  , Pod "four" "4.0.0" []
  , Pod "depWithTag" "2.0.0" []
  ]

dependencies :: [Dep]
dependencies =
  [ Dep "one"
  , Dep "three"
  , Dep "depWithTag"
  ]

externalSources :: Map.Map Text ExternalSource
externalSources =
  Map.fromList
    [ ("depWithTag", ExternalGitType $ ExternalGitSource "git@github.example.com:ab/cap.git" (Just "v1.2.3") Nothing Nothing)
    , ("depWithBranch", ExternalGitType $ ExternalGitSource "git@github.example.com:ab/cap.git" Nothing Nothing $ Just "main")
    , ("depWithCommit", ExternalGitType $ ExternalGitSource "git@github.example.com:ab/cap.git" Nothing (Just "9a9a9") Nothing)
    , ("ChatSecure-Push-iOS", ExternalOtherType)
    , ("ChatSecureCore", ExternalOtherType)
    ]

spec :: T.Spec
spec = do
  T.describe "podfile lock analyzer" $
    T.it "produces the expected output" $ do
      let graph = buildGraph $ PodLock pods dependencies externalSources
      expectDeps
        [ dependencyOne
        , dependencyTwo
        , dependencyThree
        , dependencyAbnormalName
        , dependencyFour
        , dependencyGitTagged
        , dependencyTwoDepA
        , dependencyTwoDepB
        ]
        graph
      expectDirect
        [ dependencyOne
        , dependencyThree
        , dependencyGitTagged
        ]
        graph
      expectEdges
        [ (dependencyOne, dependencyAbnormalName)
        , (dependencyOne, dependencyTwo)
        , (dependencyOne, dependencyThree)
        , (dependencyTwo, dependencyTwoDepA)
        , (dependencyTwo, dependencyTwoDepB)
        , (dependencyThree, dependencyFour)
        ]
        graph

  podLockFile <- T.runIO (BS.readFile "test/Cocoapods/testdata/Podfile.lock")
  T.describe "Podfile.lock parser" $ do
    T.it "should parse content" $
      case decodeEither' podLockFile of
        Left err -> T.expectationFailure $ "failed to parse: " <> show err
        Right result -> result `T.shouldBe` PodLock pods dependencies externalSources
