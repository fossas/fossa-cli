module Cocoapods.PodfileLockSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Yaml (decodeEither')
import DepTypes (
  DepType (PodType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Cocoapods.PodfileLock (
  Dep (Dep),
  Pod (Pod),
  Section (..),
  buildGraph,
  toSections,
 )
import Test.Hspec qualified as T

depOf :: Text -> Maybe Text -> Dependency
depOf name version =
  Dependency
    { dependencyType = PodType
    , dependencyName = name
    , dependencyVersion = CEq <$> version
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

dependencyOne :: Dependency
dependencyOne = depOf "one" (Just "1.0.0")

dependencyTwo :: Dependency
dependencyTwo = depOf "two" (Just "2.0.0")

dependencyThree :: Dependency
dependencyThree = depOf "three" (Just "3.0.0")

dependencyFour :: Dependency
dependencyFour = depOf "four" (Just "4.0.0")

dependencyAbnormalName :: Dependency
dependencyAbnormalName = depOf "ab-normal/+name" (Just "2.0.0")

dependencyNotSoSafeName :: Dependency
dependencyNotSoSafeName = depOf "not-so-safe-name" (Just "2.0.0")

dependencyGitSourced :: Dependency
dependencyGitSourced = depOf "some-dep-sourced-from-git" (Just "2.0.0")

dependencyGitTagged :: Dependency
dependencyGitTagged = depOf "depWithTag" (Just "2.0.0")

dependencyTwoDepA :: Dependency
dependencyTwoDepA = depOf "two_dep_A" Nothing

dependencyTwoDepB :: Dependency
dependencyTwoDepB = depOf "two-dep-B" Nothing

podSection :: Section
podSection =
  PodSection
    [ Pod "one" "1.0.0" [Dep "two", Dep "three", Dep "ab-normal/+name"]
    , Pod "two" "2.0.0" [Dep "two_dep_A", Dep "two-dep-B"]
    , Pod "three" "3.0.0" [Dep "four"]
    , Pod "ab-normal/+name" "2.0.0" []
    , Pod "four" "4.0.0" []
    , Pod "not-so-safe-name" "2.0.0" []
    , Pod "some-dep-sourced-from-git" "2.0.0" []
    , Pod "depWithTag" "2.0.0" []
    ]

dependencySection :: Section
dependencySection =
  DependencySection
    [ Dep "one"
    , Dep "three"
    , Dep "not-so-safe-name"
    , Dep "some-dep-sourced-from-git"
    , Dep "depWithTag"
    ]

spec :: T.Spec
spec = do
  T.describe "podfile lock analyzer" $
    T.it "produces the expected output" $ do
      let graph = buildGraph [podSection, dependencySection]
      expectDeps
        [ dependencyOne
        , dependencyTwo
        , dependencyThree
        , dependencyAbnormalName
        , dependencyFour
        , dependencyNotSoSafeName
        , dependencyGitSourced
        , dependencyGitTagged
        , dependencyTwoDepA
        , dependencyTwoDepB
        ]
        graph
      expectDirect
        [ dependencyOne
        , dependencyThree
        , dependencyNotSoSafeName
        , dependencyGitSourced
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
  T.describe "podfile lock parser" $ do
    T.it "parses pod and dependency sections" $
      case toSections <$> decodeEither' podLockFile of
        Left err -> T.expectationFailure $ "failed to parse: " <> show err
        Right result -> do
          result `T.shouldContain` [podSection]
          result `T.shouldContain` [dependencySection]
