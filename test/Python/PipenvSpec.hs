module Python.PipenvSpec (
  spec,
) where

import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes
import GraphUtil
import Strategy.Python.Pipenv
import Test.Hspec hiding (xit)

pipfileLock :: PipfileLock
pipfileLock =
  PipfileLock
    { fileMeta =
        PipfileMeta
          [ PipfileSource
              { sourceName = "package-index"
              , sourceUrl = "https://my-package-index/"
              }
          ]
    , fileDefault =
        Map.fromList
          [
            ( "pkgTwo"
            , PipfileDep
                { fileDepVersion = Just "==2.0.0"
                , fileDepIndex = Just "package-index"
                }
            )
          ,
            ( "pkgThree"
            , PipfileDep
                { fileDepVersion = Just "==3.0.0"
                , fileDepIndex = Nothing
                }
            )
          ,
            ( "pkgFour"
            , PipfileDep
                { fileDepVersion = Nothing
                , fileDepIndex = Nothing
                }
            )
          ]
    , fileDevelop =
        Map.fromList
          [
            ( "pkgOne"
            , PipfileDep
                { fileDepVersion = Just "==1.0.0"
                , fileDepIndex = Nothing
                }
            )
          ]
    }

pipenvOutput :: [PipenvGraphDep]
pipenvOutput =
  [ PipenvGraphDep
      { depName = "pkgOne"
      , depInstalled = "1.0.0"
      , depRequired = "==1.0.0"
      , depDependencies = []
      }
  , PipenvGraphDep
      { depName = "pkgTwo"
      , depInstalled = "2.0.0"
      , depRequired = "==2.0.0"
      , depDependencies =
          [ PipenvGraphDep
              { depName = "pkgThree"
              , depInstalled = "3.0.0"
              , depRequired = "==3.0.0"
              , depDependencies = []
              }
          ]
      }
  ]

depOne :: Dependency
depOne =
  Dependency
    { dependencyType = PipType
    , dependencyName = "pkgOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

depTwo :: Dependency
depTwo =
  Dependency
    { dependencyType = PipType
    , dependencyName = "pkgTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = ["https://my-package-index/"]
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

depThree :: Dependency
depThree =
  Dependency
    { dependencyType = PipType
    , dependencyName = "pkgThree"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

depFour :: Dependency
depFour =
  Dependency
    { dependencyType = PipType
    , dependencyName = "pkgFour"
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

spec :: Spec
spec = do
  pipLockFile <- runIO (BS.readFile "test/Python/testdata/Pipfile.lock")

  describe "analyzeWithCmd" $
    it "should use pipenv output for edges and tags" $ do
      let result = buildGraph pipfileLock (Just pipenvOutput)

      expectDeps [depOne, depTwo, depThree] result
      expectDirect [depOne, depTwo] result
      expectEdges [(depTwo, depThree)] result

  describe "analyzeNoCmd" $
    it "should set all dependencies as direct" $ do
      let result = buildGraph pipfileLock Nothing

      expectDeps [depOne, depTwo, depThree, depFour] result
      expectDirect [depOne, depTwo, depThree, depFour] result
      expectEdges [] result

  describe "analyzeNoCmdFromFile" $
    it "should set all dependencies as direct" $ do
      case eitherDecodeStrict pipLockFile of
        Right res -> do
          let result = buildGraph res Nothing
          expectDeps [depOne, depTwo, depThree, depFour] result
          expectDirect [depOne, depTwo, depThree, depFour] result
          expectEdges [] result
        Left _ -> expectationFailure "failed to parse"
