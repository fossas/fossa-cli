module Python.PipenvSpec
  ( spec
  ) where

import qualified Data.Map.Strict as M
import DepTypes
import GraphUtil
import Strategy.Python.Pipenv
import Test.Hspec hiding (xit)

pipfileLock :: PipfileLock
pipfileLock = PipfileLock
  { fileMeta    = PipfileMeta
    [ PipfileSource { sourceName = "package-index"
                    , sourceUrl  = "https://my-package-index/"
                    }
    ]

  , fileDefault = M.fromList
    [ ("pkgTwo", PipfileDep { fileDepVersion = "==2.0.0"
                            , fileDepIndex = Just "package-index"
                            })
    , ("pkgThree", PipfileDep { fileDepVersion = "==3.0.0"
                              , fileDepIndex = Nothing
                              })
    ]

  , fileDevelop = M.fromList
    [ ("pkgOne", PipfileDep { fileDepVersion = "==1.0.0"
                            , fileDepIndex = Nothing
                            })
    ]
  }

pipenvOutput :: [PipenvGraphDep]
pipenvOutput =
  [ PipenvGraphDep { depName         = "pkgOne"
                   , depInstalled    = "1.0.0"
                   , depRequired     = "==1.0.0"
                   , depDependencies = []
                   }
  , PipenvGraphDep { depName = "pkgTwo"
                   , depInstalled = "2.0.0"
                   , depRequired = "==2.0.0"
                   , depDependencies =
                     [ PipenvGraphDep { depName      = "pkgThree"
                                      , depInstalled = "3.0.0"
                                      , depRequired  = "==3.0.0"
                                      , depDependencies = []
                                      }
                     ]
                   }
  ]

depOne :: Dependency
depOne = Dependency
  { dependencyType = PipType
  , dependencyName = "pkgOne"
  , dependencyVersion = Just (CEq "1.0.0")
  , dependencyLocations = []
  , dependencyEnvironments = [EnvDevelopment]
  , dependencyTags = M.empty
  }

depTwo :: Dependency
depTwo = Dependency
  { dependencyType = PipType
  , dependencyName = "pkgTwo"
  , dependencyVersion = Just (CEq "2.0.0")
  , dependencyLocations = ["https://my-package-index/"]
  , dependencyEnvironments = [EnvProduction]
  , dependencyTags = M.empty
  }

depThree :: Dependency
depThree = Dependency
  { dependencyType = PipType
  , dependencyName = "pkgThree"
  , dependencyVersion = Just (CEq "3.0.0")
  , dependencyLocations = []
  , dependencyEnvironments = [EnvProduction]
  , dependencyTags = M.empty
  }

xit :: String -> Expectation -> SpecWith (Arg Expectation)
xit _ _ = it "is an ignored test" $ () `shouldBe` ()

spec :: Spec
spec = do
  describe "analyzeWithCmd" $
    -- FIXME: graphing needs to be refactored to include "reachable" alongside "direct"
    xit "should use pipenv output for edges and tags" $ do
      let result = buildGraph pipfileLock (Just pipenvOutput)

      expectDeps [depOne, depTwo, depThree] result
      expectDirect [depOne, depTwo] result
      expectEdges [(depTwo, depThree)] result

  describe "analyzeNoCmd" $
    it "should set all dependencies as direct" $ do
      let result = buildGraph pipfileLock Nothing

      expectDeps [depOne, depTwo, depThree] result
      expectDirect [depOne, depTwo, depThree] result
      expectEdges [] result
