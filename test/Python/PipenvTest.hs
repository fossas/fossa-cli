module Python.PipenvTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input

import qualified Graph as G
import           Strategy.Python.Pipenv

import GraphUtil
import Test.Tasty.Hspec

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

depOne :: G.Dependency
depOne = G.Dependency
  { dependencyType = G.PipType
  , dependencyName = "pkgOne"
  , dependencyVersion = Just "1.0.0"
  , dependencyLocations = []
  , dependencyTags = M.fromList [("environment", ["development"])]
  }

depTwo :: G.Dependency
depTwo = G.Dependency
  { dependencyType = G.PipType
  , dependencyName = "pkgTwo"
  , dependencyVersion = Just "2.0.0"
  , dependencyLocations = ["https://my-package-index/"]
  , dependencyTags = M.fromList [("environment", ["production"])]
  }

depThree :: G.Dependency
depThree = G.Dependency
  { dependencyType = G.PipType
  , dependencyName = "pkgThree"
  , dependencyVersion = Just "3.0.0"
  , dependencyLocations = []
  , dependencyTags = M.fromList [("environment", ["production"])]
  }

spec_analyze :: Spec
spec_analyze = do
  describe "analyzeWithCmd" $ do
    it "should use pipenv output for edges and tags" $ do
      let result = analyzeWithCmd
            & runInputConst @PipfileLock pipfileLock
            & runInputConst @[PipenvGraphDep] pipenvOutput
            & run

      expectDeps [depOne, depTwo, depThree] result
      expectDirect [depOne, depTwo] result
      expectEdges [(depTwo, depThree)] result

  describe "analyzeNoCmd" $ do
    it "should set all dependencies as direct" $ do
      let result = analyzeNoCmd
            & runInputConst @PipfileLock pipfileLock
            & run

      expectDeps [depOne, depTwo, depThree] result
      expectDirect [depOne, depTwo, depThree] result
      expectEdges [] result
