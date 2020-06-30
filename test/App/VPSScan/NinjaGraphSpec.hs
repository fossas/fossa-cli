module App.VPSScan.NinjaGraphSpec
  ( spec
  ) where

import Prologue

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import App.VPSScan.NinjaGraph
import App.VPSScan.Types
import Data.Text.Encoding
import Test.Hspec
import Control.Carrier.Diagnostics
import Text.URI (emptyURI)


-- out/target/product/coral/obj/STATIC_LIBRARIES/libgptutils_intermediates/gpt-utils.o: #deps 2, deps mtime 1583962189 (VALID)
--     device/google/coral/gpt-utils/gpt-utils.cpp
--     external/libcxx/include/stdio.h
--     external/libcxx/include/__config

inputOne :: DepsDependency
inputOne = DepsDependency { dependencyPath = "device/google/coral/gpt-utils/gpt-utils.cpp"
                          , dependencyComponentName = Nothing
                          , hasDependencies = False
                          }

dependencyOneA :: DepsDependency
dependencyOneA = DepsDependency { dependencyPath = "external/libcxx/include/stdio.h"
                                , dependencyComponentName = Nothing
                                , hasDependencies = False
                                }

dependencyOneB :: DepsDependency
dependencyOneB = DepsDependency { dependencyPath = "external/libcxx/include/__config"
                                , dependencyComponentName = Nothing
                                , hasDependencies = False
                                }

targetOne :: DepsTarget
targetOne = DepsTarget { targetPath = "out/target/product/coral/obj/STATIC_LIBRARIES/libgptutils_intermediates/gpt-utils.o"
                       , targetDependencies = [dependencyOneA, dependencyOneB]
                       , targetInputs = [inputOne]
                       , targetComponentName = Nothing
                       }

-- out/target/product/coral/obj/APPS/Settings_intermediates/package.apk: #deps 0, deps mtime 1583991062 (VALID)
targetTwo :: DepsTarget
targetTwo = DepsTarget { targetPath = "out/target/product/coral/obj/APPS/Settings_intermediates/package.apk"
                       , targetDependencies = []
                       , targetInputs = []
                       , targetComponentName = Nothing
                       }
-- out/target/product/coral/obj/JAVA_LIBRARIES/wifi-service_intermediates/dexpreopt.zip: #deps 2, deps mtime 1583991124 (VALID)
--     out/soong/host/linux-x86/bin/dex2oatd
--     out/soong/host/linux-x86/bin/profman
--     out/soong/host/linux-x86/bin/soong_zip

inputThree :: DepsDependency
inputThree = DepsDependency { dependencyPath = "out/soong/host/linux-x86/bin/dex2oatd"
                          , dependencyComponentName = Nothing
                          , hasDependencies = True
                          }

dependencyThreeA :: DepsDependency
dependencyThreeA = DepsDependency { dependencyPath = "out/soong/host/linux-x86/bin/profman"
                                , dependencyComponentName = Nothing
                                , hasDependencies = True
                                }

dependencyThreeB :: DepsDependency
dependencyThreeB = DepsDependency { dependencyPath = "out/soong/host/linux-x86/bin/soong_zip"
                                , dependencyComponentName = Nothing
                                , hasDependencies = True
                                }

targetThree :: DepsTarget
targetThree = DepsTarget { targetPath = "out/target/product/coral/obj/JAVA_LIBRARIES/wifi-service_intermediates/dexpreopt.zip"
                       , targetDependencies = [dependencyThreeA, dependencyThreeB]
                       , targetInputs = [inputThree]
                       , targetComponentName = Nothing
                       }

smallNinjaDepsTargets :: [DepsTarget]
smallNinjaDepsTargets =  [targetOne, targetTwo, targetThree]

spec :: Spec
spec = do
  smallNinjaDeps <- runIO (TIO.readFile "test/App/VPSScan/testdata/small-ninja-deps")

  describe "scanNinjaDeps" $
    it "parses a small ninja deps file and generates a dependency graph" $ do
      eitherScanned <- runDiagnostics $ scanNinjaDeps ninjaGraphOpts (encodeUtf8 smallNinjaDeps)
      case eitherScanned of
        Left _ -> expectationFailure (T.unpack "could not parse ninja deps")
        Right scanned -> resultValue scanned `shouldMatchList` smallNinjaDepsTargets
      where
        ninjaGraphOpts = NinjaGraphOpts { ninjaGraphNinjaPath  = Nothing
                                        , lunchTarget = Nothing
                                        , depsGraphScotlandYardUrl = emptyURI
                                        }

