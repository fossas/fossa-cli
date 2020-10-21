module App.Fossa.VPS.NinjaGraphSpec
  ( spec
  ) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.List as List
import App.Fossa.VPS.NinjaGraph
import App.Fossa.VPS.Types
import Data.Text.Encoding
import Test.Hspec
import Control.Carrier.Diagnostics


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

cfiBlacklistDep :: DepsDependency
cfiBlacklistDep = DepsDependency { dependencyPath = "external/compiler-rt/lib/cfi/cfi_blacklist.txt"
                                 , dependencyComponentName = Nothing
                                 , hasDependencies = False
                                 }

integerOverflowBlacklistDep :: DepsDependency
integerOverflowBlacklistDep = DepsDependency { dependencyPath = "build/soong/cc/config/integer_overflow_blacklist.txt"
                                             , dependencyComponentName = Nothing
                                             , hasDependencies = False
                                             }

inetDep :: DepsDependency
inetDep = DepsDependency { dependencyPath = "bionic/libc/include/arpa/inet.h"
                         , dependencyComponentName = Nothing
                         , hasDependencies = False
                         }

bpfLoaderDep :: DepsDependency
bpfLoaderDep = DepsDependency { dependencyPath = "system/bpf/bpfloader/BpfLoader.cpp"
                              , dependencyComponentName = Nothing
                              , hasDependencies = False
                              }

targetWithFirstLevelWeirdnessFix :: DepsTarget
targetWithFirstLevelWeirdnessFix = DepsTarget { targetPath = "out/soong/.intermediates/system/bpf/bpfloader/bpfloader/android_arm64_armv8-a_core/obj/system/bpf/bpfloader/BpfLoader.o"
                                              , targetDependencies = [integerOverflowBlacklistDep, inetDep]
                                              , targetInputs = [bpfLoaderDep]
                                              , targetComponentName = Nothing
                                              }

targetWithSecondLevelWeirdnessFix :: DepsTarget
targetWithSecondLevelWeirdnessFix = DepsTarget { targetPath = "out/soong/.intermediates/system/bpf/bpfloader/bpfloader/android_arm64_armv8-a_core/obj/system/bpf/bpfloader/BpfLoader.o"
                                               , targetDependencies = [cfiBlacklistDep, integerOverflowBlacklistDep, inetDep]
                                               , targetInputs = [bpfLoaderDep]
                                               , targetComponentName = Nothing
                                               }
spec :: Spec
spec = do
  smallNinjaDeps <- runIO (TIO.readFile "test/App/Fossa/VPS/testdata/small-ninja-deps")
  weirdNinjaDeps <- runIO (TIO.readFile "test/App/Fossa/VPS/testdata/ninja-deps-with-weird-targets")

  describe "scanNinjaDeps for a standard ninja deps file" $
    it "parses a small ninja deps file and generates a dependency graph" $ do
      eitherScanned <- runDiagnostics $ scanNinjaDeps (encodeUtf8 smallNinjaDeps)
      case eitherScanned of
        Left _ -> expectationFailure (T.unpack "could not parse ninja deps")
        Right scanned -> resultValue scanned `shouldMatchList` smallNinjaDepsTargets

  describe "scanNinjaDeps for a ninja deps file with weird targets" $ do
    it "finds the correct input for a target where the first dependency is a txt file" $ do
      eitherScanned <- runDiagnostics $ scanNinjaDeps (encodeUtf8 weirdNinjaDeps)
      case eitherScanned of
        Left _ -> expectationFailure (T.unpack "could not parse ninja deps")
        Right scanned -> List.head (resultValue scanned) `shouldBe` targetWithFirstLevelWeirdnessFix
    it "finds the correct input for a target where the first and second dependencies are txt files" $ do
      eitherScanned <- runDiagnostics $ scanNinjaDeps (encodeUtf8 weirdNinjaDeps)
      case eitherScanned of
        Left _ -> expectationFailure (T.unpack "could not parse ninja deps")
        Right scanned -> (resultValue scanned List.!! 1) `shouldBe` targetWithSecondLevelWeirdnessFix
