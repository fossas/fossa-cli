{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VSI.DynLinked.Internal.Lookup.APKSpec (spec) where

import App.Fossa.VSI.DynLinked.Internal.Lookup.APK (APKLookupTable (..), SyftArtifact (..), SyftArtifactMetadata (..), SyftArtifactMetadataFile (..), SyftData (..), compileSyftOutput)
import App.Fossa.VSI.DynLinked.Types (LinuxPackageMetadata (..))
import Control.Carrier.Diagnostics (runDiagnostics)
import Data.Aeson (toJSON)
import Data.Map qualified as Map
import Path (Abs, Dir, Path, mkAbsDir, mkRelFile, (</>))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "lookup table" $ do
    result <- runIO . runDiagnostics $ compileSyftOutput syntheticData

    it "correctly builds lookup table" $ case result of
      Left e -> expectationFailure ("could not construct lookup table: " <> show e)
      Right table -> table `shouldBe` expectedLookupTable

syntheticData :: SyftData
syntheticData =
  SyftData
    [ SyftArtifact
        { artifactName = "name_1"
        , artifactVersion = "version_1"
        , artifactType = "apk"
        , artifactMetadataType = "ApkMetadata"
        , artifactMetadata =
            toJSON $
              SyftArtifactMetadata
                { metadataArchitecture = "arch_1"
                , metadataFiles =
                    [ SyftArtifactMetadataFile $ safeAbsRoot </> $(mkRelFile "file1_1")
                    , SyftArtifactMetadataFile $ safeAbsRoot </> $(mkRelFile "file1_2")
                    , SyftArtifactMetadataFile $ safeAbsRoot </> $(mkRelFile "file1_3")
                    ]
                }
        }
    , SyftArtifact
        { artifactName = "name_2"
        , artifactVersion = "version_2"
        , artifactType = "apk"
        , artifactMetadataType = "ApkMetadata"
        , artifactMetadata =
            toJSON $
              SyftArtifactMetadata
                { metadataArchitecture = "arch_2"
                , metadataFiles =
                    [ SyftArtifactMetadataFile $ safeAbsRoot </> $(mkRelFile "file2_1")
                    , SyftArtifactMetadataFile $ safeAbsRoot </> $(mkRelFile "file2_2")
                    , SyftArtifactMetadataFile $ safeAbsRoot </> $(mkRelFile "file2_3")
                    ]
                }
        }
    ]

expectedLookupTable :: APKLookupTable
expectedLookupTable =
  APKLookupTable
    { pathToIndex =
        Map.fromList
          [ (safeAbsRoot </> $(mkRelFile "file1_1"), 0)
          , (safeAbsRoot </> $(mkRelFile "file1_2"), 0)
          , (safeAbsRoot </> $(mkRelFile "file1_3"), 0)
          , (safeAbsRoot </> $(mkRelFile "file2_1"), 1)
          , (safeAbsRoot </> $(mkRelFile "file2_2"), 1)
          , (safeAbsRoot </> $(mkRelFile "file2_3"), 1)
          ]
    , indexToMeta =
        Map.fromList
          [
            ( 0
            , LinuxPackageMetadata
                { linuxPackageID = "name_1"
                , linuxPackageRevision = "version_1"
                , linuxPackageArch = "arch_1"
                , linuxPackageDistroEpoch = Nothing
                }
            )
          ,
            ( 1
            , LinuxPackageMetadata
                { linuxPackageID = "name_2"
                , linuxPackageRevision = "version_2"
                , linuxPackageArch = "arch_2"
                , linuxPackageDistroEpoch = Nothing
                }
            )
          ]
    }

-- We have to use CPP pragmas here, because we can't compile abs paths for other platforms.
#ifdef mingw32_HOST_OS
safeAbsRoot :: Path Abs Dir
safeAbsRoot = $(mkAbsDir "C:/")
#else
safeAbsRoot :: Path Abs Dir
safeAbsRoot = $(mkAbsDir "/")
#endif
