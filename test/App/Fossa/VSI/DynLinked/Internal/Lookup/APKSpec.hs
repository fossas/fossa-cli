{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VSI.DynLinked.Internal.Lookup.APKSpec (spec) where

import App.Fossa.VSI.DynLinked.Internal.Lookup.APK (SyftArtifact (..), SyftArtifactMetadata (..), SyftArtifactMetadataFile (..), SyftData (..), SyftLookupTable (..), constructLookupTables)
import App.Fossa.VSI.DynLinked.Types (LinuxPackageMetadata (..))
import Control.Carrier.Diagnostics (runDiagnostics)
import Data.Aeson (toJSON)
import Data.Map qualified as Map
import Path (mkAbsFile)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "lookup table" $ do
    result <- runIO . runDiagnostics $ constructLookupTables syntheticData

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
                    [ SyftArtifactMetadataFile $(mkAbsFile "/file1_1")
                    , SyftArtifactMetadataFile $(mkAbsFile "/file1_2")
                    , SyftArtifactMetadataFile $(mkAbsFile "/file1_3")
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
                    [ SyftArtifactMetadataFile $(mkAbsFile "/file2_1")
                    , SyftArtifactMetadataFile $(mkAbsFile "/file2_2")
                    , SyftArtifactMetadataFile $(mkAbsFile "/file2_3")
                    ]
                }
        }
    ]

expectedLookupTable :: SyftLookupTable
expectedLookupTable =
  SyftLookupTable
    { pathToIndex =
        Map.fromList
          [ ($(mkAbsFile "/file1_1"), 0)
          , ($(mkAbsFile "/file1_2"), 0)
          , ($(mkAbsFile "/file1_3"), 0)
          , ($(mkAbsFile "/file2_1"), 1)
          , ($(mkAbsFile "/file2_2"), 1)
          , ($(mkAbsFile "/file2_3"), 1)
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
