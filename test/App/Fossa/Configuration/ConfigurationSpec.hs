{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Configuration.ConfigurationSpec (
  spec,
) where

import App.Fossa.Configuration
import App.Types (ReleaseGroupMetadata (..))
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Stack (runStack)
import Data.Set qualified as Set
import Effect.ReadFS
import Path
import Path.IO (getCurrentDir)
import ResultUtil
import Test.Hspec qualified as T
import Types (BuildTarget (..), TargetFilter (..))

expectedConfigFile :: ConfigFile
expectedConfigFile =
  ConfigFile
    { configVersion = 3
    , configServer = Just "https://app.fossa.com"
    , configApiKey = Just "123"
    , configProject = Just expectedConfigProject
    , configRevision = Just expectedConfigRevision
    , configTargets = Just expectedConfigTargets
    , configPaths = Nothing
    , configExperimental = Just expectedExperimentalConfig
    }

expectedConfigProject :: ConfigProject
expectedConfigProject =
  ConfigProject
    { configProjID = Just "github.com/fossa-cli"
    , configName = Just "fossa-cli"
    , configLink = Just "fossa.com"
    , configTeam = Just "fossa-team"
    , configJiraKey = Just "key"
    , configUrl = Just "fossa.com"
    , configPolicy = Just "license-policy"
    , configReleaseGroup = Just expectedReleaseGroup
    }

expectedConfigRevision :: ConfigRevision
expectedConfigRevision =
  ConfigRevision
    { configCommit = Just "12345"
    , configBranch = Just "master"
    }

expectedReleaseGroup :: ReleaseGroupMetadata
expectedReleaseGroup =
  ReleaseGroupMetadata
    { releaseGroupName = "test-release"
    , releaseGroupRelease = "123"
    }

expectedConfigTargets :: ConfigTargets
expectedConfigTargets =
  ConfigTargets
    { targetsOnly = [directoryTarget, simpleTarget, complexTarget]
    , targetsExclude = []
    }

expectedExperimentalConfig :: ExperimentalConfigs
expectedExperimentalConfig =
  ExperimentalConfigs
    { gradle = Just $ ExperimentalGradleConfigs (Set.fromList ["onlyProdConfigs", "onlyProdConfigs2"])
    }

simpleTarget :: TargetFilter
simpleTarget = TypeTarget "pip"

complexTarget :: TargetFilter
complexTarget = TypeDirTargetTarget "gradle" $(mkRelDir "./") (BuildTarget "specific-target")

directoryTarget :: TargetFilter
directoryTarget = TypeDirTarget "maven" $(mkRelDir "root")

testFile :: Path Rel File
testFile = $(mkRelFile "test/App/Fossa/Configuration/testdata/validconfig.yml")

badFile :: Path Rel File
badFile = $(mkRelFile "test/App/Fossa/Configuration/testdata/invalidconfig.yml")

missingFile :: Path Rel File
missingFile = $(mkRelFile "test/App/Fossa/Configuration/testdata/missingfile.yml")

ver2configFile :: Path Rel File
ver2configFile = $(mkRelFile "test/App/Fossa/Configuration/testdata/ver2config.yml")

spec :: T.Spec
spec = do
  dir <- T.runIO getCurrentDir

  config <- T.runIO . runStack . Diag.runDiagnostics $ runReadFSIO $ readConfigFile (dir </> testFile)
  badConfig <- T.runIO . runStack . Diag.runDiagnostics $ runReadFSIO $ readConfigFile (dir </> badFile)
  missingConfig <- T.runIO . runStack . Diag.runDiagnostics $ runReadFSIO $ readConfigFile (dir </> missingFile)
  ver2Config <- T.runIO . runStack . Diag.runDiagnostics $ runReadFSIO $ readConfigFile (dir </> ver2configFile)

  T.describe "config file parser" $ do
    T.it "parses a full configuration file correctly" $ do
      assertOnSuccess config $ \_ a -> case a of
        Nothing -> T.expectationFailure "config file was Nothing after parsing"
        Just result -> result `T.shouldBe` expectedConfigFile

    T.it "returns failure for a bad file" $ expectFailure badConfig

    T.it "returns Nothing for missing file" $
      assertOnSuccess missingConfig $ \_ result -> result `T.shouldBe` Nothing

    T.it "returns Nothing for incompatible file" $
      assertOnSuccess ver2Config $ \_ result -> result `T.shouldBe` Nothing
