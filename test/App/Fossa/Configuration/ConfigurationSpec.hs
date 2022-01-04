{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Configuration.ConfigurationSpec (
  spec,
) where

import App.Fossa.Config.ConfigFile (
  ConfigFile (..),
  ConfigProject (..),
  ConfigRevision (..),
  ConfigTargets (..),
  ExperimentalConfigs (..),
  ExperimentalGradleConfigs (ExperimentalGradleConfigs),
  resolveConfigFile,
 )
import App.Types (ReleaseGroupMetadata (..))
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Diagnostics (FailureBundle)
import Data.Set qualified as Set
import Effect.Logger (ignoreLogger)
import Effect.ReadFS (runReadFSIO)
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import Test.Hspec qualified as T
import Test.Hspec.Core.Spec (SpecM)
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

testFile :: FilePath
testFile = "validconfig.yml"

badFile :: FilePath
badFile = "invalidconfig.yml"

ver2configFile :: FilePath
ver2configFile = "ver2config.yml"

testdir :: Path Rel Dir
testdir = $(mkRelDir "test/App/Fossa/Configuration/testdata")

spec :: T.Spec
spec = do
  curdir <- T.runIO getCurrentDir

  let runIt :: Maybe FilePath -> SpecM a (Either FailureBundle (Maybe ConfigFile))
      runIt maybeFile = T.runIO . ignoreLogger $ Diag.runDiagnostics $ runReadFSIO $ resolveConfigFile (curdir </> testdir) maybeFile

  config <- runIt $ Just testFile
  badConfig <- runIt $ Just badFile
  missingConfig <- runIt Nothing
  ver2Config <- runIt $ Just ver2configFile

  T.describe "config file parser" $ do
    T.it "parses a full configuration file correctly" $
      case config of
        Left err -> T.expectationFailure ("failed to parse config file" <> show (Diag.renderFailureBundle err))
        Right a -> case a of
          Nothing -> T.expectationFailure "config file was Nothing after parsing"
          Just result -> result `T.shouldBe` expectedConfigFile

    T.it "returns failure for a bad file" $
      case badConfig of
        Left err -> show (Diag.renderFailureBundle err) `T.shouldNotBe` ""
        Right _ -> T.expectationFailure "should have failed parsing"

    T.it "returns Nothing for missing default file" $
      case missingConfig of
        Left _ -> T.expectationFailure "should have failed parsing"
        Right result -> result `T.shouldBe` Nothing

    T.it "fails for incompatible specified file" $
      case ver2Config of
        Left _ -> pure ()
        Right _ -> T.expectationFailure "Should have thrown error for invalid config"
