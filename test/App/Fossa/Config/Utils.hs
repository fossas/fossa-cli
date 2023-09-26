{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Config.Utils (parseArgString, itShouldLoadFromTheConfiguredBaseDir, itShouldFailWhenLabelsExceedFive) where

import App.Fossa.Config.Analyze (AnalyzeCliOpts, mergeOpts)
import App.Fossa.Config.ConfigFile (ConfigFile (..), OrgWideCustomLicenseConfigPolicy (..))
import App.Fossa.Config.EnvironmentVars (EnvVars (EnvVars))
import Control.Effect.Lift (Lift, sendIO)
import Data.Flag ()
import Options.Applicative (Parser, execParserPure, getParseResult, handleParseResult, header, info, prefs)
import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, toFilePath, (</>))
import Test.Effect (EffectStack, expectFatal', it', shouldBe')
import Test.Hspec (Spec)

-- import App.Fossa.Config.Container.Analyze
import Control.Carrier.Diagnostics
import Data.Text (Text)
import Path.IO (getCurrentDir)
import Test.Hspec.Core.Spec (runIO)

-- | Parses an arg string or raises an error
parseArgString :: (Has (Lift IO) sig m) => Parser a -> String -> m a
parseArgString parser = sendIO . handleParseResult . execParserPure (prefs mempty) (info parser progInfo) . words
  where
    progInfo =
      header "Test Arg Parser"

configFile :: Path Abs File -> ConfigFile
configFile path =
  ConfigFile
    { configVersion = 42
    , configServer = Nothing
    , configApiKey = Nothing
    , configProject = Nothing
    , configRevision = Nothing
    , configTargets = Nothing
    , configPaths = Nothing
    , configExperimental = Nothing
    , configVendoredDependencies = Nothing
    , configTelemetry = Nothing
    , configCustomLicenseSearch = Nothing
    , configKeywordSearch = Nothing
    , configOrgWideCustomLicenseConfigPolicy = Use
    , configConfigFilePath = path
    }

fixtureDir :: Path Rel Dir
fixtureDir = $(mkRelDir "test/App/Fossa/Config/testdata")

-- | Tests that the config loader uses the directory set in the arguments
itShouldLoadFromTheConfiguredBaseDir ::
  Parser opts -> (opts -> EffectStack (Maybe ConfigFile)) -> Spec
itShouldLoadFromTheConfiguredBaseDir parser loadConfig = do
  currDir <- runIO getCurrentDir
  let scanDir = currDir </> fixtureDir
  it' "should load from the configured base dir" $ do
    let absFilePath = scanDir </> $(mkRelFile ".fossa.yml")
    options <- sendIO $ parseArgString parser (toFilePath scanDir)
    maybeConfigFile <- loadConfig options
    maybeConfigFile `shouldBe'` Just (configFile absFilePath)

itShouldFailWhenLabelsExceedFive :: Parser AnalyzeCliOpts -> Spec
itShouldFailWhenLabelsExceedFive parser =
  it' "should fail when labels exceed 5" $ do
    let p = execParserPure (prefs mempty) (info parser $ header "Test Arg Parser") []
    case getParseResult p of
      Nothing -> fatal ("test failed" :: Text)
      Just cliOpts -> do
        expectFatal' $ mergeOpts Nothing (EnvVars Nothing False False Nothing Nothing mempty) cliOpts
