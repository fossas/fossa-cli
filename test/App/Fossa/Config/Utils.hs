{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Config.Utils (parseArgString, itShouldLoadFromTheConfiguredBaseDir) where

import App.Fossa.Config.ConfigFile (ConfigFile (..))
import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Data.ByteString qualified as BS
import Options.Applicative (Parser, execParserPure, handleParseResult, header, info, prefs)
import Path (mkRelFile, toFilePath, (</>))
import Test.Effect (EffectStack, it', shouldBe', withTempDir)
import Test.Hspec (Spec)

-- | Parses an arg string or raises an error
parseArgString :: (Has (Lift IO) sig m) => Parser a -> String -> m a
parseArgString parser = sendIO . handleParseResult . execParserPure (prefs mempty) (info parser progInfo) . words
  where
    progInfo =
      header "Test Arg Parser"

-- | Tests that the config loader uses the directory set in the arguments
itShouldLoadFromTheConfiguredBaseDir ::
  Parser opts -> (opts -> EffectStack (Maybe ConfigFile)) -> Spec
itShouldLoadFromTheConfiguredBaseDir parser loadConfig =
  it' "should load from the configured base dir"
    . withTempDir "AnalyzeSpec"
    $ \tempDir ->
      do
        sendIO $ BS.writeFile (toFilePath (tempDir </> $(mkRelFile ".fossa.yml"))) "version: 42\n"
        options <- sendIO $ parseArgString parser (toFilePath tempDir)
        let configFile =
              ConfigFile
                { configVersion = 42
                , configServer = Nothing
                , configApiKey = Nothing
                , configProject = Nothing
                , configRevision = Nothing
                , configTargets = Nothing
                , configPaths = Nothing
                , configExperimental = Nothing
                , configTelemetry = Nothing
                }
        maybeConfigFile <- loadConfig options
        maybeConfigFile `shouldBe'` Just configFile
