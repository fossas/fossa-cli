{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.InitSpec (spec) where

import App.Fossa.Config.ConfigFile (resolveConfigFile)
import App.Fossa.Init qualified as Init
import Control.Effect.Lift (Has, Lift)
import Control.Monad (unless)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsBS)
import Path (Abs, File, Path, mkRelFile, toFilePath, (</>))
import Test.Effect (expectationFailure', itWithTempDir', shouldBe')
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  describe ".fossa.yml" $ do

    itWithTempDir' "should write valid .fossa.yml.example file" $ \tmp -> do
      let expectedFossaYml = tmp </> $(mkRelFile ".fossa.yml.example")

      Init.mkFossaYml tmp
      rawContent <- readContentsBS expectedFossaYml

      fileShouldExist expectedFossaYml
      rawContent `shouldBe'` Init.exampleFossaYml

      maybeConfig <- resolveConfigFile tmp (Just ".fossa.yml.example")
      case maybeConfig of
        Just _ -> pure ()
        Nothing -> expectationFailure' "Could not parse example .fossa.yml.example"

  describe "fossa-deps.yml" $ do
    itWithTempDir' "should write valid fossa-deps file" $ \tmp -> do
      let expectedFossaDeps = tmp </> $(mkRelFile "fossa-deps.yml.example")

      Init.mkFossaDeps tmp
      rawContent <- readContentsBS expectedFossaDeps

      fileShouldExist expectedFossaDeps
      rawContent `shouldBe'` Init.exampleFossaDeps


fileShouldExist :: (Has (Lift IO) sig m, Has ReadFS sig m) => Path Abs File -> m ()
fileShouldExist path = do
  exists <- doesFileExist path
  unless exists $ expectationFailure' $ (toFilePath path) <> " does not exist!"
