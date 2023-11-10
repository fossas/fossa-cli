{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.InitSpec (spec) where

import App.Fossa.Config.ConfigFile (resolveConfigFile)
import App.Fossa.Init qualified as Init
import App.Fossa.ManualDeps (findFossaDepsFile)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as BS
import Effect.ReadFS (ReadFS, doesFileExist, readContentsBS)
import Path (Abs, Dir, File, Path, Rel, mkRelFile, toFilePath, (</>))
import Test.Effect (EffectStack, expectationFailure', itWithTempDir', shouldBe')
import Test.Hspec (Spec, SpecWith, describe)

spec :: Spec
spec = do
  describe ".fossa.yml" $ do
    doesNotWriteIfFileExists fossaYml fossaYml Init.mkFossaYml
    doesNotWriteIfFileExists fossaYml fossaYaml Init.mkFossaYml

    itWithTempDir' "should write valid .fossa.yml file" $ \tmp -> do
      let expectedFossaYml = tmp </> $(mkRelFile ".fossa.yml")
      fileShouldNotExist expectedFossaYml
      fileShouldNotExist (tmp </> $(mkRelFile ".fossa.yaml"))

      Init.mkFossaYml tmp
      rawContent <- readContentsBS expectedFossaYml

      fileShouldExist expectedFossaYml
      rawContent `shouldBe'` Init.exampleFossaYml

      maybeConfig <- resolveConfigFile tmp (Just ".fossa.yml")
      case maybeConfig of
        Just _ -> pure ()
        Nothing -> expectationFailure' "Could not parse example .fossa.yml"

  describe "fossa-deps.yml" $ do
    doesNotWriteIfFileExists fossaDepsYml fossaDepsYml Init.mkFossaDeps
    doesNotWriteIfFileExists fossaDepsYml fossaDepsYaml Init.mkFossaDeps
    doesNotWriteIfFileExists fossaDepsYml fossaDepsJson Init.mkFossaDeps

    itWithTempDir' "should write valid fossa-deps file" $ \tmp -> do
      let expectedFossaDeps = tmp </> $(mkRelFile "fossa-deps.yml")
      fileShouldNotExist expectedFossaDeps
      fileShouldNotExist (tmp </> $(mkRelFile "fossa-deps.yaml"))
      fileShouldNotExist (tmp </> $(mkRelFile "fossa-deps.json"))

      Init.mkFossaDeps tmp
      rawContent <- readContentsBS expectedFossaDeps

      fileShouldExist expectedFossaDeps
      rawContent `shouldBe'` Init.exampleFossaDeps

      maybeDeps <- findFossaDepsFile tmp
      case maybeDeps of
        Just _ -> pure ()
        Nothing -> expectationFailure' "Could not parse example fossa-deps.yml"

doesNotWriteIfFileExists :: Path Rel File -> Path Rel File -> (Path Abs Dir -> EffectStack ()) -> SpecWith ()
doesNotWriteIfFileExists targetFile existingFile f =
  itWithTempDir' ("should write not write " <> show targetFile <> " file, if file " <> show existingFile <> " already exists!") $ \tmp -> do
    let existingFile' = tmp </> existingFile
    let targetFile' = tmp </> targetFile

    let someContent = "some content"
    sendIO $ BS.writeFile (toFilePath existingFile') someContent
    void $ f tmp

    when (targetFile /= existingFile) $
      fileShouldNotExist targetFile'

    rawContent <- readContentsBS existingFile'
    rawContent `shouldBe'` someContent

fileShouldNotExist :: (Has (Lift IO) sig m, Has ReadFS sig m) => Path Abs File -> m ()
fileShouldNotExist path = do
  exists <- doesFileExist path
  when exists $ expectationFailure' $ (toFilePath path) <> " exists!"

fileShouldExist :: (Has (Lift IO) sig m, Has ReadFS sig m) => Path Abs File -> m ()
fileShouldExist path = do
  exists <- doesFileExist path
  unless exists $ expectationFailure' $ (toFilePath path) <> " does not exist!"

fossaYml :: Path Rel File
fossaYml = $(mkRelFile ".fossa.yml")

fossaYaml :: Path Rel File
fossaYaml = $(mkRelFile ".fossa.yaml")

fossaDepsYml :: Path Rel File
fossaDepsYml = $(mkRelFile "fossa-deps.yml")

fossaDepsYaml :: Path Rel File
fossaDepsYaml = $(mkRelFile "fossa-deps.yaml")

fossaDepsJson :: Path Rel File
fossaDepsJson = $(mkRelFile "fossa-deps.json")
