{-# LANGUAGE BlockArguments #-}

module App.Fossa.CirceSpec (
  spec,
) where

import App.Fossa.EmbeddedBinary (toPath, withCirceBinary)
import Data.Either (isRight)
import Data.String.Conversion (toText)
import Effect.Exec
import Path (parent)
import Path.IO (doesFileExist)
import Test.Effect (it', shouldSatisfy')
import Test.Hspec

spec :: Spec
spec = do
  describe "Circe binary" $ do
    it "should be extractable" $ do
      -- Extract the circe binary and verify it exists
      withCirceBinary $ \binPaths -> do
        let binPath = toPath binPaths

        -- Check that the binary file exists
        fileExists <- doesFileExist binPath
        fileExists `shouldBe` True

    it' "should run with --version argument" $ do
      -- Extract the circe binary and test that it can run with --version
      withCirceBinary $ \binPaths -> do
        let binPath = toPath binPaths
        let binDir = parent binPath
        let binPathText = toText binPath
        let command = Command binPathText ["--version"] Never

        -- Run circe with --version argument
        result <- exec binDir command

        -- Check that it executed successfully
        result `shouldSatisfy'` isRight
