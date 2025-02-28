{-# LANGUAGE BlockArguments #-}

module App.Fossa.CirceSpec (
  spec,
) where

import App.Fossa.EmbeddedBinary (withCirceBinary, toPath)
import Path.IO (doesFileExist)
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