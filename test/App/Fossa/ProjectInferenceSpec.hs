{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.ProjectInferenceSpec (spec) where

import App.Fossa.ProjectInference (inferGitRemote, linesWithoutCR)
import Control.Effect.Lift (sendIO)
import Data.Text (Text)
import Path (reldir, (</>))
import Path.IO (createDirIfMissing)
import Test.Effect (itWithTempDir', shouldBe')
import Test.Git (writeGitConfig)
import Test.Hspec (Spec, describe, it, shouldBe)

originConfig :: Text -> Text
originConfig url = "[remote \"origin\"]\n\turl = " <> url <> "\n\tfetch = +refs/heads/*:refs/remotes/origin/*\n"

spec :: Spec
spec = do
  describe "linesWithoutCR" $ do
    it "should remove CR from lines if present" $
      linesWithoutCR "a\r\nb\r\n" `shouldBe` ["a", "b"]

    it "should remove CR from lines" $
      linesWithoutCR "a\nb\n" `shouldBe` ["a", "b"]

  describe "inferGitRemote" $ do
    itWithTempDir' "should read the origin remote" $ \dir -> do
      sendIO . writeGitConfig dir $ originConfig "git@github.com:acme/widgets.git"
      remote <- inferGitRemote dir
      remote `shouldBe'` Just "git@github.com:acme/widgets.git"

    itWithTempDir' "should remove credentials from a URL remote" $ \dir -> do
      sendIO . writeGitConfig dir $ originConfig "https://user:secret@github.com/acme/widgets.git"
      remote <- inferGitRemote dir
      remote `shouldBe'` Just "https://github.com/acme/widgets.git"

    itWithTempDir' "should read the checkout a subdirectory belongs to" $ \dir -> do
      sendIO . writeGitConfig dir $ originConfig "https://github.com/acme/widgets.git"
      let subdir = dir </> [reldir|services/api|]
      sendIO $ createDirIfMissing True subdir
      remote <- inferGitRemote subdir
      remote `shouldBe'` Just "https://github.com/acme/widgets.git"

    itWithTempDir' "should be Nothing without an origin remote" $ \dir -> do
      sendIO . writeGitConfig dir $ "[remote \"upstream\"]\n\turl = https://github.com/acme/widgets.git\n"
      remote <- inferGitRemote dir
      remote `shouldBe'` Nothing

    itWithTempDir' "should be Nothing outside a git checkout" $ \dir -> do
      remote <- inferGitRemote dir
      remote `shouldBe'` Nothing
