module System.CGroup.CPUSpec (
  spec,
) where

import Path.IO (getCurrentDir, resolveDir)
import System.CGroup.CPU
import System.CGroup.Types (Controller (..))
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir

  describe "getCPUQuota" $ do
    it "should return CPUQuota when there is a quota" $ do
      controller <- resolveDir currentDir "test/System/CGroup/testdata/cpu-quota"
      quota <- getCPUQuota (Controller controller)
      quota `shouldBe` CPUQuota 1 2

    it "should return NoQuota when there is no quota" $ do
      controller <- resolveDir currentDir "test/System/CGroup/testdata/cpu-noquota"
      quota <- getCPUQuota (Controller controller)
      quota `shouldBe` NoQuota
