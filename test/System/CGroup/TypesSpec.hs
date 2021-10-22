module System.CGroup.TypesSpec (
  spec,
) where

import Path
import Path.IO (getCurrentDir, resolveFile)
import System.CGroup.Types
import System.Info (os)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

-- This test won't work on Windows, because paths starting with `/` are invalid
spec :: Spec
spec = exceptOnWindows $ do
  currentDir <- runIO getCurrentDir

  describe "resolveGroupController" $ do
    it "should work on a real world example" $ do
      cgroup <- resolveFile currentDir "test/System/CGroup/testdata/realworld/cgroup"
      mountinfo <- resolveFile currentDir "test/System/CGroup/testdata/realworld/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu"

      controller <- resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

    it "should resolve a direct mount root" $ do
      cgroup <- resolveFile currentDir "test/System/CGroup/testdata/direct/cgroup"
      mountinfo <- resolveFile currentDir "test/System/CGroup/testdata/direct/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu"

      controller <- resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

    it "should resolve subdirectories of a mount root" $ do
      cgroup <- resolveFile currentDir "test/System/CGroup/testdata/indirect/cgroup"
      mountinfo <- resolveFile currentDir "test/System/CGroup/testdata/indirect/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu/subdir"

      controller <- resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

    it "should work for cgroups v2" $ do
      cgroup <- resolveFile currentDir "test/System/CGroup/testdata/cgroupsv2/cgroup"
      mountinfo <- resolveFile currentDir "test/System/CGroup/testdata/cgroupsv2/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu"

      controller <- resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

exceptOnWindows :: Applicative m => m () -> m ()
exceptOnWindows act
  | os == "mingw32" = pure ()
  | otherwise = act
