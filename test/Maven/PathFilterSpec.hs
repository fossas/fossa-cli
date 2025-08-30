{-# LANGUAGE TemplateHaskell #-}

module Maven.PathFilterSpec (
  spec,
) where

import Control.Carrier.Reader (local)
import Control.Effect.Lift (sendIO)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Discovery.Filters (AllFilters (..), comboExclude)
import Path (Abs, Dir, File, Path, mkRelDir, mkRelFile, (</>))
import Path qualified
import Path.IO (createDirIfMissing)
import Strategy.Maven.Pom.Resolver (GlobalClosure (..), buildGlobalClosure)
import Test.Effect (EffectStack, itWithTempDir', shouldBe')
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
  describe "Maven path filtering" $ do
    itWithTempDir' "does not traverse POMs inside excluded directories" $ \rootDir ->
      testNoTraversal rootDir

-- | Build a minimal on-disk Maven project with a parent POM located in an
-- excluded '.m2' directory.  The test asserts that the parent POM is not
-- loaded when discovery filters exclude that directory.
testNoTraversal :: Path Abs Dir -> EffectStack ()
testNoTraversal rootDir = do
  -- Paths -------------------------------------------------------------
  let rootPomPath :: Path Abs File
      rootPomPath = rootDir </> $(mkRelFile "pom.xml")

      parentDir :: Path Abs Dir
      parentDir =
        rootDir </> $(mkRelDir ".m2/repository/test/parent")

      parentPomPath :: Path Abs File
      parentPomPath = parentDir </> $(mkRelFile "pom.xml")

  -- File contents -----------------------------------------------------
  let rootPom :: Text.Text
      rootPom =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          <> "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">\n"
          <> "  <modelVersion>4.0.0</modelVersion>\n"
          <> "  <parent>\n"
          <> "    <groupId>test</groupId>\n"
          <> "    <artifactId>parent</artifactId>\n"
          <> "    <version>1.0.0</version>\n"
          <> "    <relativePath>.m2/repository/test/parent/pom.xml</relativePath>\n"
          <> "  </parent>\n"
          <> "  <groupId>test</groupId>\n"
          <> "  <artifactId>child</artifactId>\n"
          <> "  <version>1.0.0</version>\n"
          <> "</project>\n"

      parentPom :: Text.Text
      parentPom =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          <> "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">\n"
          <> "  <modelVersion>4.0.0</modelVersion>\n"
          <> "  <groupId>test</groupId>\n"
          <> "  <artifactId>parent</artifactId>\n"
          <> "  <version>1.0.0</version>\n"
          <> "</project>\n"

  -- Create directories + write files ---------------------------------
  sendIO $ createDirIfMissing True (parentDir)
  sendIO $ TIO.writeFile (Path.toFilePath rootPomPath) rootPom
  sendIO $ TIO.writeFile (Path.toFilePath parentPomPath) parentPom

  -- Exclude the .m2 directory via filters -----------------------------
  let filters = AllFilters mempty (comboExclude [] [$(mkRelDir ".m2")])

  -- Build global closure with filters --------------------------------
  GlobalClosure{globalPoms} <- local (const filters) $ buildGlobalClosure rootDir [rootPomPath]

  -- We expect only the root POM to be present -------------------------
  Map.size globalPoms `shouldBe'` 1
