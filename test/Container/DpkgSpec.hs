module Container.DpkgSpec (spec) where

import Container.Dpkg (DpkgEntry, analyzeDpkgEntriesScoped)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, Dir, Path)
import Path.IO qualified as PIO
import ResultUtil (assertOnSuccess)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "Container Dpkg Parser" $ do
    target <- runIO testDataDir
    result <- runIO . runStack . runDiagnostics . runReadFSIO $ analyzeDpkgEntriesScoped target

    it "parses var/lib/dpkg/status" $
      assertOnSuccess result $ \_ c ->
        c `shouldBe` expectedEntries

-- | Inside testdata, a file is stored at @var/lib/dpkg/status@.
-- This file is the exact contents of a file at @/var/lib/dpkg/status@ on @debian:bullseye@.
testDataDir :: IO (Path Abs Dir)
testDataDir = PIO.resolveDir' "test/Container/testdata/"

expectedEntries :: [DpkgEntry]
expectedEntries = []
