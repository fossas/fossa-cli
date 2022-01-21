{-# LANGUAGE CPP #-}

module App.Fossa.VSI.DynLinked.UtilSpec (spec) where

import App.Fossa.VSI.DynLinked.Util (hasSetUID)
import Control.Carrier.Diagnostics (runDiagnostics)
import Path (Abs, File, Path)
import Path.IO qualified as PIO
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "setuid bit" $ do
    pathSetUID <- runIO fileSetUID
    pathStandard <- runIO fileStandard
    resultSetUID <- runIO . runDiagnostics $ hasSetUID pathSetUID
    resultStandard <- runIO . runDiagnostics $ hasSetUID pathStandard

    it "reports non-setuid bit correctly" $ case resultStandard of
      Left _ -> expectationFailure "could not check file: ensure you've run `make build-test-data` locally"
      Right result -> result `shouldBe` fileStandardExpected

    it "reports setuid bit correctly" $ case resultSetUID of
      Left _ -> expectationFailure "could not check file: ensure you've run `make build-test-data` locally"
      Right result -> result `shouldBe` fileSetUIDExpected

fileStandard :: IO (Path Abs File)
fileStandard = PIO.resolveFile' "test/App/Fossa/VSI/DynLinked/testdata/hello_standard"

fileStandardExpected :: Bool
fileStandardExpected = False

fileSetUID :: IO (Path Abs File)
fileSetUID = PIO.resolveFile' "test/App/Fossa/VSI/DynLinked/testdata/hello_setuid"

#ifdef mingw32_HOST_OS

fileSetUIDExpected :: Bool
fileSetUIDExpected = False

#else

fileSetUIDExpected :: Bool
fileSetUIDExpected = True

#endif
