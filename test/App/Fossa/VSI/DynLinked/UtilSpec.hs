{-# LANGUAGE CPP #-}

module App.Fossa.VSI.DynLinked.UtilSpec (spec) where

import App.Fossa.VSI.DynLinked.Util (isSetUID)
import Control.Carrier.Diagnostics (runDiagnostics)
import Path (Abs, File, Path)
import Path.IO qualified as PIO
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "reports setuid bit correctly" $ do
    pathSetUID <- runIO fileSetUID
    pathStandard <- runIO fileStandard
    resultSetUID <- runIO . runDiagnostics $ isSetUID pathSetUID
    resultStandard <- runIO . runDiagnostics $ isSetUID pathStandard

    it "reports non-setuid bit correctly" $ case resultStandard of
      Left _ -> expectationFailure "could not check file"
      Right result -> result `shouldBe` fileStandardExpected

    it "reports setuid bit correctly" $ case resultSetUID of
      Left _ -> expectationFailure "could not check file"
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
