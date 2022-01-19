{-# LANGUAGE CPP #-}

module App.Fossa.VSI.DynLinked.UtilSpec (spec) where

import App.Fossa.VSI.DynLinked.Util (isSetUID)
import Path (Abs, File, Path)
import Path.IO qualified as PIO
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "reports setuid bit correctly" $ do
    pathSetUID <- runIO fileSetUID
    pathStandard <- runIO fileStandard
    resultSetUID <- runIO $ isSetUID pathSetUID
    resultStandard <- runIO $ isSetUID pathStandard

    it "reports setuid bit correctly" $ do

#ifdef mingw32_HOST_OS
      resultSetUID `shouldBe` False
      resultStandard `shouldBe` False
#else
      resultSetUID `shouldBe` True
      resultStandard `shouldBe` False
#endif

fileSetUID :: IO (Path Abs File)
fileSetUID = PIO.resolveFile' "test/App/Fossa/VSI/DynLinked/testdata/hello_setuid"

fileStandard :: IO (Path Abs File)
fileStandard = PIO.resolveFile' "test/App/Fossa/VSI/DynLinked/testdata/hello_standard"
