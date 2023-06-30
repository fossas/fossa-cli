module App.Fossa.RunThemisSpec (spec) where

import App.Fossa.RunThemis (themisFlags)
import App.Types (FullFileUploads (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Types (GlobFilter (GlobFilter), LicenseScanPathFilters (..))

spec :: Spec
spec = do
  describe "themisFlags" $ do
    it "should return the default flag if LicenseScanPathFilters is Nothing" $
      themisFlags Nothing (FullFileUploads False) `shouldBe` ["--srclib-with-matches"]

    it "should return the full-files flag if LicenseScanPathFilters is Nothing and fulFiles is true" $
      themisFlags Nothing (FullFileUploads True) `shouldBe` ["--srclib-with-full-files"]

    it "should add multiple only flags if provided" $
      let licenseScanPathFilters =
            Just
              LicenseScanPathFilters
                { licenseScanPathFiltersOnly = [GlobFilter "**.rb", GlobFilter "**.html"]
                , licenseScanPathFiltersExclude = []
                , licenseScanPathFilterFileExclude = []
                }
       in themisFlags licenseScanPathFilters (FullFileUploads False) `shouldBe` ["--srclib-with-matches", "--only-paths", "**.rb", "--only-paths", "**.html"]

    it "should add only and exclude flags if provided" $
      let licenseScanPathFilters =
            Just
              LicenseScanPathFilters
                { licenseScanPathFiltersOnly = [GlobFilter "**.rb", GlobFilter "**.html"]
                , licenseScanPathFiltersExclude = [GlobFilter "**.jsx"]
                , licenseScanPathFilterFileExclude = []
                }
       in themisFlags licenseScanPathFilters (FullFileUploads False) `shouldBe` ["--srclib-with-matches", "--only-paths", "**.rb", "--only-paths", "**.html", "--exclude-paths", "**.jsx"]
