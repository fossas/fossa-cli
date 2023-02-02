module App.Fossa.Config.AnalyzeSpec (spec) where

import App.Fossa.Config.Analyze (
  cliParser,
  loadConfig,
 )
import App.Fossa.Config.Utils (itShouldLoadFromTheConfiguredBaseDir, itShouldFailWhenLabelsExceedFive)
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  describe "loadConfig" $ do
    itShouldLoadFromTheConfiguredBaseDir cliParser loadConfig
  describe "5 labels are the max" $ 
    itShouldFailWhenLabelsExceedFive cliParser
    