module App.Fossa.Config.TestSpec (spec) where

import App.Fossa.Config.Test (
  loadConfig,
  parser,
 )
import App.Fossa.Config.Utils (itShouldLoadFromTheConfiguredBaseDir)
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  describe "loadConfig" $ do
    itShouldLoadFromTheConfiguredBaseDir parser loadConfig
