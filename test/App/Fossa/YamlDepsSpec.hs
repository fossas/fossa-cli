module App.Fossa.YamlDepsSpec
  ( spec,
  )
where

import App.Fossa.YamlDeps
  ( CustomDependency (CustomDependency),
    ReferencedDependency (ReferencedDependency),
    YamlDependencies (YamlDependencies),
  )
import Control.Effect.Exception (displayException)
import Data.ByteString qualified as BS
import Data.Yaml (decodeEither')
import DepTypes (DepType (..))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe, Expectation, shouldContain)
import Test.Hspec.Core.Spec (SpecM)

getTestDataFile :: String -> SpecM a BS.ByteString
getTestDataFile name = runIO . BS.readFile $ "test/App/Fossa/testdata/" <> name

theWorks :: YamlDependencies
theWorks = YamlDependencies references customs
  where
    references =
        [ ReferencedDependency "one" GemType Nothing,
          ReferencedDependency "two" URLType $ Just "1.0.0"
        ]
    customs =
        [ CustomDependency "hello" "1.2.3" "MIT" Nothing Nothing,
          CustomDependency "full" "3.2.1" "GPL-3.0" (Just "description for full") (Just "we don't validate url's")
        ]

exceptionContains :: BS.ByteString -> String -> Expectation
exceptionContains yamlBytes partial = case decodeEither' @YamlDependencies yamlBytes of
  -- Ethics issue: right is wrong
  Right _ -> expectationFailure $ "Expected to fail with message containing: " <> partial
  Left exc -> displayException exc `shouldContain` partial


spec :: Spec
spec =
  describe "fossa-deps parser" $ do
    theWorksBS <- getTestDataFile "the-works.yml"
    it "should successfully parse all possible inputs" $
      case decodeEither' theWorksBS of
        Left err -> expectationFailure $ displayException err
        Right yamlDeps -> yamlDeps `shouldBe` theWorks

    unsupportedTypeBS <- getTestDataFile "unsupported-type.yml"
    it "should report an unsupported type" $ exceptionContains unsupportedTypeBS "dep type: notafetcher not supported"

    licenseInRefDepBS <- getTestDataFile "license-in-ref-dep.yml"
    it "should report license used on referenced deps" $
      exceptionContains licenseInRefDepBS "Invalid field name for referenced dependencies: license"