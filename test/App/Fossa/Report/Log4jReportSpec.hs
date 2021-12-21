module App.Fossa.Report.Log4jReportSpec (
  spec,
) where

import App.Fossa.Analyze.Log4jReport (SimplifiedVersion (SimplifiedVersion), Vulnerability (VulnerabilityOther, VulnerabilityRemoteCodeExecution), getVulnerability, parseSimplifiedVersion)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.String.Conversion (ToString (toString), toText)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseInto :: Text -> SimplifiedVersion -> Expectation
shouldParseInto = parseMatch parseSimplifiedVersion

mkDep :: Text -> Maybe VerConstraint -> Dependency
mkDep name version = Dependency MavenType name version [] mempty mempty

someOfKnownVulnerableDeps :: [Dependency]
someOfKnownVulnerableDeps =
  [ mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.0-alpha1")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.0-alpha2")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.0-beta")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.0-rc1")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.0")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.1")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.2")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.4.1")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.6.1")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.15.0")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.2.0.redhat-1")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.15.0.redhat-000001")
  , mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.16.0")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.0-alpha2")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.0-beta")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.0-rc1")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.0")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.1")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.2")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.4.1")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.6.1")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.15.0")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.2.0.redhat-1")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.15.0.redhat-000001")
  , mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.16.0")
  , mkDep "log4j:log4j" (Just $ CEq "1.1.3")
  , mkDep "log4j:log4j" (Just $ CEq "1.2.17")
  , mkDep "log4j:log4j" (Just $ CEq "1.1.17-atlassian-15")
  , mkDep "log4j:log4j" (Just $ CEq "1.2.17.redhat-3")
  ]

spec :: Spec
spec = do
  describe "parseConstraintExpr" $
    it "should parse major and minor version" $ do
      "1.1" `shouldParseInto` SimplifiedVersion 1 1
      "1.1.1" `shouldParseInto` SimplifiedVersion 1 1
      "1.1.1.1" `shouldParseInto` SimplifiedVersion 1 1
      "1.1.1.alpha" `shouldParseInto` SimplifiedVersion 1 1
      "1.1-alpha" `shouldParseInto` SimplifiedVersion 1 1
      "1.1.alpha" `shouldParseInto` SimplifiedVersion 1 1
      "1.0-alpha" `shouldParseInto` SimplifiedVersion 1 0

  describe "log4j vulnerability" $ do
    it "should identify RCE Vulnerability with log4j 2.15.0 and below" $ do
      getVulnerability (mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.15.0")) `shouldBe` Just VulnerabilityRemoteCodeExecution
      getVulnerability (mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.15.0")) `shouldBe` Just VulnerabilityRemoteCodeExecution
      getVulnerability (mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.15.0")) `shouldBe` Just VulnerabilityRemoteCodeExecution
      getVulnerability (mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.0")) `shouldBe` Just VulnerabilityRemoteCodeExecution

    it "should identify non RCE vulnerability with log4j 2.16.0" $ do
      getVulnerability (mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.16.0")) `shouldBe` Just VulnerabilityOther
      getVulnerability (mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.16.0")) `shouldBe` Just VulnerabilityOther

    it "should not identify any vulnerability with log4j 2.17.0 or above" $ do
      getVulnerability (mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.17.0")) `shouldBe` Nothing
      getVulnerability (mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.17.0")) `shouldBe` Nothing

    it "should compare prefix of vulnerable versions" $ do
      getVulnerability (mkDep "log4j:log4j" (Just $ CEq "1.1.17-atlassian-15")) `shouldSatisfy` isJust
      getVulnerability (mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.15.0.redhat-000001")) `shouldSatisfy` isJust
      getVulnerability (mkDep "org.apache.logging.log4j:log4j" (Just $ CEq "2.0-rc1")) `shouldSatisfy` isJust
      getVulnerability (mkDep "org.apache.logging.log4j:log4j-core" (Just $ CEq "2.0+somebuild")) `shouldSatisfy` isJust

    for_ someOfKnownVulnerableDeps $ \knownV -> do
      it (toString $ "should identify vulnerability with: " <> (dependencyName knownV) <> ": " <> (toText . show $ dependencyVersion knownV)) $ do
        getVulnerability knownV `shouldSatisfy` isJust
