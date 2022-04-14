module System.ArgsSpec (spec) where

import System.Args (redactApiKeyFromCmdArgs)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "redactApiKeyFromCmdArgs" $ do
    it "should redact api key, when supplied via =" $ do
      let providedArgs = ["fossa", "analyze", "--fossa-api-key=secret"]
          redactedArgs = ["fossa", "analyze", "--fossa-api-key=<REDACTED>"]

      (redactApiKeyFromCmdArgs providedArgs) `shouldBe` redactedArgs

    it "should redact api key, when supplied via arg" $ do
      let providedArgs = ["fossa", "analyze", "--fossa-api-key", "secret"]
          redactedArgs = ["fossa", "analyze", "--fossa-api-key", "<REDACTED>"]
      (redactApiKeyFromCmdArgs providedArgs) `shouldBe` redactedArgs

    it "should not redact any other args" $ do
      let providedArgs = ["fossa", "analyze", "--output", "--debug", "--project", "fossa-api"]
          redactedArgs = ["fossa", "analyze", "--output", "--debug", "--project", "fossa-api"]
      (redactApiKeyFromCmdArgs providedArgs) `shouldBe` redactedArgs
