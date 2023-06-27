{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.VSI.TypesSpec (spec) where

import App.Fossa.VSI.Types (Locator (..), VsiExportedInferencesBody (VsiExportedInferencesBody), VsiFilePath (..), VsiInference (VsiInference))
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map qualified as Map
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ

inferencesBody :: BS.ByteString
inferencesBody =
  [r|
{
  "InferencesByFilePath": {
    "/foo.h": {
      "RawSha256": "YmIwMTg2NTNlOTVlY2U5M2VmMDYwMTQ3YjA0ZjZhYzRkZjhlMzFhZDc1OWFjYmExZWJmMjIwZDVjZTJlM2ZkZQ==",
      "ComponentID": "0f4ba6a8-5b3f-436f-8c36-828e7375aef7",
      "Locator": "git+github.com/facebook/folly$v2016.08.08.00",
      "Confidence": 1
    }
  }
}
|]

expectedBody :: VsiExportedInferencesBody
expectedBody =
  VsiExportedInferencesBody $
    Map.fromList
      [
        ( VsiFilePath "/foo.h"
        , VsiInference "git+github.com/facebook/folly$v2016.08.08.00"
        )
      ]

noRevisionBody :: BS.ByteString
noRevisionBody =
  [r|
{
  "InferencesByFilePath": {
    "/foo.h": {
      "RawSha256": "YmIwMTg2NTNlOTVlY2U5M2VmMDYwMTQ3YjA0ZjZhYzRkZjhlMzFhZDc1OWFjYmExZWJmMjIwZDVjZTJlM2ZkZQ==",
      "ComponentID": "0f4ba6a8-5b3f-436f-8c36-828e7375aef7",
      "Locator": "git+github.com/facebook/folly$",
      "Confidence": 1
    }
  }
}
|]

expectedNoRevision :: String
expectedNoRevision = ""

spec :: Spec
spec = describe "VSI Types" $ do
  it "Parses a VsiExportedInferencesBody" $ do
    let body = eitherDecode inferencesBody :: Either String VsiExportedInferencesBody
    body `shouldBe` Right expectedBody

-- it "Rejects inferences with no revision" $ do
--   let body = eitherDecode noRevisionBody :: Either String VsiExportedInferencesBody
--   body `shouldBe` Left expectedNoRevision
