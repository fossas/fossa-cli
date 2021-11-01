module App.DocsSpec (
  spec,
) where

import App.Docs (fossaYmlDocUrl, newIssueUrl, userGuideUrl)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.HTTP.Req (
  GET (GET),
  NoReqBody (NoReqBody),
  bsResponse,
  defaultHttpConfig,
  req,
  responseStatusCode,
  runReq,
  useHttpsURI,
 )
import Test.Hspec (Expectation, Spec, describe, shouldBe, xit)
import Text.URI (mkURI)

shouldRespondToGETWithHttpCode :: Text -> Int -> Expectation
shouldRespondToGETWithHttpCode uri expected = do
  (url, _) <- fromJust . useHttpsURI <$> mkURI uri
  r <- runReq defaultHttpConfig $ req GET url NoReqBody bsResponse mempty
  responseStatusCode r `shouldBe` expected

spec :: Spec
spec = do
  describe "userGuideUrl" $
    xit "should be reachable" $
      userGuideUrl `shouldRespondToGETWithHttpCode` 200

  describe "newIssueUrl" $
    xit "should be reachable" $
      newIssueUrl `shouldRespondToGETWithHttpCode` 200

  describe "fossaYmlDocUrl" $
    xit "should be reachable" $
      fossaYmlDocUrl `shouldRespondToGETWithHttpCode` 200
