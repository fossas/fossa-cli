module App.DocsSpec (
  spec,
) where

import App.Docs (fossaYmlDocUrl, newIssueUrl, userGuideUrl)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.String.Conversion (toString)
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
import Strategy.Ruby.Errors (bundlerLockFileRationaleUrl, rubyFossaDocUrl)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Text.URI (mkURI)

shouldRespondToGETWithHttpCode :: Text -> Int -> Expectation
shouldRespondToGETWithHttpCode uri expected = do
  (url, _) <- fromJust . useHttpsURI <$> mkURI uri
  r <- runReq defaultHttpConfig $ req GET url NoReqBody bsResponse mempty
  responseStatusCode r `shouldBe` expected

urlsToCheck :: [Text]
urlsToCheck =
  [ userGuideUrl
  , newIssueUrl
  , fossaYmlDocUrl
  , bundlerLockFileRationaleUrl
  , rubyFossaDocUrl
  ]

spec :: Spec
spec = do
  describe "Documentation links provided in application are reachable" $
    for_ urlsToCheck $ \url ->
      it (toString url <> " should be reachable") $
        url `shouldRespondToGETWithHttpCode` 200
