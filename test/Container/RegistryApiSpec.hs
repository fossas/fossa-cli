{-# LANGUAGE QuasiQuotes #-}

module Container.RegistryApiSpec (spec) where

import Container.Docker.OciManifest (OciManifestConfig (configDigest), OciManifestV2 (ociConfig))
import Container.Docker.SourceParser (RegistryImageSource, RepoDigest (RepoDigest), parseImageUrl)
import Control.Carrier.ContainerRegistryApi (runTasks)
import Control.Carrier.ContainerRegistryApi.Authorization (RegistryAuthChallenge (..), RegistryBearerChallenge (..), mkRequest, parseAuthChallenge)
import Control.Carrier.ContainerRegistryApi.Common (AuthToken (BasicAuthToken, BearerAuthToken), RegistryCtx (RegistryCtx), getToken, safeReplaceToken, updateToken)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Stack (runStack)
import Control.Carrier.TaskPool (Progress)
import Control.Concurrent.STM (newEmptyTMVarIO)
import Control.Effect.ContainerRegistryApi (ContainerRegistryApi, getImageManifest)
import Control.Effect.Diagnostics (Diagnostics, Has, fatalText, fromEitherShow)
import Control.Effect.Lift (Lift)
import Data.ByteString (ByteString, isInfixOf)
import Data.Foldable (for_, traverse_)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Void (Void)
import Diag.Result (Result (Failure, Success), renderFailure)
import Effect.Logger (ignoreLogger)
import Network.HTTP.Client (ManagerSettings (managerRawConnection), Response (responseStatus), defaultManagerSettings, makeConnection, managerSetProxy, newManager, noProxy, parseRequest)
import Network.HTTP.Client.Internal (Connection)
import Network.HTTP.Types (ok200)
import ResultUtil (assertOnSuccess, expectFailure)
import System.Timeout (timeout)
import Test.Effect (it', shouldBe')
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldContain)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.RawString.QQ (r)

wwwAuthenticate :: Text
wwwAuthenticate = [r|Bearer realm="https://quay.io/v2/auth",service="quay.io",scope="repository:some-repo/some-img:pull"|]

wwwAuthenticateBasic :: Text
wwwAuthenticateBasic = [r|Basic realm="https://quay.io/v2/auth",service="quay.io"|]

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser mempty input `shouldParse` expected

decodeStrict :: Text -> Text -> Either (ParseErrorBundle Text Void) RegistryImageSource
decodeStrict arch = parse (parseImageUrl arch) mempty

getImageConfig ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ContainerRegistryApi sig m
  ) =>
  Text ->
  Text ->
  m RepoDigest
getImageConfig arch img =
  (configDigest . ociConfig)
    <$> (getImageManifest =<< fromEitherShow (decodeStrict arch img))

spec :: Spec
spec = do
  registryApiSpec
  parseAuthChallengeSpec
  redirectSpec
  safeReplaceTokenSpec
  runTasksSpec

-- Blob downloads are forked onto a task pool. A task that fails through 'Diagnostics' has to be
-- reported once the pool has drained: otherwise the worker running it exits without marking the
-- task as finished, the remaining threads wait forever for it, and the run dies with "thread
-- blocked indefinitely in an STM transaction".
runTasksSpec :: Spec
runTasksSpec =
  describe "runTasks" $ do
    it "should return every task's result in order" $ do
      result <- runStack . runDiagnostics $ runTasks 2 noProgress (pure <$> [1 :: Int .. 10])
      assertOnSuccess result $ \_ results -> results `shouldBe` [1 .. 10]

    it "should report every failed task instead of blocking forever" $ do
      -- Without the fix this never returns, since a failing task's worker exits silently.
      result <-
        timeout 5_000_000 . runStack . runDiagnostics $
          runTasks 2 noProgress [pure (1 :: Int), fatalText "registry rejected blob one", pure 3, fatalText "registry rejected blob two"]
      case result of
        Nothing -> expectationFailure "runTasks did not return after its tasks failed"
        Just (Success _ _) -> expectationFailure "expected a failure"
        Just (Failure ws eg) -> do
          let rendered = show (renderFailure ws eg "An issue occurred")
          rendered `shouldContain` "registry rejected blob one"
          rendered `shouldContain` "registry rejected blob two"
  where
    noProgress :: Applicative m => Progress -> m ()
    noProgress = const (pure ())

-- A registry may answer a blob request with a redirect to its storage (S3, Azure Blob Storage, a
-- CDN...), at a URL that carries its own credentials. The 'Authorization' header must not follow
-- such a redirect: it would hand the registry credentials to another host, and S3 rejects a
-- pre-signed URL that also has that header with "400 Bad Request". A redirect that stays on the
-- registry still needs the header.
redirectSpec :: Spec
redirectSpec =
  describe "mkRequest" $
    for_ [("basic credentials", BasicAuthToken "user" "pass"), ("a bearer token", BearerAuthToken "token")] $ \(tokenDesc, token) ->
      it ("should send " <> tokenDesc <> " to the registry, but not to the storage host it redirects to") $ do
        manager <- newManager . managerSetProxy noProxy $ defaultManagerSettings{managerRawConnection = pure (\_ host _ -> connectTo host)}
        ctx <- RegistryCtx <$> newEmptyTMVarIO
        updateToken ctx token
        req <- parseRequest "http://registry.test/v2/some-repo/blobs/sha256:abc"
        result <- runStack . runDiagnostics . ignoreLogger . runReader ctx $ mkRequest manager Nothing Nothing req
        -- Without the fix this fails with "Caught unexpected error from: (400) http://storage.test/abc?X-Amz-Signature=abc".
        assertOnSuccess result $ \_ response -> responseStatus response `shouldBe` ok200
  where
    -- An in-memory connection to the host, which answers the one request written to it.
    connectTo :: String -> IO Connection
    connectTo host = do
      request <- newIORef mempty
      answered <- newIORef False
      let answer = do
            alreadyAnswered <- atomicModifyIORef' answered (True,)
            if alreadyAnswered then pure mempty else respond host <$> readIORef request
      makeConnection answer (\bytes -> modifyIORef' request (<> bytes)) (pure ())

    -- The registry wants its credentials, and redirects a blob request to itself and from there to
    -- its storage. The storage answers like S3 does to a pre-signed URL: no other credentials allowed.
    respond :: String -> ByteString -> ByteString
    respond host request = "HTTP/1.1 " <> status <> "\r\nConnection: close\r\nContent-Length: 0\r\n\r\n"
      where
        status = case (host, "Authorization: " `isInfixOf` request, "/v2/" `isInfixOf` request) of
          ("storage.test", True, _) -> "400 Bad Request"
          ("storage.test", False, _) -> "200 OK"
          (_, False, _) -> "401 Unauthorized"
          (_, True, True) -> "307 Temporary Redirect\r\nLocation: /blobs/abc"
          (_, True, False) -> "307 Temporary Redirect\r\nLocation: http://storage.test/abc?X-Amz-Signature=abc"

-- Blob downloads share one 'RegistryCtx' across the task pool's threads, and 'getToken' waits
-- while another thread is replacing the token. A failed replacement therefore has to put the
-- previous state back, otherwise every other thread blocks forever.
safeReplaceTokenSpec :: Spec
safeReplaceTokenSpec =
  describe "safeReplaceToken" $
    for_ [("no token", Nothing), ("an existing token", Just (BearerAuthToken "old-token"))] $ \(previousDesc, previous) ->
      it ("should restore " <> previousDesc <> " when fetching the new token fails") $ do
        ctx <- RegistryCtx <$> newEmptyTMVarIO
        traverse_ (updateToken ctx) previous

        result <- runStack . runDiagnostics $ safeReplaceToken ctx (fatalText "registry rejected the token request")
        expectFailure result

        -- Without the fix this never returns, since the context is left as "updating".
        token <- timeout 5_000_000 (getToken ctx)
        token `shouldBe` Just previous

parseAuthChallengeSpec :: Spec
parseAuthChallengeSpec =
  describe "parseAuthChallenge" $ do
    let shouldParseInto = parseMatch parseAuthChallenge
    it "should parse bearer auth challenge" $
      wwwAuthenticate
        `shouldParseInto` BearerAuthChallenge
          ( RegistryBearerChallenge
              "https://quay.io/v2/auth"
              "quay.io"
              "repository:some-repo/some-img:pull"
          )

    it "should parse basic auth challenge" $
      wwwAuthenticateBasic
        `shouldParseInto` (BasicAuthChallenge "https://quay.io/v2/auth")

registryApiSpec :: Spec
registryApiSpec =
  describe "Container Registry API" $
    describe "Public Registry APIs" $ do
      describe "Microsoft registry" $ do
        it' "should get manifest with digest" $ do
          confDigest <- getImageConfig amd64 mcrRegistryImage
          confDigest `shouldBe'` mcrRegistryImageDigest

      describe "gcr (google cloud) registry" $ do
        it' "should get manifest with digest" $ do
          confDigest <- getImageConfig amd64 debian2gcrImage
          confDigest `shouldBe'` debian2gcrImageDigest

      describe "Custom Registry" $ do
        it' "should get manifest with tag" $ do
          confDigest <- getImageConfig amd64 githubImage
          confDigest `shouldBe'` githubImageConfigDigest

        it' "should get manifest with digest" $ do
          confDigest <- getImageConfig amd64 githubImageWithDigest
          confDigest `shouldBe'` githubImageConfigDigest

        it' "should get manifest for multi-platform image (chooses target platform - graalvm-ce)" $ do
          confDigest <- getImageConfig amd64 githubMultiArchImage
          confDigest `shouldBe'` githubMultiArchImageConfigDigest

        it' "should get manifest for multi-platform image (chooses target platform) in oci image index" $ do
          confDigest <- getImageConfig amd64 haskellDevImage
          confDigest `shouldBe'` haskellDevImageDigest

      describe "Default Index (docker)" $ do
        it' "should get manifest with tag" $ do
          confDigest <- getImageConfig amd64 dhImage
          confDigest `shouldBe'` dhImageDigest

        it' "should get manifest with digest" $ do
          confDigest <- getImageConfig amd64 dhImageWithDigest
          confDigest `shouldBe'` dhImageDigest

        it' "should get manifest for multi-platform image (chooses target platform - grafana arm)" $ do
          confDigest <- getImageConfig arm64 grafanaMultiArchImage
          confDigest `shouldBe'` grafanaMultiArchImageDigest

        it' "should get manifest for multi-platform images (chooses target platform -  redis arm64)" $ do
          redisDigest <- getImageConfig arm64 redisImage
          redisDigest `shouldBe'` redisImageDigest

amd64 :: Text
amd64 = "amd64"

arm64 :: Text
arm64 = "arm64"

githubImage :: Text
githubImage = "ghcr.io/fossas/haskell-dev-tools:8.10.4"

githubImageConfigDigest :: RepoDigest
githubImageConfigDigest =
  RepoDigest "sha256:e83e5c9e32de2454ef584b4582f7ea171c9c71b5f23c8f13e297a9153e02f4af"

githubImageWithDigest :: Text
githubImageWithDigest =
  "ghcr.io/fossas/haskell-dev-tools@sha256:35643079905dfdd597fa2cff3b46aadf5378d325a68f00bdb2665c49fa6ca6a2"

githubMultiArchImage :: Text
githubMultiArchImage =
  "ghcr.io/graalvm/graalvm-ce:ol7-java11-21.3.3"

githubMultiArchImageConfigDigest :: RepoDigest
githubMultiArchImageConfigDigest =
  RepoDigest "sha256:bdcba07acb11053fea0026b807ecf94550ace7df27b10596ca4c765165243cef"

dhImage :: Text
dhImage = "amazon/aws-cli:2.0.6"

dhImageDigest :: RepoDigest
dhImageDigest =
  RepoDigest "sha256:af825d93886a9ce8af4b7b9d36f3becbadef7420f79d2b05a3178ec0ad9e1a21"

dhImageWithDigest :: Text
dhImageWithDigest =
  "amazon/aws-cli@sha256:7a27c26c2937a3d0b84171675709df1dc09aa331e86cad90f74ada6df7b59c89"

grafanaMultiArchImage :: Text
grafanaMultiArchImage = "grafana/grafana:8.1.7-ubuntu"

grafanaMultiArchImageDigest :: RepoDigest
grafanaMultiArchImageDigest =
  RepoDigest "sha256:ce1398ea33a7afcbc577e417d1a499aeeba6b5be732a2336995916406c913833"

mcrRegistryImage :: Text
mcrRegistryImage = "mcr.microsoft.com/azure-cli:0.10.13"

mcrRegistryImageDigest :: RepoDigest
mcrRegistryImageDigest = RepoDigest "sha256:37b1e1b1484d540b96f8f30f4c4da24b704a110aa662f79b641f5ab375ee8094"

debian2gcrImage :: Text
debian2gcrImage = "gcr.io/google-containers/debian-base:v2.0.0"

debian2gcrImageDigest :: RepoDigest
debian2gcrImageDigest = RepoDigest "sha256:9bd6154724425e6083550fd85a91952fa2f79ef0b9844f0d009c37a72d075757"

redisImage :: Text
redisImage = "redis:6.0.14-buster"

redisImageDigest :: RepoDigest
redisImageDigest = RepoDigest "sha256:86813a7dd3971d9b8088bb34fd894d2cf18a48679d3d8d958a1a0bd4955e6cef"

haskellDevImage :: Text
haskellDevImage = "ghcr.io/fossas/haskell-dev-tools:9.0.2"

haskellDevImageDigest :: RepoDigest
haskellDevImageDigest = RepoDigest "sha256:06541fab718090e2d346e0313d3f1e841d1ea2b157630ad76ca20eb1507a4223"
