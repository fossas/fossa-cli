{-# LANGUAGE DataKinds #-}

module Analysis.ContainerScanningSpec (spec) where

import Container.Docker.OciManifest (OciManifestConfig (..), OciManifestV2 (..))
import Container.Docker.SourceParser (
  RegistryImageSource,
  RepoDigest (..),
  parseImageUrl,
 )
import Control.Algebra (Has)
import Control.Carrier.ContainerRegistryApi (runContainerRegistryApi)
import Control.Carrier.ContainerRegistryApi.Common (RegistryCtx)
import Control.Carrier.Diagnostics (
  DiagnosticsC,
  fromEitherShow,
  runDiagnostics,
 )
import Control.Carrier.Finally (FinallyC, runFinally)
import Control.Carrier.Lift (Lift)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC)
import Control.Carrier.Stack (StackC, runStack)
import Control.Carrier.StickyLogger (
  IgnoreStickyLoggerC,
  ignoreStickyLogger,
 )
import Control.Effect.ContainerRegistryApi (
  ContainerRegistryApi,
  ContainerRegistryApiF,
  getImageManifest,
 )
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (sendIO)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Diag.Result (Result (..), renderFailure)
import Discovery.Filters (AllFilters)
import Effect.Exec (ExecIOC, runExecIO)
import Effect.Logger (IgnoreLoggerC, ignoreLogger)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (ParseErrorBundle, parse)
import Type.Operator (type ($))

spec :: Spec
spec = testPrivateRepos

-- Integeration tests to catch, if registry api implementation changes
-- abruptly.
testPrivateRepos :: Spec
testPrivateRepos =
  describe "private registries" $ do
    it "dockerhub" $ do
      -- Refer to 1Password for creds or Github Action Env Keys
      img <- withAuth "FOSSA_DOCKERHUB_WWW_STYLE_USER_PASS" dockerHubImage
      res <- runEff $ getImageConfig "amd64" img
      case res of
        Failure ws eg -> fail (show (renderFailure ws eg "An issue occurred"))
        Success _ res' -> res' `shouldBe` dockerHubImageConfigDigest

dockerHubImage :: Text
dockerHubImage = "index.docker.io/fossabot/container-test-fixture:0"

dockerHubImageConfigDigest :: RepoDigest
dockerHubImageConfigDigest = RepoDigest "sha256:792d29ec0ccee20718b0233b1ca0c633a57009bbb4d99c247b0ec1e3f562b19b"

--

type EffectStack m =
  FinallyC
    $ SimpleC ContainerRegistryApiF
    $ ReaderC RegistryCtx
    $ ReaderC AllFilters
    $ ExecIOC
    $ ReadFSIOC
    $ DiagnosticsC
    $ IgnoreLoggerC
    $ IgnoreStickyLoggerC
    $ StackC IO

runEff :: EffectStack IO a -> IO (Result a)
runEff =
  runStack
    . ignoreStickyLogger
    . ignoreLogger
    . runDiagnostics
    . runReadFSIO
    . runExecIO
    . runReader mempty
    . runContainerRegistryApi
    . runFinally

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
  configDigest . ociConfig
    <$> (getImageManifest =<< fromEitherShow (decodeStrict arch img))

withAuth :: Has (Lift IO) sig m => String -> Text -> m Text
withAuth authEnvKey target = do
  auth <- sendIO $ lookupEnv authEnvKey
  case auth of
    Nothing -> pure target
    Just auth' -> pure $ (toText auth') <> "@" <> target
