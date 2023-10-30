{-# LANGUAGE DataKinds #-}


module Analysis.ContainerScanningSpec (spec) where

import Test.Hspec ( it, shouldBe, Spec, describe )
import Control.Effect.ContainerRegistryApi
    ( ContainerRegistryApi, getImageManifest, ContainerRegistryApiF )
import Control.Effect.Diagnostics (Diagnostics)
import Control.Algebra (Has)
import Control.Carrier.Lift (Lift)
import Data.Text (Text)
import Container.Docker.SourceParser
    ( RepoDigest(..), RegistryImageSource, parseImageUrl )
import Control.Carrier.Diagnostics
    ( fromEitherShow, runDiagnostics, DiagnosticsC )
import Container.Docker.OciManifest (OciManifestV2(..), OciManifestConfig(..))
import Text.Megaparsec (ParseErrorBundle, parse)
import Data.Void (Void)
import Control.Carrier.Stack ( runStack, StackC )
import Control.Carrier.StickyLogger
    ( ignoreStickyLogger, IgnoreStickyLoggerC )
import Effect.Logger ( ignoreLogger, IgnoreLoggerC )
import Effect.Exec ( runExecIO, ExecIOC )
import Control.Carrier.Reader (runReader, ReaderC)
import Control.Carrier.ContainerRegistryApi (runContainerRegistryApi)
import Control.Carrier.Finally ( runFinally, FinallyC )
import Diag.Result ( Result(..), renderFailure )
import Effect.ReadFS ( runReadFSIO, ReadFSIOC )
import Control.Carrier.ContainerRegistryApi.Common (RegistryCtx)
import Discovery.Filters (AllFilters)
import Control.Carrier.Simple (SimpleC)
import Type.Operator (type ($))
import Control.Effect.Lift (sendIO)
import Data.String.Conversion ( toText )
import System.Environment (lookupEnv)

spec :: Spec
spec = testPrivateRepos

testPrivateRepos :: Spec
testPrivateRepos =
    describe "private registries" $ do
        it "dockerhub" $ do
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
runEff = runStack
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
