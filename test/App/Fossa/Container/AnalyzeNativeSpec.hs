module App.Fossa.Container.AnalyzeNativeSpec (spec) where

import App.Fossa.Container.AnalyzeNative (ContainerImageSource (DockerEngine), parseDockerEngineSource)
import Control.Carrier.Diagnostics (DiagnosticsC, Has)
import Control.Carrier.Stack (StackC, runStack)
import Control.Effect.DockerEngineApi (DockerEngineApiF (GetImageSize, IsDockerEngineAccessible))
import Data.Text (Text)
import Effect.Logger (IgnoreLoggerC, ignoreLogger)
import Test.Effect (expectFatal', handleDiag, shouldBe')
import Test.Hspec (Spec, SpecWith, describe, it)
import Test.MockDockerEngineApi (DockerEngineApiMockC, MockApi, MockApiC, alwaysReturns, fails, runApiWithMock, runMockApi)
import Type.Operator (type ($))

spec :: Spec
spec = do
  describe "parseDockerEngineSource" $ do
    it' "should fail if docker engine is not accessible" $ do
      expectDockerSdkNotAccessible
      expectFatal' $ parseDockerEngineSource exampleImgWithTag

    it' "should fail if text lacks tag signifier " $ do
      expectDockerSdkToBeAccessible
      expectFatal' $ parseDockerEngineSource exampleImgWithoutTag

    it' "should fail if image's size cannot be retrieved" $ do
      expectDockerSdkToBeAccessible
      expectGetImageSizeToFail
      expectFatal' $ parseDockerEngineSource exampleImgWithTag

    it' "should parse image source if image size can be inferred via docker api" $ do
      expectDockerSdkToBeAccessible
      expectGetImageSizeToSucceed
      src <- parseDockerEngineSource exampleImgWithTag
      src `shouldBe'` DockerEngine exampleImgWithTag

exampleImgWithoutTag :: Text
exampleImgWithoutTag = "redis"

exampleImgWithTag :: Text
exampleImgWithTag = "redis:alpine"

expectDockerSdkNotAccessible :: Has MockApi sig m => m ()
expectDockerSdkNotAccessible =
  IsDockerEngineAccessible `alwaysReturns` False

expectDockerSdkToBeAccessible :: Has MockApi sig m => m ()
expectDockerSdkToBeAccessible =
  IsDockerEngineAccessible `alwaysReturns` True

expectGetImageSizeToFail :: Has MockApi sig m => m ()
expectGetImageSizeToFail = fails (GetImageSize exampleImgWithTag) "fails"

expectGetImageSizeToSucceed :: Has MockApi sig m => m ()
expectGetImageSizeToSucceed = (GetImageSize exampleImgWithTag) `alwaysReturns` 100

type EffStack = DockerEngineApiMockC $ DiagnosticsC $ MockApiC $ IgnoreLoggerC $ StackC IO

it' :: String -> EffStack () -> SpecWith ()
it' msg = it msg . runWithMockDockerEngineEff

runWithMockDockerEngineEff :: EffStack () -> IO ()
runWithMockDockerEngineEff =
  runStack
    . ignoreLogger
    . runMockApi
    . handleDiag
    . runApiWithMock
