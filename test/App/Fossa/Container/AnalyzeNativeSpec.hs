{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Container.AnalyzeNativeSpec (spec) where

import App.Fossa.Container.Scan (
  ContainerImageSource (DockerEngine),
  parseDockerEngineSource,
 )
import App.Fossa.Container.Sources.DockerArchive (analyzeFromDockerArchive)
import Container.Types (
  ContainerScan (imageData),
  ContainerScanImage (imageLayers),
  srcUnits,
 )
import Control.Carrier.Debug (IgnoreDebugC, ignoreDebug)
import Control.Carrier.Diagnostics (DiagnosticsC, Has)
import Control.Carrier.Stack (StackC, runStack)
import Control.Carrier.Telemetry (IgnoreTelemetryC, withoutTelemetry)
import Control.Effect.DockerEngineApi (
  DockerEngineApiF (GetImageSize, IsDockerEngineAccessible),
 )
import Data.Maybe (mapMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Discovery.Filters (AllFilters (AllFilters), comboExclude)
import Effect.Exec (ExecIOC, runExecIO)
import Effect.Logger (IgnoreLoggerC, ignoreLogger)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Path (Dir, File, Rel, mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Path.Internal (Path)
import Srclib.Types (
  Locator (Locator),
  SourceUnit (sourceUnitBuild),
  SourceUnitBuild (buildImports),
 )
import Test.Effect (
  expectFatal',
  handleDiag,
  shouldBe',
  shouldBeSupersetOf',
  shouldNotContain',
 )
import Test.Hspec (Spec, SpecWith, describe, it, runIO)
import Test.MockDockerEngineApi (
  DockerEngineApiMockC,
  MockApi,
  MockApiC,
  alwaysReturns,
  fails,
  runApiWithMock,
  runMockApi,
 )
import Type.Operator (type ($))
import Types (
  DiscoveredProjectType (SetuptoolsProjectType),
  TargetFilter (TypeDirTarget, TypeTarget),
 )
import Data.Flag (toFlag')

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

  describe "analyze" $ do
    currDir <- runIO getCurrentDir
    let imageArchive = currDir </> appDepsImage

    it' "should not analyze application dependencies when only-system-dependencies are requested" $ do
      containerScan <- analyzeFromDockerArchive True mempty (toFlag' False) imageArchive
      buildImportsOf containerScan `shouldNotContain'` [numpy, scipy, black]

    it' "should analyze application dependencies" $ do
      containerScan <- analyzeFromDockerArchive False mempty (toFlag' False) imageArchive
      buildImportsOf containerScan `shouldBeSupersetOf'` [numpy, scipy, black]
      buildImportsOf containerScan `shouldNotContain'` [networkX]

    it' "should apply tool exclusion filter" $ do
      containerScan <- analyzeFromDockerArchive False (excludeTool SetuptoolsProjectType) (toFlag' False) imageArchive
      buildImportsOf containerScan `shouldNotContain'` [numpy, scipy, black]

    it' "should apply project exclusion filter" $ do
      let appAPath = $(mkRelDir "app/services/b/")
      containerScan <- analyzeFromDockerArchive False (excludeProject SetuptoolsProjectType appAPath) (toFlag' False) imageArchive
      buildImportsOf containerScan `shouldNotContain'` [scipy]
      buildImportsOf containerScan `shouldBeSupersetOf'` [numpy, black]

    it' "should analyze all targets, if default filter is disabled" $ do
      containerScan <- analyzeFromDockerArchive False mempty (toFlag' True) imageArchive
      buildImportsOf containerScan `shouldBeSupersetOf'` [numpy, scipy, black, networkX]

    it' "should apply path exclusion filter" $ do
      let appAPath = $(mkRelDir "app/services/b/internal/")
      containerScan <- analyzeFromDockerArchive False (excludePath appAPath) (toFlag' False) imageArchive
      buildImportsOf containerScan `shouldNotContain'` [black]
      buildImportsOf containerScan `shouldBeSupersetOf'` [numpy, scipy]

buildImportsOf :: ContainerScan -> [Locator]
buildImportsOf scan =
  concatMap buildImports $
    concatMap (mapMaybe sourceUnitBuild . srcUnits) (imageLayers . imageData $ scan)

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

type EffStack = DockerEngineApiMockC $ ExecIOC $ ReadFSIOC $ IgnoreDebugC $ DiagnosticsC $ MockApiC $ IgnoreLoggerC $ StackC $ IgnoreTelemetryC IO

it' :: String -> EffStack () -> SpecWith ()
it' msg = it msg . runWithMockDockerEngineEff

runWithMockDockerEngineEff :: EffStack () -> IO ()
runWithMockDockerEngineEff =
  withoutTelemetry
    . runStack
    . ignoreLogger
    . runMockApi
    . handleDiag
    . ignoreDebug
    . runReadFSIO
    . runExecIO
    . runApiWithMock

appDepsImage :: Path Rel File
appDepsImage = $(mkRelFile "test/Container/testdata/app_deps_example.tar")

-- | From "app/services/a/" project.
numpy :: Locator
numpy = Locator "pip" "numpy" Nothing

-- | From "app/services/b/" project.
scipy :: Locator
scipy = Locator "pip" "scipy" Nothing

-- | From "app/services/b/internal/" project.
black :: Locator
black = Locator "pip" "black" Nothing

-- | From "app/services/b/internal/test/" project.
networkX :: Locator
networkX = Locator "pip" "networkx" Nothing

excludePath :: Path Rel Dir -> AllFilters
excludePath path = AllFilters mempty $ comboExclude mempty [path]

excludeTool :: DiscoveredProjectType -> AllFilters
excludeTool tool = AllFilters mempty $ comboExclude [TypeTarget $ toText tool] mempty

excludeProject :: DiscoveredProjectType -> Path Rel Dir -> AllFilters
excludeProject ty path = AllFilters mempty $ comboExclude [TypeDirTarget (toText ty) path] mempty
