module Android.UtilSpec (
  spec,
) where

import Data.Foldable (for_)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Strategy.Android.Util (isDefaultAndroidDevConfig, isDefaultAndroidTestConfig)
import Test.Hspec (Spec, describe, it, shouldBe)

defaultAndroidDevConfigs :: [Text]
defaultAndroidDevConfigs =
  [ "lintClassPath"
  , "debugRuntimeClasspath"
  ]

defaultAndroidTestConfigs :: [Text]
defaultAndroidTestConfigs =
  [ "testApiDependenciesMetadata"
  , "testReleaseApiDependenciesMetadata"
  , "testReleaseCompileOnlyDependenciesMetadata"
  , "androidTestApiDependenciesMetadata"
  , "debugAndroidTestImplementationDependenciesMetadata"
  , "releaseUnitTestImplementationDependenciesMetadata"
  , "debugUnitTestAnnotationProcessorClasspath"
  , "debugUnitTestCompileClasspath"
  , "debugUnitTestRuntimeClasspath"
  ]

defaultNonDevAndTestConfigs :: [Text]
defaultNonDevAndTestConfigs =
  [ "default"
  , "androidApis"
  , "implementation"
  , "coreLibraryDesugaring"
  , "releaseRuntimeOnlyDependenciesMetadata"
  , "archives"
  ]

spec :: Spec
spec = do
  describe "isDefaultAndroidDevConfig" $ do
    for_ defaultAndroidDevConfigs $ \candidate ->
      it ("should return true when provided with: " <> toString candidate) $
        isDefaultAndroidDevConfig candidate `shouldBe` True

    for_ defaultNonDevAndTestConfigs $ \candidate ->
      it ("should return false when provided with: " <> toString candidate) $
        isDefaultAndroidDevConfig candidate `shouldBe` False

  describe "isDefaultAndroidTestConfig" $ do
    for_ defaultAndroidTestConfigs $ \candidate ->
      it ("should return true when provided with: " <> toString candidate) $
        isDefaultAndroidTestConfig candidate `shouldBe` True

    for_ defaultNonDevAndTestConfigs $ \candidate ->
      it ("should return false when provided with: " <> toString candidate) $
        isDefaultAndroidTestConfig candidate `shouldBe` False
