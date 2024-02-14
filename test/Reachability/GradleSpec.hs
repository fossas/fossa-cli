{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reachability.GradleSpec (spec) where

import App.Fossa.Reachability.Gradle (
  jarFileSignifier,
  jarPathsFromScriptOutput,
 )
import Data.String.Conversion (ConvertUtf8 (..), toText)
import Data.Text (Text)
import Path (Abs, File, Path, mkRelFile, (</>))
import Path.IO qualified as PIO
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, runIO)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Gradle" $ describe "jarPathsFromScriptOutput" $ do
  jarPath <- runIO sampleJar
  it' "should get zero paths from empty" $ do
    paths <- jarPathsFromScriptOutput . encodeUtf8 $ empty
    paths `shouldBe'` []

  it' "should get zero paths when missing jars" $ do
    paths <- jarPathsFromScriptOutput . encodeUtf8 $ noJars
    paths `shouldBe'` []

  it' "should not include path which do not exist" $ do
    paths <- jarPathsFromScriptOutput . encodeUtf8 $ validButMissingPath
    paths `shouldBe'` []

  it' "should get path from successful output" $ do
    paths <- jarPathsFromScriptOutput . encodeUtf8 . toValidEntry $ jarPath
    paths `shouldBe'` [jarPath]

empty :: Text
empty = [r||]

noJars :: Text
noJars =
  [r|
> Task :app:jarPaths
  project is project ':app'
  hasJavaPlugin is true
  runTask is task ':app:jar'

  BUILD SUCCESSFUL in 255ms
  1 actionable task: 1 executed
|]

validButMissingPath :: Text
validButMissingPath =
  [r|
  > Task :app:jarPaths
  project is project ':app'
  hasJavaPlugin is true
  runTask is task ':app:jar'
  JARFILE::/Users/dev/code/example-projects/reachability/java/vulnerable-function-used/app/build/libs/app.jar

  BUILD SUCCESSFUL in 255ms
  1 actionable task: 1 executed
|]

sampleJar :: IO (Path Abs File)
sampleJar = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/sample.jar"))

toValidEntry :: Path Abs File -> Text
toValidEntry p = jarFileSignifier <> toText p
