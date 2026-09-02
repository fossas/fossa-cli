{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Ficus.WorkflowSpec (spec) where

import App.Fossa.Ficus.Types (
  FicusFinding (FicusFinding),
  FicusMessageData (FicusMessageData),
  WorkflowEvent (..),
  WorkflowExecutable (WorkflowExecutable),
  WorkflowRunArtifact (WorkflowRunArtifact),
  findingToWorkflowEvent,
  toWorkflowExecutable,
 )
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Path (Abs, Dir, File, Path, mkAbsDir, mkAbsFile, toFilePath)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

targetDir :: Path Abs Dir
workDir :: Path Abs Dir
jsBundle :: Path Abs File
mjsBundle :: Path Abs File
cjsBundle :: Path Abs File
upperJsBundle :: Path Abs File
nativeAnalyzer :: Path Abs File
#ifdef mingw32_HOST_OS
targetDir = $(mkAbsDir "C:/repo")
workDir = $(mkAbsDir "C:/scratch")
jsBundle = $(mkAbsFile "C:/dist/analyzer.js")
mjsBundle = $(mkAbsFile "C:/dist/analyzer.mjs")
cjsBundle = $(mkAbsFile "C:/dist/analyzer.cjs")
upperJsBundle = $(mkAbsFile "C:/dist/analyzer.JS")
nativeAnalyzer = $(mkAbsFile "C:/bin/analyzer")
#else
targetDir = $(mkAbsDir "/abs/repo")
workDir = $(mkAbsDir "/abs/scratch")
jsBundle = $(mkAbsFile "/abs/dist/analyzer.js")
mjsBundle = $(mkAbsFile "/abs/dist/analyzer.mjs")
cjsBundle = $(mkAbsFile "/abs/dist/analyzer.cjs")
upperJsBundle = $(mkAbsFile "/abs/dist/analyzer.JS")
nativeAnalyzer = $(mkAbsFile "/usr/local/bin/analyzer")
#endif

-- | The wire contract with @ficus x-workflow@. Every key name, the schema
-- version and the inferred program are literal here; only the paths are
-- rendered, so the same expectation holds on Windows.
expectedArtifactJson :: Text
expectedArtifactJson =
  Text.concat
    [ "{\"version\":1"
    , ",\"executable\":{\"program\":\"node\",\"args\":["
    , jsonString jsBundle
    , "]}"
    , ",\"target\":"
    , jsonString targetDir
    , ",\"workingDirectory\":"
    , jsonString workDir
    , "}"
    ]
  where
    jsonString :: Path Abs t -> Text
    jsonString = decodeUtf8 . Aeson.encode . toFilePath

payloadFinding :: Text -> FicusFinding
payloadFinding = FicusFinding . FicusMessageData "workflow"

spec :: Spec
spec = do
  describe "run artifact encoding" $ do
    it "matches the JSON ficus parses" $ do
      let artifact = WorkflowRunArtifact (toWorkflowExecutable jsBundle) targetDir workDir
      Just (Aeson.toJSON artifact) `shouldBe` Aeson.decodeStrictText expectedArtifactJson

  describe "program inference" $ do
    it "runs a .js bundle under node" $
      toWorkflowExecutable jsBundle `shouldBe` WorkflowExecutable "node" [toText $ toFilePath jsBundle]

    it "runs a .mjs bundle under node" $
      toWorkflowExecutable mjsBundle `shouldBe` WorkflowExecutable "node" [toText $ toFilePath mjsBundle]

    it "runs a .cjs bundle under node" $
      toWorkflowExecutable cjsBundle `shouldBe` WorkflowExecutable "node" [toText $ toFilePath cjsBundle]

    it "runs an uppercase .JS bundle under node" $
      toWorkflowExecutable upperJsBundle `shouldBe` WorkflowExecutable "node" [toText $ toFilePath upperJsBundle]

    it "passes any other path through as the program itself" $
      toWorkflowExecutable nativeAnalyzer `shouldBe` WorkflowExecutable (toText $ toFilePath nativeAnalyzer) []

  describe "workflow event decoding" $ do
    it "decodes workflow-started" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"workflow-started\",\"ficusVersion\":\"1.2.3\",\"executable\":{\"program\":\"node\",\"args\":[]},\"resolvedProgram\":\"/usr/bin/node\",\"analyzerVersion\":\"unknown\"}")
        `shouldBe` Just (WorkflowStarted "/usr/bin/node" "unknown")

    it "decodes step-completed" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"step-completed\",\"step\":\"module-discovery\"}")
        `shouldBe` Just (WorkflowStepCompleted "module-discovery")

    it "decodes workflow-result" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"workflow-result\",\"result\":{\"schemaVersion\":1,\"packages\":[]}}")
        `shouldBe` Just (WorkflowResult (Aeson.object ["schemaVersion" Aeson..= (1 :: Int), "packages" Aeson..= ([] :: [Aeson.Value])]))

    it "decodes workflow-failed carrying an exitCode" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"workflow-failed\",\"reason\":\"workflow executable exited with code 3\",\"exitCode\":3,\"stderrTail\":\"boom\"}")
        `shouldBe` Just (WorkflowFailed "workflow executable exited with code 3" "boom")

    it "decodes workflow-failed carrying a timeout" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"workflow-failed\",\"reason\":\"workflow executable ran longer than 900 seconds\",\"timeout\":\"total\",\"stderrTail\":\"boom\"}")
        `shouldBe` Just (WorkflowFailed "workflow executable ran longer than 900 seconds" "boom")

    it "ignores findings from another strategy" $
      findingToWorkflowEvent (FicusFinding (FicusMessageData "vendetta" "{\"type\":\"step-completed\",\"step\":\"module-discovery\"}"))
        `shouldBe` Nothing

    it "rejects an unrecognised event type rather than decoding it" $
      (Aeson.eitherDecodeStrictText "{\"type\":\"workflow-adjourned\"}" :: Either String WorkflowEvent)
        `shouldSatisfy` isLeft
