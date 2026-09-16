{-# LANGUAGE CPP #-}

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
import Control.Exception (throw)
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Path (Abs, Dir, File, Path, parseAbsDir, parseAbsFile, toFilePath)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | The fixtures below are valid paths only on the platform they are written
-- for; a parse failure means the fixture itself is broken, so fail with the
-- parse error rather than carrying it on to an assertion.
mustParse :: (Show e) => (String -> Either e p) -> String -> p
mustParse f s = either (throw . userError . show) id (f s)

targetDir :: Path Abs Dir
workDir :: Path Abs Dir
jsBundle :: Path Abs File
mjsBundle :: Path Abs File
cjsBundle :: Path Abs File
upperJsBundle :: Path Abs File
nativeAnalyzer :: Path Abs File
#ifdef mingw32_HOST_OS
targetDir = mustParse parseAbsDir "C:/repo"
workDir = mustParse parseAbsDir "C:/scratch"
jsBundle = mustParse parseAbsFile "C:/dist/analyzer.js"
mjsBundle = mustParse parseAbsFile "C:/dist/analyzer.mjs"
cjsBundle = mustParse parseAbsFile "C:/dist/analyzer.cjs"
upperJsBundle = mustParse parseAbsFile "C:/dist/analyzer.JS"
nativeAnalyzer = mustParse parseAbsFile "C:/bin/analyzer"
#else
targetDir = mustParse parseAbsDir "/abs/repo"
workDir = mustParse parseAbsDir "/abs/scratch"
jsBundle = mustParse parseAbsFile "/abs/dist/analyzer.js"
mjsBundle = mustParse parseAbsFile "/abs/dist/analyzer.mjs"
cjsBundle = mustParse parseAbsFile "/abs/dist/analyzer.cjs"
upperJsBundle = mustParse parseAbsFile "/abs/dist/analyzer.JS"
nativeAnalyzer = mustParse parseAbsFile "/usr/local/bin/analyzer"
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
        `shouldBe` Just (Right (WorkflowStarted "/usr/bin/node" "unknown"))

    it "decodes step-completed" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"step-completed\",\"step\":\"module-discovery\"}")
        `shouldBe` Just (Right (WorkflowStepCompleted "module-discovery"))

    it "decodes workflow-result" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"workflow-result\",\"result\":{\"schemaVersion\":1,\"packages\":[]}}")
        `shouldBe` Just (Right (WorkflowResult (Aeson.object ["schemaVersion" Aeson..= (1 :: Int), "packages" Aeson..= ([] :: [Aeson.Value])])))

    it "decodes workflow-failed carrying an exitCode" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"workflow-failed\",\"reason\":\"workflow executable exited with code 3\",\"exitCode\":3,\"stderrTail\":\"boom\"}")
        `shouldBe` Just (Right (WorkflowFailed "workflow executable exited with code 3" "boom"))

    it "decodes workflow-failed carrying a timeout" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"workflow-failed\",\"reason\":\"workflow executable ran longer than 900 seconds\",\"timeout\":\"total\",\"stderrTail\":\"boom\"}")
        `shouldBe` Just (Right (WorkflowFailed "workflow executable ran longer than 900 seconds" "boom"))

    it "ignores findings from another strategy" $
      findingToWorkflowEvent (FicusFinding (FicusMessageData "vendetta" "{\"type\":\"step-completed\",\"step\":\"module-discovery\"}"))
        `shouldBe` Nothing

    it "keeps the raw payload of a workflow finding it cannot decode" $
      findingToWorkflowEvent (payloadFinding "{\"type\":\"workflow-adjourned\"}")
        `shouldBe` Just (Left "{\"type\":\"workflow-adjourned\"}")

    it "rejects an unrecognised event type rather than decoding it" $
      (Aeson.eitherDecodeStrictText "{\"type\":\"workflow-adjourned\"}" :: Either String WorkflowEvent)
        `shouldSatisfy` isLeft
