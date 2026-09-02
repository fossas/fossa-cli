{-# LANGUAGE CPP #-}

module Ficus.WorkflowProcessSpec (spec) where

#ifdef mingw32_HOST_OS
import Test.Hspec (Spec)

-- The fake ficus is a @/bin/sh@ script, which the Windows CI runner does not have.
spec :: Spec
spec = pure ()
#else
import App.Fossa.Ficus.Analyze (execFicusStreaming)
import App.Fossa.Ficus.Types (
  FicusMessage (FicusMessageFinding),
  WorkflowEvent (WorkflowResult, WorkflowStepCompleted),
  WorkflowRunArtifact (WorkflowRunArtifact),
  findingToWorkflowEvent,
  toWorkflowExecutable,
  workflowResultJson,
 )
import App.Fossa.Ficus.Workflow (runWorkflowWith)
import Control.Carrier.Debug (Scope (scopeMetadata), runDebug)
import Control.Effect.Exception (SomeException, try)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Exception (throw, throwIO)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft, rights)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Exec (AllowErr (Never), Command (..), ExitCode (ExitFailure, ExitSuccess))
import Path (Abs, Dir, File, Path, parseAbsDir, parseAbsFile, parseRelFile, toFilePath, (</>))
import System.Directory (getPermissions, setOwnerExecutable, setPermissions)
import Test.Effect (expectFatal', itWithTempDir', shouldBe', shouldSatisfy')
import Test.Hspec (Spec, describe)

-- | The fixtures below are valid paths on the platform the tests run on; a
-- parse failure means the fixture itself is broken, so fail with the parse
-- error rather than carrying it on to an assertion.
mustParse :: (Show e) => (String -> Either e p) -> String -> p
mustParse f s = either (throw . userError . show) id (f s)

spec :: Spec
spec = do
  streamingSpec
  workflowSpec

-- | ficus's own wire-contract assertion, rebuilt here so the fake speaks exactly
-- what fossa-cli sees in production (@ficus/tests/it/workflow.rs:308-318@).
observationEnvelope :: Text -> Text
observationEnvelope payload =
  decodeUtf8 . Aeson.encode $
    Aeson.object
      [ "version" Aeson..= (1 :: Int)
      , "level" Aeson..= ("INFO" :: Text)
      , "observation"
          Aeson..= Aeson.object
            [ "kind" Aeson..= ("finding" :: Text)
            , "payload" Aeson..= payload
            , "strategy" Aeson..= ("workflow" :: Text)
            ]
      ]

workflowStartedPayload :: Text
workflowStartedPayload =
  decodeUtf8 . Aeson.encode $
    Aeson.object
      [ "type" Aeson..= ("workflow-started" :: Text)
      , "ficusVersion" Aeson..= ("0.0.0" :: Text)
      , "executable" Aeson..= Aeson.object ["program" Aeson..= ("node" :: Text), "args" Aeson..= ([] :: [Text])]
      , "resolvedProgram" Aeson..= ("/usr/bin/node" :: Text)
      , "analyzerVersion" Aeson..= ("unknown" :: Text)
      ]

stepCompletedPayload :: Text
stepCompletedPayload =
  decodeUtf8 . Aeson.encode $
    Aeson.object ["type" Aeson..= ("step-completed" :: Text), "step" Aeson..= ("module-discovery" :: Text)]

workflowResultPayload :: Text
workflowResultPayload =
  decodeUtf8 . Aeson.encode $
    Aeson.object
      [ "type" Aeson..= ("workflow-result" :: Text)
      , "result" Aeson..= expectedResult
      ]

expectedResult :: Aeson.Value
expectedResult = Aeson.object ["schemaVersion" Aeson..= (1 :: Int)]

happyPayloads :: [Text]
happyPayloads = [stepCompletedPayload, workflowResultPayload]

-- | A fake ficus: emits the given observations, copies its stdin to stderr so a
-- test can prove the run artifact arrived there, and exits with the given code.
fakeFicus :: [Text] -> Int -> Text
fakeFicus payloads exitCode =
  Text.unlines $
    ["#!/bin/sh"]
      <> map (\payload -> "printf '%s\\n' '" <> observationEnvelope payload <> "'") payloads
      <> ["cat >&2", "exit " <> toText (show exitCode)]

writeFakeFicus :: (Has (Lift IO) sig m) => Path Abs Dir -> [Text] -> Int -> m Command
writeFakeFicus dir payloads exitCode = do
  let script = dir </> mustParse parseRelFile "fake-ficus.sh"
  sendIO $ do
    writeFile (toFilePath script) (toString $ fakeFicus payloads exitCode)
    permissions <- getPermissions (toFilePath script)
    setPermissions (toFilePath script) (setOwnerExecutable True permissions)
  pure
    Command
      { cmdName = toText $ toFilePath script
      , cmdArgs = []
      , cmdAllowErr = Never
      , cmdEnvVars = Map.empty
      }

runArtifactBytes :: ByteString
runArtifactBytes =
  BL.toStrict . Aeson.encode $
    WorkflowRunArtifact
      (toWorkflowExecutable (mustParse parseAbsFile "/abs/dist/analyzer.js"))
      (mustParse parseAbsDir "/abs/repo")
      (mustParse parseAbsDir "/abs/scratch")

collectMessages :: [FicusMessage] -> FicusMessage -> IO [FicusMessage]
collectMessages acc message = pure (acc <> [message])

-- | Fails the stream mid-run, after the first observation has been teed to the
-- debug log but before the run can end normally.
explodingStep :: [FicusMessage] -> FicusMessage -> IO [FicusMessage]
explodingStep _ _ = throwIO $ userError "stream processing exploded"

decodedEvents :: [FicusMessage] -> [WorkflowEvent]
decodedEvents = rights . mapMaybe toEvent
  where
    toEvent (FicusMessageFinding finding) = findingToWorkflowEvent finding
    toEvent _ = Nothing

analyzerBundle :: Path Abs Dir -> Path Abs File
analyzerBundle dir = dir </> mustParse parseRelFile "analyzer.js"

streamingSpec :: Spec
streamingSpec = describe "execFicusStreaming" $ do
  itWithTempDir' "streams observations, delivers the artifact on stdin, and reports success" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir happyPayloads 0
    (messages, exitCode, stdErrLines) <-
      execFicusStreaming tmpDir cmd (Just runArtifactBytes) Nothing "fossa.ficus-workflow" collectMessages []
    decodedEvents messages
      `shouldBe'` [WorkflowStepCompleted "module-discovery", WorkflowResult expectedResult]
    stdErrLines `shouldSatisfy'` any (Text.isInfixOf (decodeUtf8 runArtifactBytes))
    exitCode `shouldBe'` ExitSuccess

  itWithTempDir' "reports a non-zero exit rather than swallowing it" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir happyPayloads 3
    (_, exitCode, _) <-
      execFicusStreaming tmpDir cmd (Just runArtifactBytes) Nothing "fossa.ficus-workflow" collectMessages ([] :: [FicusMessage])
    exitCode `shouldBe'` ExitFailure 3

  itWithTempDir' "writes debug logs under the basename it was given" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir happyPayloads 0
    _ <-
      execFicusStreaming tmpDir cmd (Just runArtifactBytes) (Just $ toFilePath tmpDir) "fossa.ficus-workflow" collectMessages ([] :: [FicusMessage])
    stdoutLog <- sendIO . readFile . toFilePath $ tmpDir </> mustParse parseRelFile "fossa.ficus-workflow-stdout.log"
    stderrLog <- sendIO . readFile . toFilePath $ tmpDir </> mustParse parseRelFile "fossa.ficus-workflow-stderr.log"
    toText stdoutLog `shouldSatisfy'` Text.isInfixOf (observationEnvelope stepCompletedPayload)
    toText stderrLog `shouldSatisfy'` Text.isInfixOf (decodeUtf8 runArtifactBytes)

  itWithTempDir' "closes the debug logs when stream processing throws" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir happyPayloads 0
    outcome <-
      try $
        execFicusStreaming tmpDir cmd (Just runArtifactBytes) (Just $ toFilePath tmpDir) "fossa.ficus-throwing" explodingStep ([] :: [FicusMessage])
    (outcome :: Either SomeException ([FicusMessage], ExitCode, [Text])) `shouldSatisfy'` isLeft
    stdoutLog <- sendIO . readFile . toFilePath $ tmpDir </> mustParse parseRelFile "fossa.ficus-throwing-stdout.log"
    toText stdoutLog `shouldSatisfy'` Text.isInfixOf (observationEnvelope stepCompletedPayload)

workflowSpec :: Spec
workflowSpec = describe "analyzeWithWorkflow" $ do
  itWithTempDir' "fails when ficus exits non-zero" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir [stepCompletedPayload] 1
    expectFatal' $ runWorkflowWith cmd tmpDir (analyzerBundle tmpDir) Nothing

  itWithTempDir' "fails when ficus exits cleanly without a result" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir [stepCompletedPayload] 0
    expectFatal' $ runWorkflowWith cmd tmpDir (analyzerBundle tmpDir) Nothing

  itWithTempDir' "returns the result and records it in the debug bundle on success" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir [workflowStartedPayload, stepCompletedPayload, workflowResultPayload] 0
    (scope, result) <- runDebug $ runWorkflowWith cmd tmpDir (analyzerBundle tmpDir) Nothing
    result `shouldBe'` expectedResult
    Map.lookup workflowResultJson (scopeMetadata scope) `shouldBe'` Just expectedResult
#endif
